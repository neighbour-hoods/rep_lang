use pretty::RcDoc;
use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    fmt, iter,
};

use rep_lang_concrete_syntax::{sp, util::pretty::parens};
use rep_lang_core::{
    abstract_syntax::{primop_arity, Defn, Expr, Lit, Name, PrimOp, Program},
    app, lam,
    util::calculate_hash,
};

use StoCell::*;
use Thunk::*;
use Value::*;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct VRef(usize);

#[derive(Clone, Debug, Eq, Hash)]
pub enum Value<R> {
    VInt(i64),
    VBool(bool),
    VClosure(Name, Box<Expr>, TermEnv),
    VCons(R, R),
    VNil,
    VPair(R, R),
}

pub enum Thunk<R> {
    UnevExpr(Expr, TermEnv),
    UnevRust(Box<dyn FnMut() -> FlatThunk>),
    Ev(Value<R>),
}

#[derive(Debug, PartialEq)]
pub struct FlatValue(pub Value<Box<FlatValue>>);

#[derive(Debug, PartialEq)]
pub struct FlatThunk(pub Thunk<Box<FlatThunk>>);

pub fn inject_flatvalue_to_flatthunk(val: FlatValue) -> FlatThunk {
    todo!()
}

// FlatThunk Ev
#[macro_export]
macro_rules! fte {
    ( $a: expr ) => {
        FlatThunk(Thunk::Ev($a))
    };
}

#[macro_export]
macro_rules! vcons {
    ( $a: expr, $b: expr ) => {
        FlatThunk(Thunk::Ev(Value::VCons(Box::new($a), Box::new($b))))
    };
}

impl<R: PartialEq> PartialEq for Value<R> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VInt(i1), VInt(i2)) => i1 == i2,
            (VBool(b1), VBool(b2)) => b1 == b2,
            (VCons(x1, l1), VCons(x2, l2)) => x1 == x2 && l1 == l2,
            (VNil, VNil) => true,
            (VPair(x1, y1), VPair(x2, y2)) => x1 == x2 && y1 == y2,
            (_, _) => false,
        }
    }
}

impl<R: PartialEq> PartialEq for Thunk<R> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ev(x1), Ev(x2)) => x1 == x2,
            _ => false,
        }
    }
}

impl<R: fmt::Debug> fmt::Debug for Thunk<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ev(val) => f.debug_struct("Ev").field("_", val).finish(),
            UnevExpr(expr, env) => f
                .debug_struct("UnevExpr")
                .field("expr", &expr)
                .field("env", &env)
                .finish(),
            UnevRust(_) => f.write_str("UnevRust <<closure>>"),
        }
    }
}

type TermEnv = BTreeMap<Name, VRef>;

pub fn new_term_env() -> TermEnv {
    BTreeMap::new()
}

#[derive(Debug)]
pub enum StoCell {
    CellThunk(Thunk<VRef>),
    CellRedirect(VRef),
}

#[derive(Debug)]
pub struct Sto {
    // "heap" of addressed thunks
    pub sto_vec: Vec<StoCell>,

    // map from evaluated `Value` to index in `sto_vec`.
    //
    // keys here are hashes of `Value<VRef>`.
    // not storing the full value saves space.
    pub sto_ev_map: HashMap<u64, usize>,

    // map from unevaluated expressions (and their requisite environment) to
    // index in `sto_vec`.
    //
    // keys here are hashes of `(Expr, TermEnv)`.
    // not storing the full values saves space.
    pub sto_unev_map: HashMap<u64, usize>,
}

impl Sto {
    pub fn new() -> Sto {
        Sto {
            sto_vec: Vec::new(),
            sto_ev_map: HashMap::new(),
            sto_unev_map: HashMap::new(),
        }
    }
}

impl Default for Sto {
    fn default() -> Self {
        Self::new()
    }
}

// TODO this likely performs significant amounts of cloning. however, it's
// possible this is approximately the minimal amount of cloning possible.
pub fn lookup_sto<'a>(es: &mut EvalState, vr: &VRef, sto: &'a mut Sto) -> Value<VRef> {
    let VRef(idx) = *vr;
    match sto.sto_vec.get_mut(idx) {
        None => panic!("lookup_sto: out of bounds"),
        Some(CellRedirect(vr2)) => lookup_sto(es, &vr2.clone(), sto),
        Some(CellThunk(thnk)) => match thnk {
            Ev(val) => val.clone(),
            UnevExpr(expr, env) => {
                let expr2 = expr.clone();
                let env2 = env.clone();
                let vr2 = eval_(&env2, sto, es, &expr2);
                let val = lookup_sto(es, &vr2, sto);
                sto.sto_vec[idx] = CellRedirect(vr2);
                val
            }
            UnevRust(clo) => {
                let flat_thunk = clo();
                let vr2 = flat_thunk_to_sto_ref(es, sto, flat_thunk);
                let val = lookup_sto(es, &vr2, sto);
                sto.sto_vec[idx] = CellRedirect(vr2);
                val
            }
        },
    }
}

pub fn add_to_sto(thnk: Thunk<VRef>, sto: &mut Sto) -> VRef {
    match thnk {
        Ev(ref val) => {
            let hash = calculate_hash(&val);
            match sto.sto_ev_map.get(&hash) {
                Some(idx) => VRef(*idx),
                None => {
                    let idx = sto.sto_vec.len();
                    sto.sto_vec.push(CellThunk(thnk));
                    sto.sto_ev_map.insert(hash, idx);
                    VRef(idx)
                }
            }
        }
        UnevExpr(ref expr, ref env) => {
            let hash = calculate_hash(&(expr, env));
            match sto.sto_unev_map.get(&hash) {
                Some(idx) => VRef(*idx),
                None => {
                    let idx = sto.sto_vec.len();
                    sto.sto_vec.push(CellThunk(thnk));
                    sto.sto_unev_map.insert(hash, idx);
                    VRef(idx)
                }
            }
        }
        UnevRust(_) => {
            let idx = sto.sto_vec.len();
            sto.sto_vec.push(CellThunk(thnk));
            VRef(idx)
        }
    }
}

pub fn value_to_flat_thunk(es: &mut EvalState, val: &Value<VRef>, sto: &mut Sto) -> FlatThunk {
    match val {
        VInt(x) => FlatThunk(Ev(VInt(*x))),
        VBool(x) => FlatThunk(Ev(VBool(*x))),
        VClosure(nm, bd, env) => FlatThunk(Ev(VClosure(nm.clone(), bd.clone(), env.clone()))),
        VCons(hd, tl) => {
            let hd_v = lookup_sto(es, hd, sto);
            let hd_ = value_to_flat_thunk(es, &hd_v, sto);
            let tl_v = lookup_sto(es, tl, sto);
            let tl_ = value_to_flat_thunk(es, &tl_v, sto);
            FlatThunk(Ev(VCons(Box::new(hd_), Box::new(tl_))))
        }
        VNil => FlatThunk(Ev(VNil)),
        VPair(x, y) => {
            let x_v = lookup_sto(es, x, sto);
            let x_ = value_to_flat_thunk(es, &x_v, sto);
            let y_v = lookup_sto(es, y, sto);
            let y_ = value_to_flat_thunk(es, &y_v, sto);
            FlatThunk(Ev(VPair(Box::new(x_), Box::new(y_))))
        }
    }
}

pub fn flat_thunk_to_sto_ref(es: &mut EvalState, sto: &mut Sto, flat_thunk: FlatThunk) -> VRef {
    let FlatThunk(thunk) = flat_thunk;
    match thunk {
        UnevExpr(expr, env) => add_to_sto(UnevExpr(expr, env), sto),
        UnevRust(clo) => add_to_sto(UnevRust(clo), sto),
        Ev(val) => {
            let new_val = match val {
                VCons(hd, tl) => {
                    let hd_ = flat_thunk_to_sto_ref(es, sto, *hd);
                    let tl_ = flat_thunk_to_sto_ref(es, sto, *tl);
                    VCons(hd_, tl_)
                }
                VPair(x, y) => {
                    let x_ = flat_thunk_to_sto_ref(es, sto, *x);
                    let y_ = flat_thunk_to_sto_ref(es, sto, *y);
                    VPair(x_, y_)
                }

                // these last 4 cases seem like boilerplate, but I believe it's
                // necessary in order to recast the structs with a different `<R>`.
                VInt(x) => VInt(x),
                VBool(x) => VBool(x),
                VClosure(nm, bd, env) => VClosure(nm, bd, env),
                VNil => VNil,
            };
            add_to_sto(Ev(new_val), sto)
        }
    }
}

impl FlatThunk {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
            FlatThunk(Ev(val)) => match val {
                VInt(n) => RcDoc::as_string(n),
                VBool(true) => RcDoc::text("true"),
                VBool(false) => RcDoc::text("false"),
                VClosure(_, _, _) => RcDoc::text("<<closure>>"),
                VCons(hd, tl) => parens(
                    RcDoc::text("cons")
                        .append(sp!())
                        .append(hd.ppr())
                        .append(sp!())
                        .append(tl.ppr()),
                ),
                VNil => RcDoc::text("nil"),
                VPair(a, b) => parens(a.ppr().append(RcDoc::text(", ")).append(b.ppr())),
            },
            _ => panic!("ppr: FlatThunk: unforced thunk"),
        }
    }
}

// // TODO figure out if this is revive-able. `RcDoc` seems to do some
// weirdness with lifetimes that is difficult to make jive with `Sto`.
//
// pub fn ppr_value<'a>(val: Value<VRef>, sto: &'a mut Sto) -> RcDoc<'a, ()> {
//     match val {
//         VInt(n) => RcDoc::as_string(n),
//         VBool(true) => RcDoc::text("true"),
//         VBool(false) => RcDoc::text("false"),
//         VClosure(_, _, _) => RcDoc::text("<<closure>>"),
//         VCons(hd, tl) => {
//             let hd_v = lookup_sto(&hd, sto);
//             let hd_ppr = ppr_value(hd_v, sto);
//             let tl_v = lookup_sto(&tl, sto);
//             let tl_ppr = ppr_value(tl_v, sto);
//             parens(
//                 RcDoc::text("cons")
//                     .append(sp!())
//                     .append(hd_ppr)
//                     .append(sp!())
//                     .append(tl_ppr),
//             )
//         }
//         VNil => RcDoc::text("nil"),
//         VPair(a, b) => {
//             let a_v = lookup_sto(&a, sto);
//             let a_ppr = ppr_value(a_v, sto);
//             let b_v = lookup_sto(&b, sto);
//             let b_ppr = ppr_value(b_v, sto);
//             parens(a_ppr.append(RcDoc::text(", ")).append(b_ppr))
//         }
//     }
// }

pub struct EvalState(u64);

impl EvalState {
    pub fn new() -> EvalState {
        EvalState(0)
    }

    pub fn fresh(&mut self) -> Name {
        let cnt = match self {
            EvalState(c) => {
                *c += 1;
                *c
            }
        };
        let s = format!("_{}", cnt);
        Name(s)
    }
}

impl Default for EvalState {
    fn default() -> Self {
        Self::new()
    }
}

pub fn eval_defn(env: &mut TermEnv, sto: &mut Sto, es: &mut EvalState, defn: &Defn) {
    let Defn(nm, bd) = defn;
    let vr = eval_(env, sto, es, bd);
    env.insert(nm.clone(), vr);
}

pub fn eval_program(env: &mut TermEnv, sto: &mut Sto, es: &mut EvalState, prog: &Program) -> VRef {
    for defn in prog.p_defns.iter() {
        eval_defn(env, sto, es, defn);
    }
    eval_(env, sto, es, &prog.p_body)
}

pub fn eval(sto: &mut Sto, expr: &Expr) -> VRef {
    let env = new_term_env();
    let mut es = EvalState::new();
    eval_(&env, sto, &mut es, expr)
}

macro_rules! primop_binop_int {
    ( $op: tt, $op_name: literal, $es: expr, $arg_1: expr, $arg_2: expr, $sto: expr ) => {
        match (lookup_sto($es, $arg_1, $sto), lookup_sto($es, $arg_2, $sto)) {
            (VInt(a_), VInt(b_)) => {
                let val = VInt(a_ $op b_);
                add_to_sto(Ev(val), $sto)
            }
            _ => panic!("{}: bad types", $op_name),
        }
    };
}

pub fn eval_(env: &TermEnv, sto: &mut Sto, es: &mut EvalState, expr: &Expr) -> VRef {
    match primop_apply_case(es, expr) {
        // in this case we directly interpret the PrimOp.
        PrimOpApplyCase::FullyApplied(op, args) => {
            let args_v: Vec<VRef> = args
                .iter()
                .map(|arg| eval_(env, sto, es, &arg.clone()))
                .collect();
            match op {
                PrimOp::Add => primop_binop_int!(+, "+", es, &args_v[0], &args_v[1], sto),
                PrimOp::Sub => primop_binop_int!(-, "-", es, &args_v[0], &args_v[1], sto),
                PrimOp::Mul => primop_binop_int!(*, "*", es, &args_v[0], &args_v[1], sto),
                PrimOp::Div => primop_binop_int!(/, "/", es, &args_v[0], &args_v[1], sto),
                PrimOp::Eql => match (
                    lookup_sto(es, &args_v[0], sto),
                    lookup_sto(es, &args_v[1], sto),
                ) {
                    (VInt(a_), VInt(b_)) => {
                        let val = VBool(a_ == b_);
                        add_to_sto(Ev(val), sto)
                    }
                    _ => panic!("==: bad types"),
                },
                PrimOp::Null => {
                    let val = match lookup_sto(es, &args_v[0], sto) {
                        VCons(_, _) => VBool(false),
                        VNil => VBool(true),
                        _ => panic!("null: bad types"),
                    };
                    add_to_sto(Ev(val), sto)
                }
                PrimOp::Pair => {
                    let val = VPair(args_v[0], args_v[1]);
                    add_to_sto(Ev(val), sto)
                }
                PrimOp::Fst => match lookup_sto(es, &args_v[0], sto) {
                    VPair(a, _) => a,
                    _ => panic!("fst: bad types"),
                },
                PrimOp::Snd => match lookup_sto(es, &args_v[0], sto) {
                    VPair(_, b) => b,
                    _ => panic!("snd: bad types"),
                },
                PrimOp::Cons => {
                    let val = VCons(args_v[0], args_v[1]);
                    add_to_sto(Ev(val), sto)
                }
                PrimOp::Head => match lookup_sto(es, &args_v[0], sto) {
                    VCons(hd, _tl) => hd,
                    VNil => panic!("head: called on empty list"),
                    _ => panic!("head: bad types"),
                },
                PrimOp::Tail => match lookup_sto(es, &args_v[0], sto) {
                    VCons(_hd, tl) => tl,
                    VNil => panic!("tail: called on empty list"),
                    _ => panic!("tail: bad types"),
                },
                PrimOp::Nil => panic!("nil: application of non-function"),
            }
        }

        PrimOpApplyCase::PartiallyApplied(lam) => eval_(env, sto, es, &lam),

        // we do not find a direct PrimOp application, so we interpret normally.
        PrimOpApplyCase::Other => match expr {
            Expr::Lit(Lit::LInt(x)) => add_to_sto(Ev(VInt(*x)), sto),
            Expr::Lit(Lit::LBool(x)) => add_to_sto(Ev(VBool(*x)), sto),

            Expr::Var(x) => match env.get(x) {
                None => panic!("impossible: free variable: {:?}", x),
                Some(v) => *v,
            },

            Expr::Lam(nm, bd) => add_to_sto(Ev(VClosure(nm.clone(), bd.clone(), env.clone())), sto),

            Expr::Let(x, e, bd) => {
                let e_v = eval_(env, sto, es, e);
                let mut new_env = env.clone();
                new_env.insert(x.clone(), e_v);
                eval_(&new_env, sto, es, bd)
            }

            Expr::If(tst, thn, els) => {
                let tst_ref = eval_(env, sto, es, tst);
                match lookup_sto(es, &tst_ref, sto) {
                    VBool(true) => eval_(env, sto, es, thn),
                    VBool(false) => eval_(env, sto, es, els),
                    _ => panic!("impossible: non-bool in test position of if"),
                }
            }

            // we treat `Nil` here differently from the other `PrimOp`s,
            // interpreting it directly as a value (since it is not a function,
            // like all the other `PrimOp`s.
            Expr::Prim(PrimOp::Nil) => add_to_sto(Ev(VNil), sto),

            // this represents a PrimOp that is not in application position.
            // since it is then being used as an argument (or being bound), we
            // must package it into a closure so it can be used "lifted".
            //
            // if it is a nullary primop, we do not need to wrap it in a
            // closure, and we don't.
            Expr::Prim(op) => {
                let arity = primop_arity(op);
                assert_ne!(arity, 0, "unhandled nullary primop");
                let fresh_names: Vec<Name> = iter::repeat_with(|| es.fresh()).take(arity).collect();

                // create the inner lambda body, successively apply `op` to
                // each fresh name.
                let apply_vars = |acc, nm: &Name| app!(acc, Expr::Var(nm.clone()));
                let app_body = fresh_names.iter().fold(Expr::Prim(op.clone()), apply_vars);

                // create the full lambda, successively wrapping a lambda which
                // binds each fresh name.
                let wrap_lambda = |acc, nm| lam!(nm, acc);
                let full_lam = fresh_names.into_iter().rev().fold(app_body, wrap_lambda);

                // cheeky shortcut to repackage lambda into a `VClosure`, so we
                // can retain generality / avoid special-casing on the outermost
                // lambda.
                eval(sto, &full_lam)
            }

            Expr::App(fun, arg) => {
                let fun_ref = eval_(env, sto, es, fun);
                let fun_val = lookup_sto(es, &fun_ref, sto);
                match fun_val {
                    VClosure(nm, bd, clo) => {
                        let mut new_env = clo;
                        let arg_thnk = UnevExpr(*arg.clone(), env.clone());
                        let arg_thnk_ref = add_to_sto(arg_thnk, sto);
                        new_env.insert(nm, arg_thnk_ref);
                        eval_(&new_env, sto, es, &bd)
                    }
                    _ => panic!("impossible: non-closure in function position of app"),
                }
            }

            Expr::Fix(e) => eval_(
                env,
                sto,
                es,
                &Expr::App(e.clone(), Box::new(Expr::Fix(e.clone()))),
            ),
        },
    }
}

enum PrimOpApplyCase {
    FullyApplied(PrimOp, Vec<Expr>),
    PartiallyApplied(Expr),
    Other,
}

/// look for a "direct application" of a PrimOp. this means that it is nested
/// within some number of `App`s, with no intervening other constructors. we
/// want to find this because we can directly interpret these, rather than
/// packaging them into closures.
///
/// we must take the `in_app` boolean so that we can differentiate "bare"
/// PrimOps, which are not inside an `App`, from enclosed ones.
fn find_prim_app(expr: &Expr, in_app: bool) -> Option<(PrimOp, Vec<Expr>)> {
    match expr {
        Expr::App(fun, arg) => {
            let (op, mut args) = find_prim_app(fun, true)?;
            args.push(*arg.clone());
            Some((op, args))
        }
        Expr::Prim(op) if in_app => Some((op.clone(), Vec::new())),
        _ => None,
    }
}

fn primop_apply_case(es: &mut EvalState, expr: &Expr) -> PrimOpApplyCase {
    match find_prim_app(expr, false) {
        None => PrimOpApplyCase::Other,
        Some((op, args)) => {
            let delta = primop_arity(&op) - args.len();
            match delta.cmp(&0) {
                Ordering::Less => panic!(
                    "primop_apply_case: impossible: primop {:?} is over-applied",
                    op
                ),

                // fully applied
                Ordering::Equal => PrimOpApplyCase::FullyApplied(op, args),

                // not fully applied
                Ordering::Greater => {
                    // generate fresh names for the args which have not been applied
                    let names: Vec<Name> = iter::repeat_with(|| es.fresh()).take(delta).collect();
                    // wrap said fresh names into `Expr`s
                    let name_vars = names.clone().into_iter().map(Expr::Var);
                    // iterator which runs through the provided arguments, adding the fresh names
                    // onto the end to fill out to a full application
                    let all_args = args.into_iter().chain(name_vars);
                    // fold over the arguments to construct a full application of `op`
                    let app_f = |f, arg| Expr::App(Box::new(f), Box::new(arg));
                    let app = all_args.fold(Expr::Prim(op), app_f);
                    // fold over the generated freshnames to construct a lambda which will bind the
                    // names used in the applicaton
                    let lam_f = |bd, nm| Expr::Lam(nm, Box::new(bd));
                    let lam = names.into_iter().rev().fold(app, lam_f);
                    // return the constructed `Expr`
                    PrimOpApplyCase::PartiallyApplied(lam)
                }
            }
        }
    }
}

use pretty::RcDoc;
use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    iter,
};

use rep_lang_concrete_syntax::{sp, util::pretty::parens};
use rep_lang_core::{
    abstract_syntax::{primop_arity, Defn, Expr, Lit, Name, PrimOp, Program},
    app, lam,
    util::calculate_hash,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct VRef(usize);

#[derive(Debug, Eq, Hash)]
pub enum Value<R> {
    VInt(i64),
    VBool(bool),
    VClosure(Name, Box<Expr>, TermEnv),
    VCons(R, R),
    VNil,
    VPair(R, R),
}

// TODO add thunk type

// TODO this must wrap Thunk
#[derive(Debug, PartialEq)]
pub struct FlatValue(pub Value<Box<FlatValue>>);

// TODO modify this to mirror `FlatValue` changes
#[macro_export]
macro_rules! vcons {
    ( $a: expr, $b: expr ) => {
        FlatValue(Value::VCons(Box::new($a), Box::new($b)))
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

type TermEnv = BTreeMap<Name, VRef>;

pub fn new_term_env() -> TermEnv {
    BTreeMap::new()
}

#[derive(Debug)]
pub struct Sto {
    // TODO change this to Thunk<VRef>
    //               |
    pub sto_vec: Vec<Value<VRef>>,
    // instead of using `Value<VRef>` as keys (which would store the whole value
    // in the HashMap), we only store a hash as key.
    pub sto_ev_map: HashMap<u64, usize>,
    // for later - lazy
    // pub sto_unev_map: HashMap<(Expr, TermEnv), usize>,
}

impl Sto {
    pub fn new() -> Sto {
        Sto {
            sto_vec: Vec::new(),
            sto_ev_map: HashMap::new(),
        }
    }
}

// TODO decide if this should return a `Value` (thereby forcing the thunk at
// the provided index), or a `Thunk` (which could still be unevaluated).
//
// `Value` is probably the right decision.
pub fn lookup_sto<'a>(vr: &VRef, sto: &'a Sto) -> &'a Value<VRef> {
    let VRef(idx) = *vr;
    match sto.sto_vec.get(idx) {
        Some(val) => val,
        None => panic!("lookup_sto: out of bounds"),
    }
}

pub fn add_to_sto(val: Value<VRef>, sto: &mut Sto) -> VRef {
    let hash = calculate_hash(&val);
    match sto.sto_ev_map.get(&hash) {
        None => {
            let idx = sto.sto_vec.len();
            sto.sto_vec.push(val);
            sto.sto_ev_map.insert(hash, idx);
            VRef(idx)
        }
        Some(idx) => VRef(*idx),
    }
}

pub fn value_to_flat_value(val: &Value<VRef>, sto: &Sto) -> FlatValue {
    match val {
        VInt(x) => FlatValue(VInt(*x)),
        VBool(x) => FlatValue(VBool(*x)),
        VClosure(nm, bd, env) => FlatValue(VClosure(nm.clone(), bd.clone(), env.clone())),
        VCons(hd, tl) => {
            let hd_ = value_to_flat_value(lookup_sto(hd, sto), sto);
            let tl_ = value_to_flat_value(lookup_sto(tl, sto), sto);
            FlatValue(VCons(Box::new(hd_), Box::new(tl_)))
        }
        VNil => FlatValue(VNil),
        VPair(x, y) => {
            let x_ = value_to_flat_value(lookup_sto(x, sto), sto);
            let y_ = value_to_flat_value(lookup_sto(y, sto), sto);
            FlatValue(VPair(Box::new(x_), Box::new(y_)))
        }
    }
}

pub fn ppr_value_ref<'a>(val: &'a Value<VRef>, sto: &'a Sto) -> RcDoc<'a, ()> {
    match val {
        VInt(n) => RcDoc::as_string(n),
        VBool(true) => RcDoc::text("true"),
        VBool(false) => RcDoc::text("false"),
        VClosure(_, _, _) => RcDoc::text("<<closure>>"),
        VCons(hd, tl) => {
            let hd_ppr = ppr_value_ref(lookup_sto(hd, sto), sto);
            let tl_ppr = ppr_value_ref(lookup_sto(tl, sto), sto);
            parens(
                RcDoc::text("cons")
                    .append(sp!())
                    .append(hd_ppr)
                    .append(sp!())
                    .append(tl_ppr),
            )
        }
        VNil => RcDoc::text("nil"),
        VPair(a, b) => {
            let a_ppr = ppr_value_ref(lookup_sto(a, sto), sto);
            let b_ppr = ppr_value_ref(lookup_sto(b, sto), sto);
            parens(a_ppr.append(RcDoc::text(", ")).append(b_ppr))
        }
    }
}

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

pub fn eval_program(prog: &Program) -> (VRef, TermEnv, Sto) {
    let mut env = new_term_env();
    let mut es = EvalState::new();
    let mut sto = Sto::new();
    for Defn(nm, bd) in prog.p_defns.iter() {
        let val = eval_(&env, &mut sto, &mut es, bd);
        env.insert(nm.clone(), val);
    }
    (eval_(&env, &mut sto, &mut es, &prog.p_body), env, sto)
}

pub fn eval(sto: &mut Sto, expr: &Expr) -> VRef {
    let env = new_term_env();
    let mut es = EvalState::new();
    eval_(&env, sto, &mut es, expr)
}

macro_rules! primop_binop_int {
    ( $op: tt, $op_name: literal, $arg_1: expr, $arg_2: expr, $sto: expr ) => {
        match (lookup_sto($arg_1, $sto), lookup_sto($arg_2, $sto)) {
            (VInt(a_), VInt(b_)) => {
                let val = VInt(a_ $op b_);
                add_to_sto(val, $sto)
            }
            _ => panic!("{}: bad types", $op_name),
        }
    };
}

use Value::*;
pub fn eval_(env: &TermEnv, sto: &mut Sto, es: &mut EvalState, expr: &Expr) -> VRef {
    match primop_apply_case(es, expr) {
        // in this case we directly interpret the PrimOp.
        PrimOpApplyCase::FullyApplied(op, args) => {
            let args_v: Vec<VRef> = args
                .iter()
                .map(|arg| eval_(env, sto, es, &arg.clone()))
                .collect();
            match op {
                PrimOp::Add => primop_binop_int!(+, "+", &args_v[0], &args_v[1], sto),
                PrimOp::Sub => primop_binop_int!(-, "-", &args_v[0], &args_v[1], sto),
                PrimOp::Mul => primop_binop_int!(*, "*", &args_v[0], &args_v[1], sto),
                PrimOp::Div => primop_binop_int!(/, "/", &args_v[0], &args_v[1], sto),
                PrimOp::Eql => match (lookup_sto(&args_v[0], sto), lookup_sto(&args_v[1], sto)) {
                    (VInt(a_), VInt(b_)) => {
                        let val = VBool(a_ == b_);
                        add_to_sto(val, sto)
                    }
                    _ => panic!("==: bad types"),
                },
                PrimOp::Null => {
                    let val = match lookup_sto(&args_v[0], sto) {
                        VCons(_, _) => VBool(false),
                        VNil => VBool(true),
                        _ => panic!("null: bad types"),
                    };
                    add_to_sto(val, sto)
                }
                PrimOp::Pair => {
                    let val = VPair(args_v[0], args_v[1]);
                    add_to_sto(val, sto)
                }
                PrimOp::Fst => match lookup_sto(&args_v[0], sto) {
                    VPair(a, _) => *a,
                    _ => panic!("fst: bad types"),
                },
                PrimOp::Snd => match lookup_sto(&args_v[0], sto) {
                    VPair(_, b) => *b,
                    _ => panic!("snd: bad types"),
                },
                PrimOp::Cons => {
                    let val = VCons(args_v[0], args_v[1]);
                    add_to_sto(val, sto)
                }
                PrimOp::Nil => panic!("nil: application of non-function"),
            }
        }

        PrimOpApplyCase::PartiallyApplied(lam) => eval_(env, sto, es, &lam),

        // we do not find a direct PrimOp application, so we interpret normally.
        PrimOpApplyCase::Other => match expr {
            Expr::Lit(Lit::LInt(x)) => add_to_sto(VInt(*x), sto),
            Expr::Lit(Lit::LBool(x)) => add_to_sto(VBool(*x), sto),

            Expr::Var(x) => match env.get(x) {
                None => panic!("impossible: free variable: {:?}", x),
                Some(v) => *v,
            },

            Expr::Lam(nm, bd) => add_to_sto(VClosure(nm.clone(), bd.clone(), env.clone()), sto),

            Expr::Let(x, e, bd) => {
                let e_v = eval_(env, sto, es, e);
                let mut new_env = env.clone();
                new_env.insert(x.clone(), e_v);
                eval_(&new_env, sto, es, bd)
            }

            Expr::If(tst, thn, els) => {
                let tst_ref = eval_(env, sto, es, tst);
                match lookup_sto(&tst_ref, sto) {
                    VBool(true) => eval_(env, sto, es, thn),
                    VBool(false) => eval_(env, sto, es, els),
                    _ => panic!("impossible: non-bool in test position of if"),
                }
            }

            // we treat `Nil` here differently from the other `PrimOp`s,
            // interpreting it directly as a value (since it is not a function,
            // like all the other `PrimOp`s.
            Expr::Prim(PrimOp::Nil) => add_to_sto(VNil, sto),

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
                let fun_val = lookup_sto(&fun_ref, sto);
                match fun_val {
                    VClosure(nm, bd, clo) => {
                        let nm2 = nm.clone();
                        let bd2 = bd.clone();
                        let mut new_env = clo.clone();
                        // it's possible we need to
                        // delay this ------------------+
                        // or it might just work.       |
                        let arg_v = eval_(env, sto, es, arg);
                        new_env.insert(nm2, arg_v);
                        eval_(&new_env, sto, es, &bd2)
                    }
                    _ => panic!("impossible: non-closure in function position of app"),
                }
            }
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

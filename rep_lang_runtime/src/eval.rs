use pretty::RcDoc;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    fmt, iter,
};

use rep_lang_concrete_syntax::{sp, util::pretty::parens};
use rep_lang_core::{
    abstract_syntax::{gas_of_expr, primop_arity, Defn, Expr, Gas, Lit, Name, PrimOp, Program},
    app, error, lam,
    util::calculate_hash,
};

use crate::fte;
use Expr::*;
use StoCell::*;
use Thunk::*;
use Value::*;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub struct VRef(usize);

// TODO assess whether deriving PartialEq is ok. I was manually implementing PartialEq so that it
// closures would always be non-equal. I don't see we should do that instead of a regular check,
// other than efficiency concerns.
/// `R` and `CR` are separate type variables because `FlatValue` must instantiate values and thunks
/// at different types.
///
/// `IValue` instantiates both at `VRef` because they are effectively the same thing when housed in
/// the `Sto`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub enum Value<R, CR> {
    VInt(i64),
    VBool(bool),
    VClosure(Name, Box<Expr>, TermEnv<CR>),
    VCons(R, R),
    VNil,
    VPair(R, R),
}

// TODO figure out our PartialEq/Eq story. was it needed for HashMap? why did we only compare `Ev`?
#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
/// see `Value` for commentary on `R` and `CR`.
pub enum Thunk<M, R, CR> {
    UnevExpr(Expr, TermEnv<CR>),
    Marker(M),
    Ev(Value<R, CR>),
}

type IThunk<M> = Thunk<M, VRef, VRef>;
type IValue = Value<VRef, VRef>;

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub struct FlatValue<M>(pub Value<Box<FlatValue<M>>, FlatThunk<M>>);

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub struct FlatThunk<M>(pub Thunk<M, Box<FlatThunk<M>>, FlatThunk<M>>);

pub fn inject_flatvalue_to_flatthunk<M>(flat_val: FlatValue<M>) -> FlatThunk<M> {
    let FlatValue(val) = flat_val;
    match val {
        VInt(x) => fte!(VInt(x)),
        VBool(x) => fte!(VBool(x)),
        VClosure(nm, bd, env) => fte!(VClosure(nm, bd, env)),
        VNil => fte!(VNil),
        VCons(x, y) => {
            let x_ft = inject_flatvalue_to_flatthunk(*x);
            let y_ft = inject_flatvalue_to_flatthunk(*y);
            fte!(VCons(Box::new(x_ft), Box::new(y_ft)))
        }
        VPair(x, y) => {
            let x_ft = inject_flatvalue_to_flatthunk(*x);
            let y_ft = inject_flatvalue_to_flatthunk(*y);
            fte!(VPair(Box::new(x_ft), Box::new(y_ft)))
        }
    }
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
        FlatValue(Value::VCons(Box::new($a), Box::new($b)))
    };
}

impl<M, R, CR> fmt::Debug for Thunk<M, R, CR>
where
    M: fmt::Debug,
    R: fmt::Debug,
    CR: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ev(val) => f.debug_struct("Ev").field("_", val).finish(),
            UnevExpr(expr, env) => f
                .debug_struct("UnevExpr")
                .field("expr", &expr)
                .field("env", &env)
                .finish(),
            Marker(m) => f.debug_struct("Marker").field("_", m).finish(),
        }
    }
}

type TermEnv<V> = BTreeMap<Name, V>;
type ITermEnv = TermEnv<VRef>;

pub fn new_term_env<V>() -> TermEnv<V> {
    BTreeMap::new()
}

#[derive(Debug)]
pub enum StoCell<M> {
    CellThunk(IThunk<M>),
    CellRedirect(VRef),
}

#[derive(Debug)]
pub struct Sto<M> {
    // "heap" of addressed thunks
    pub sto_vec: Vec<StoCell<M>>,

    // TODO decide if we will cache `Marker`s

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

impl<M> Sto<M> {
    pub fn new() -> Sto<M> {
        Sto {
            sto_vec: Vec::new(),
            sto_ev_map: HashMap::new(),
            sto_unev_map: HashMap::new(),
        }
    }
}

impl<M> Default for Sto<M> {
    fn default() -> Self {
        Self::new()
    }
}

// TODO this likely performs significant amounts of cloning. however, it's
// possible this is approximately the minimal amount of cloning possible.
pub fn lookup_sto<'a, M>(es: &mut EvalState, vr: &VRef, sto: &'a mut Sto<M>) -> IValue {
    let VRef(idx) = *vr;
    match sto.sto_vec.get_mut(idx) {
        None => error!("lookup_sto: out of bounds"),
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
            Marker(_m) => {
                todo!("call the oracle here")
            }
        },
    }
}

pub fn get_sto<'a, M>(es: &mut EvalState, vr: &VRef, sto: &'a mut Sto<M>) -> IThunk<M>
where
    M: Clone,
{
    let VRef(idx) = *vr;
    match sto.sto_vec.get_mut(idx) {
        None => error!("lookup_sto: out of bounds"),
        Some(CellRedirect(vr2)) => get_sto(es, &vr2.clone(), sto),
        Some(CellThunk(thnk)) => thnk.clone(),
    }
}

pub fn add_to_sto<M>(thnk: IThunk<M>, sto: &mut Sto<M>) -> VRef {
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
        Marker(_) => {
            let idx = sto.sto_vec.len();
            sto.sto_vec.push(CellThunk(thnk));
            VRef(idx)
        }
    }
}

pub fn thunk_to_flat_thunk<M>(
    es: &mut EvalState,
    thnk: &IThunk<M>,
    sto: &mut Sto<M>,
) -> FlatThunk<M>
where
    M: Clone,
{
    match thnk {
        Ev(val) => inject_flatvalue_to_flatthunk(value_to_flat_value(es, val, sto)),
        UnevExpr(expr, env) => {
            let mut new_env: TermEnv<FlatThunk<M>> = new_term_env();
            for (nm, vr) in env {
                let thnk = get_sto(es, vr, sto);
                let flat_thnk = thunk_to_flat_thunk(es, &thnk, sto);
                new_env.insert(nm.clone(), flat_thnk);
            }
            FlatThunk(UnevExpr(expr.clone(), new_env))
        }
        Marker(m) => FlatThunk(Marker(m.clone())),
    }
}

pub fn value_to_flat_value<M>(es: &mut EvalState, val: &IValue, sto: &mut Sto<M>) -> FlatValue<M>
where
    M: Clone,
{
    match val {
        VInt(x) => FlatValue(VInt(*x)),
        VBool(x) => FlatValue(VBool(*x)),
        VClosure(nm, bd, env) => {
            let mut new_env: TermEnv<FlatThunk<M>> = new_term_env();
            for (nm, vr) in env {
                let thnk = get_sto(es, vr, sto);
                let flat_thnk = thunk_to_flat_thunk(es, &thnk, sto);
                new_env.insert(nm.clone(), flat_thnk);
            }
            FlatValue(VClosure(nm.clone(), bd.clone(), new_env))
        }
        VCons(hd, tl) => {
            let hd_v = lookup_sto(es, hd, sto);
            let hd_ = value_to_flat_value(es, &hd_v, sto);
            let tl_v = lookup_sto(es, tl, sto);
            let tl_ = value_to_flat_value(es, &tl_v, sto);
            FlatValue(VCons(Box::new(hd_), Box::new(tl_)))
        }
        VNil => FlatValue(VNil),
        VPair(x, y) => {
            let x_v = lookup_sto(es, x, sto);
            let x_ = value_to_flat_value(es, &x_v, sto);
            let y_v = lookup_sto(es, y, sto);
            let y_ = value_to_flat_value(es, &y_v, sto);
            FlatValue(VPair(Box::new(x_), Box::new(y_)))
        }
    }
}

pub fn flat_thunk_to_sto_ref<M>(
    es: &mut EvalState,
    sto: &mut Sto<M>,
    flat_thunk: FlatThunk<M>,
) -> VRef {
    let FlatThunk(thunk) = flat_thunk;
    match thunk {
        UnevExpr(expr, env) => {
            let mut new_env: TermEnv<VRef> = new_term_env();
            for (nm, f_t) in env {
                let vr = flat_thunk_to_sto_ref(es, sto, f_t);
                new_env.insert(nm.clone(), vr);
            }
            add_to_sto(UnevExpr(expr, new_env), sto)
        }
        Marker(m) => add_to_sto(Marker(m), sto),
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
                VClosure(nm, bd, env) => {
                    let mut new_env: TermEnv<VRef> = new_term_env();
                    for (nm, f_t) in env {
                        let vr = flat_thunk_to_sto_ref(es, sto, f_t);
                        new_env.insert(nm.clone(), vr);
                    }
                    VClosure(nm, bd, new_env)
                }
                VNil => VNil,
            };
            add_to_sto(Ev(new_val), sto)
        }
    }
}

impl<M> FlatValue<M> {
    pub fn ppr(&self) -> RcDoc<()> {
        let FlatValue(val) = self;
        match val {
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

pub struct EvalState {
    fresh_name_counter: u64,
    gas_counter: Gas,
}

impl EvalState {
    pub fn new() -> EvalState {
        EvalState {
            fresh_name_counter: 0,
            gas_counter: 0,
        }
    }

    pub fn fresh_name(&mut self) -> Name {
        let cnt = self.fresh_name_counter;
        self.fresh_name_counter += 1;
        let s = format!("_{}", cnt);
        Name(s)
    }

    pub fn add_gas_for_expr(&mut self, expr: &Expr) {
        self.gas_counter += gas_of_expr(expr);
    }

    pub fn current_gas_count(&self) -> Gas {
        self.fresh_name_counter
    }
}

impl Default for EvalState {
    fn default() -> Self {
        Self::new()
    }
}

pub fn eval_defn<M>(env: &mut ITermEnv, sto: &mut Sto<M>, es: &mut EvalState, defn: &Defn) {
    let Defn(nm, bd) = defn;
    let vr = eval_(env, sto, es, bd);
    env.insert(nm.clone(), vr);
}

pub fn eval_program<M>(
    env: &mut ITermEnv,
    sto: &mut Sto<M>,
    es: &mut EvalState,
    prog: &Program,
) -> VRef {
    for defn in prog.p_defns.iter() {
        eval_defn(env, sto, es, defn);
    }
    eval_(env, sto, es, &prog.p_body)
}

pub fn eval<M>(sto: &mut Sto<M>, expr: &Expr) -> VRef {
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
            bad => error!("{}: bad types: {:?}", $op_name, bad),
        }
    };
}

pub fn eval_<M>(env: &ITermEnv, sto: &mut Sto<M>, es: &mut EvalState, expr: &Expr) -> VRef {
    es.add_gas_for_expr(expr);
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
                    bad => error!("==: bad types: {:?}", bad),
                },
                PrimOp::And => match (
                    lookup_sto(es, &args_v[0], sto),
                    lookup_sto(es, &args_v[1], sto),
                ) {
                    (VBool(a_), VBool(b_)) => {
                        let val = VBool(a_ && b_);
                        add_to_sto(Ev(val), sto)
                    }
                    bad => error!("and: bad types: {:?}", bad),
                },
                PrimOp::Or => match (
                    lookup_sto(es, &args_v[0], sto),
                    lookup_sto(es, &args_v[1], sto),
                ) {
                    (VBool(a_), VBool(b_)) => {
                        let val = VBool(a_ || b_);
                        add_to_sto(Ev(val), sto)
                    }
                    bad => error!("or: bad types: {:?}", bad),
                },
                PrimOp::Not => match lookup_sto(es, &args_v[0], sto) {
                    VBool(a_) => {
                        let val = VBool(!a_);
                        add_to_sto(Ev(val), sto)
                    }
                    bad => error!("not: bad types: {:?}", bad),
                },
                PrimOp::Lt => match (
                    lookup_sto(es, &args_v[0], sto),
                    lookup_sto(es, &args_v[1], sto),
                ) {
                    (VInt(a_), VInt(b_)) => {
                        let val = VBool(a_ < b_);
                        add_to_sto(Ev(val), sto)
                    }
                    bad => error!("<: bad types: {:?}", bad),
                },
                PrimOp::Gt => match (
                    lookup_sto(es, &args_v[0], sto),
                    lookup_sto(es, &args_v[1], sto),
                ) {
                    (VInt(a_), VInt(b_)) => {
                        let val = VBool(a_ > b_);
                        add_to_sto(Ev(val), sto)
                    }
                    bad => error!(">: bad types: {:?}", bad),
                },
                PrimOp::Null => {
                    let val = match lookup_sto(es, &args_v[0], sto) {
                        VCons(_, _) => VBool(false),
                        VNil => VBool(true),
                        bad => error!("null: bad types: {:?}", bad),
                    };
                    add_to_sto(Ev(val), sto)
                }
                PrimOp::Pair => {
                    let val = VPair(args_v[0], args_v[1]);
                    add_to_sto(Ev(val), sto)
                }
                PrimOp::Fst => match lookup_sto(es, &args_v[0], sto) {
                    VPair(a, _) => a,
                    bad => error!("fst: bad types: {:?}", bad),
                },
                PrimOp::Snd => match lookup_sto(es, &args_v[0], sto) {
                    VPair(_, b) => b,
                    bad => error!("snd: bad types: {:?}", bad),
                },
                PrimOp::Cons => {
                    let val = VCons(args_v[0], args_v[1]);
                    add_to_sto(Ev(val), sto)
                }
                PrimOp::Head => match lookup_sto(es, &args_v[0], sto) {
                    VCons(hd, _tl) => hd,
                    VNil => error!("head: called on empty list"),
                    bad => error!("head: bad types: {:?}", bad),
                },
                PrimOp::Tail => match lookup_sto(es, &args_v[0], sto) {
                    VCons(_hd, tl) => tl,
                    VNil => error!("tail: called on empty list"),
                    bad => error!("tail: bad types: {:?}", bad),
                },
                PrimOp::Nil => error!("nil: application of non-function"),
            }
        }

        PrimOpApplyCase::PartiallyApplied(lam) => eval_(env, sto, es, &lam),

        // we do not find a direct PrimOp application, so we interpret normally.
        PrimOpApplyCase::Other => match expr {
            Expr::Lit(Lit::LInt(x)) => add_to_sto(Ev(VInt(*x)), sto),
            Expr::Lit(Lit::LBool(x)) => add_to_sto(Ev(VBool(*x)), sto),

            Expr::Var(x) => match env.get(x) {
                None => error!("impossible: free variable: {:?}", x),
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
                    _ => error!("impossible: non-bool in test position of if"),
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
                let fresh_names: Vec<Name> =
                    iter::repeat_with(|| es.fresh_name()).take(arity).collect();

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
                    _ => error!("impossible: non-closure in function position of app"),
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
                Ordering::Less => error!(
                    "primop_apply_case: impossible: primop {:?} is over-applied",
                    op
                ),

                // fully applied
                Ordering::Equal => PrimOpApplyCase::FullyApplied(op, args),

                // not fully applied
                Ordering::Greater => {
                    // generate fresh names for the args which have not been applied
                    let names: Vec<Name> =
                        iter::repeat_with(|| es.fresh_name()).take(delta).collect();
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

pub trait Normalizable {
    fn normalize(&self, hm: &mut HashMap<Name, Name>, es: &mut EvalState) -> Self;
}

impl<M> Normalizable for FlatThunk<M>
where
    M: Clone,
{
    fn normalize(
        self: &FlatThunk<M>,
        hm: &mut HashMap<Name, Name>,
        es: &mut EvalState,
    ) -> FlatThunk<M> {
        let FlatThunk(thunk) = self;
        FlatThunk(thunk.normalize(hm, es))
    }
}

impl<M, R, CR> Normalizable for Thunk<M, Box<R>, CR>
where
    M: Clone,
    R: Normalizable + Clone,
    CR: Normalizable + Clone,
{
    fn normalize(&self, hm: &mut HashMap<Name, Name>, es: &mut EvalState) -> Self {
        match self {
            Ev(val) => Ev(val.normalize(hm, es)),
            UnevExpr(expr, env) => {
                // norm env first, so that any remapped Names get mapped in expr
                let env_norm = env.normalize(hm, es);
                let expr_norm = expr.normalize(hm, es);
                UnevExpr(expr_norm, env_norm)
            }
            Marker(m) => Marker(m.clone()),
        }
    }
}

impl<CR> Normalizable for TermEnv<CR>
where
    CR: Normalizable + Clone,
{
    fn normalize(&self, hm: &mut HashMap<Name, Name>, es: &mut EvalState) -> Self {
        self.iter()
            .map(|(nm_env, flat_thunk)| {
                let nm_env_norm = es.fresh_name();
                hm.insert(nm_env.clone(), nm_env_norm.clone());
                // clone hm to avoid "lateral" contagion
                (nm_env_norm, flat_thunk.normalize(&mut hm.clone(), es))
            })
            .collect()
    }
}

impl<M> Normalizable for FlatValue<M>
where
    M: Clone,
{
    fn normalize(&self, hm: &mut HashMap<Name, Name>, es: &mut EvalState) -> Self {
        let FlatValue(val) = self;
        FlatValue(val.normalize(hm, es))
    }
}

impl<R, CR> Normalizable for Value<Box<R>, CR>
where
    R: Normalizable + Clone,
    CR: Normalizable + Clone,
{
    fn normalize(&self, hm: &mut HashMap<Name, Name>, es: &mut EvalState) -> Self {
        match self {
            VInt(_) | VBool(_) | VNil => (*self).clone(),
            VClosure(nm, bd, env) => {
                // TODO verify this is right
                let env_norm = env.normalize(hm, es);
                // INFO `bd` after `env`, so that vars in `bd` (which refer to `env`) get mapped right.
                let nm_norm = es.fresh_name();
                hm.insert(nm.clone(), nm_norm.clone());
                let bd_norm = Box::new(bd.normalize(hm, es));
                VClosure(nm_norm, bd_norm, env_norm)
            }
            VCons(x, y) => {
                // clone hm to avoid "lateral" contagion
                let x_norm = Box::new(x.normalize(&mut hm.clone(), es));
                let y_norm = Box::new(y.normalize(hm, es));
                VCons(x_norm, y_norm)
            }
            VPair(x, y) => {
                // clone hm to avoid "lateral" contagion
                let x_norm = Box::new(x.normalize(&mut hm.clone(), es));
                let y_norm = Box::new(y.normalize(hm, es));
                VPair(x_norm, y_norm)
            }
        }
    }
}

impl Normalizable for Expr {
    fn normalize(&self, hm: &mut HashMap<Name, Name>, es: &mut EvalState) -> Self {
        match self {
            Lit(_) | Prim(_) => self.clone(),

            Var(x) => match hm.get(x) {
                None => error!("normalize: free variable: {:?}", x),
                Some(v) => Var(v.clone()),
            },

            Lam(nm, bd) => {
                let nm_norm = es.fresh_name();
                hm.insert(nm.clone(), nm_norm.clone());
                let bd_norm = Box::new(bd.normalize(hm, es));
                Lam(nm_norm, bd_norm)
            }

            Let(nm, e, bd) => {
                let nm_norm = es.fresh_name();
                hm.insert(nm.clone(), nm_norm.clone());
                let e_norm = Box::new(e.normalize(&mut hm.clone(), es));
                let bd_norm = Box::new(bd.normalize(hm, es));
                Let(nm_norm, e_norm, bd_norm)
            }

            If(tst, thn, els) => {
                let tst_norm = Box::new(tst.normalize(&mut hm.clone(), es));
                let thn_norm = Box::new(thn.normalize(&mut hm.clone(), es));
                let els_norm = Box::new(els.normalize(hm, es));
                If(tst_norm, thn_norm, els_norm)
            }

            App(fun, arg) => {
                let fun_norm = Box::new(fun.normalize(&mut hm.clone(), es));
                let arg_norm = Box::new(arg.normalize(hm, es));
                App(fun_norm, arg_norm)
            }

            Fix(e) => Fix(Box::new(e.normalize(hm, es))),
        }
    }
}

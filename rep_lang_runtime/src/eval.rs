use pretty::RcDoc;
use std::{cmp::Ordering, collections::HashMap, iter};

use rep_lang_concrete_syntax::{sp, util::pretty::parens};
use rep_lang_core::{
    abstract_syntax::{primop_arity, Defn, Expr, Lit, Name, PrimOp, Program},
    app, lam,
};

#[derive(Clone)]
pub enum Value {
    VInt(i64),
    VBool(bool),
    VClosure(Name, Box<Expr>, TermEnv),
    VCons(Box<Thunk>, Box<Thunk>),
    VNil,
    VPair(Box<Thunk>, Box<Thunk>),
}

enum Thunk {
    UnevExpr(Box<Expr>, TermEnv),
    UnevRust(Box<FnMut() -> Value>),
    Ev(Value),
}

#[macro_export]
macro_rules! vcons {
    ( $a: expr, $b: expr ) => {
        Value::VCons(Box::new(Thunk::Ev($a)), Box::new(Thunk::Ev($b)))
    };
}

impl PartialEq for Value {
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

type TermEnv = HashMap<Name, Value>;

impl Value {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
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

pub fn eval_program(prog: &Program) -> (Value, TermEnv) {
    let mut env = HashMap::new();
    let mut es = EvalState::new();
    for Defn(nm, bd) in prog.p_defns.iter() {
        let val = eval_(&env, &mut es, bd);
        env.insert(nm.clone(), val);
    }
    (eval_(&env, &mut es, &prog.p_body), env)
}

pub fn eval(expr: &Expr) -> Value {
    let env = HashMap::new();
    let mut es = EvalState::new();
    eval_(&env, &mut es, expr)
}

use Value::*;
// this will have to change to `Thunk` ------------------------\/
pub fn eval_(env: &TermEnv, es: &mut EvalState, expr: &Expr) -> Value {
    match primop_apply_case(es, expr) {
        // in this case we directly interpret the PrimOp.
        PrimOpApplyCase::FullyApplied(op, args) => {
            // `actual_value` instead of `eval` for args
            let args_v: Vec<Value> = args
                .iter()
                .map(|arg| eval_(env, es, &arg.clone()))
                .collect();
            match op {
                PrimOp::Add => match (&args_v[0], &args_v[1]) {
                    (VInt(a_), VInt(b_)) => VInt(a_ + b_),
                    _ => panic!("+: bad types"),
                },
                PrimOp::Sub => match (&args_v[0], &args_v[1]) {
                    (VInt(a_), VInt(b_)) => VInt(a_ - b_),
                    _ => panic!("-: bad types"),
                },
                PrimOp::Mul => match (&args_v[0], &args_v[1]) {
                    (VInt(a_), VInt(b_)) => VInt(a_ * b_),
                    _ => panic!("*: bad types"),
                },
                PrimOp::Div => match (&args_v[0], &args_v[1]) {
                    (VInt(a_), VInt(b_)) => VInt(a_ / b_),
                    _ => panic!("*: bad types"),
                },
                PrimOp::Eql => match (&args_v[0], &args_v[1]) {
                    (VInt(a_), VInt(b_)) => VBool(a_ == b_),
                    _ => panic!("==: bad types"),
                },
                PrimOp::Null => match &args_v[0] {
                    VCons(_, _) => VBool(false),
                    VNil => VBool(true),
                    _ => panic!("null: bad types"),
                },
                PrimOp::Pair => {
                    let a = args_v[0].clone();
                    let b = args_v[1].clone();
                    VPair(Box::new(a), Box::new(b))
                }
                PrimOp::Fst => match &args_v[0] {
                    VPair(a, _) => *a.clone(),
                    _ => panic!("fst: bad types"),
                },
                PrimOp::Snd => match &args_v[0] {
                    VPair(_, b) => *b.clone(),
                    _ => panic!("snd: bad types"),
                },
                PrimOp::Cons => VCons(Box::new(args_v[0].clone()), Box::new(args_v[1].clone())),
                PrimOp::Nil => panic!("nil: application of non-function"),
            }
        }

        PrimOpApplyCase::PartiallyApplied(lam) => eval_(env, es, &lam),

        // we do not find a direct PrimOp application, so we interpret normally.
        PrimOpApplyCase::Other => match expr {
            Expr::Lit(Lit::LInt(x)) => VInt(*x),
            Expr::Lit(Lit::LBool(x)) => VBool(*x),

            Expr::Var(x) => match env.get(x) {
                None => panic!("impossible: free variable: {:?}", x),
                Some(v) => v.clone(),
            },

            Expr::Lam(nm, bd) => VClosure(nm.clone(), bd.clone(), env.clone()),

            Expr::Let(x, e, bd) => {
                let e_v = eval_(env, es, e);
                let mut new_env = env.clone();
                new_env.insert(x.clone(), e_v);
                eval_(&new_env, es, bd)
            }

            // `actual_value` instead of `eval_` for `tst`
            Expr::If(tst, thn, els) => match eval_(env, es, tst) {
                VBool(true) => eval_(env, es, thn),
                VBool(false) => eval_(env, es, els),
                _ => panic!("impossible: non-bool in test position of if"),
            },

            // we treat `Nil` here differently from the other `PrimOp`s,
            // interpreting it directly as a value (since it is not a function,
            // like all the other `PrimOp`s.
            Expr::Prim(PrimOp::Nil) => VNil,

            // this represents a PrimOp that is not in application position.
            // since it is then being used as an argument (or being bound), we
            // must package it into a closure so it can be used "lifted".
            //
            // if it is a nullary primop, we do not need to wrap it in a
            // closure, and we don't.
            Expr::Prim(op) => {
                let fresh_names: Vec<Name> = iter::repeat_with(|| es.fresh())
                    .take(primop_arity(op))
                    .collect();

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
                eval(&full_lam)
            }

            // `actual_value` instead of `eval_` for `fun`
            Expr::App(fun, arg) => match eval_(env, es, fun) {
                VClosure(nm, bd, clo) => {
                    // must `delay` `arg` -----\/
                    let arg_v = eval_(env, es, arg);
                    let mut new_env = clo;
                    new_env.insert(nm, arg_v);
                    eval_(&new_env, es, &bd)
                }
                _ => panic!("impossible: non-closure in function position of app"),
            },
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

use pretty::RcDoc;
use std::collections::HashMap;
use std::iter;

use super::syntax::{Defn, Expr, Lit, Name, PrimOp, Program};
use super::util::pretty::parens;
use crate::{app, lam, sp};

#[derive(Clone)]
pub enum Value {
    VInt(i64),
    VBool(bool),
    VClosure(Name, Box<Expr>, TermEnv),
    VList(Vec<Value>),
    VPair(Box<Value>, Box<Value>),
}

type TermEnv = HashMap<Name, Value>;

impl Value {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
            VInt(n) => RcDoc::as_string(n),
            VBool(true) => RcDoc::text("true"),
            VBool(false) => RcDoc::text("false"),
            VClosure(_, _, _) => RcDoc::text("<<closure>>"),
            VList(vec) => {
                let header = iter::once(RcDoc::text("(list"));
                let footer = RcDoc::text(")");
                let middle = vec.iter().map(|x| x.ppr());
                RcDoc::intersperse(header.chain(middle), sp!()).append(footer)
            }
            VPair(a, b) => parens(a.ppr().append(RcDoc::text(", ")).append(b.ppr())),
        }
    }
}

pub struct EvalState(u64);

impl EvalState {
    pub fn new() -> EvalState {
        EvalState(0)
    }

    fn fresh(&mut self) -> Name {
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
pub fn eval_(env: &TermEnv, es: &mut EvalState, expr: &Expr) -> Value {
    match find_prim_app(&expr, false) {
        // in this case we directly interpret the PrimOp.
        Some((op, args)) => {
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
                PrimOp::Eql => match (&args_v[0], &args_v[1]) {
                    (VInt(a_), VInt(b_)) => VBool(a_ == b_),
                    _ => panic!("==: bad types"),
                },
                PrimOp::Null => match &args_v[0] {
                    VList(vec) => VBool(vec.is_empty()),
                    _ => panic!("null: bad types"),
                },
                PrimOp::Map => match (&args_v[0], &args_v[1]) {
                    (VClosure(nm, bd, clo), VList(vec)) => {
                        let mut results = Vec::new();
                        for arg_v in vec {
                            let mut new_env = clo.clone();
                            // TODO
                            // why is this clone necessary? \|/
                            // don't we have ownership?      |
                            new_env.insert(nm.clone(), arg_v.clone());
                            results.push(eval_(&new_env, es, &bd));
                        }
                        Value::VList(results)
                    }
                    _ => panic!("map: bad types"),
                },
                PrimOp::Foldl => match (&args_v[0], &args_v[1], &args_v[2]) {
                    (VClosure(nm, bd, clo), init, VList(vec)) => {
                        let applicator = |acc: Value, arg_v: &Value| {
                            let mut new_env = clo.clone();
                            new_env.insert(nm.clone(), acc);
                            match eval_(&new_env, es, &bd) {
                                VClosure(nm2, bd2, clo2) => {
                                    let mut new_env2 = clo2.clone();
                                    new_env2.insert(nm2, arg_v.clone());
                                    eval_(&new_env2, es, &bd2)
                                }
                                _ => panic!("foldl: bad types"),
                            }
                        };
                        // TODO: why is this clone necessary?
                        vec.into_iter().fold(init.clone(), applicator)
                    }
                    _ => panic!("foldl: bad types"),
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
                PrimOp::Cons => match &args_v[1] {
                    VList(vec) => VList(
                        iter::once(&args_v[0])
                            .chain(vec.into_iter())
                            .map(|x| x.clone())
                            .collect(),
                    ),
                    _ => panic!("cons: bad types"),
                },
                PrimOp::Nil => panic!("nil: application of non-function"),
            }
        }

        // we do not find a direct PrimOp application, so we interpret normally.
        None => match expr {
            Expr::Lit(Lit::LInt(x)) => VInt(*x),
            Expr::Lit(Lit::LBool(x)) => VBool(*x),

            Expr::Var(x) => match env.get(&x) {
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

            Expr::If(tst, thn, els) => match eval_(env, es, tst) {
                VBool(true) => eval_(env, es, thn),
                VBool(false) => eval_(env, es, els),
                _ => panic!("impossible: non-bool in test position of if"),
            },

            // we treat `Nil` here differently from the other `PrimOp`s,
            // interpreting it directly as a value (since it is not a function,
            // like all the other `PrimOp`s.
            Expr::Prim(PrimOp::Nil) => VList(Vec::new()),

            // this represents a PrimOp that is not in application position.
            // since it is then being used as an argument (or being bound), we
            // must package it into a closure so it can be used "lifted".
            //
            // note that we assume these are arity-2 PrimOps - an assumption
            // which likely will not hold in the future.
            Expr::Prim(op) => {
                let nm1 = es.fresh();
                let nm2 = es.fresh();
                let bd = app!(
                    app!(Expr::Prim(op.clone()), Expr::Var(nm1.clone())),
                    Expr::Var(nm2.clone())
                );
                let inner = lam!(nm2, bd);
                VClosure(nm1, Box::new(inner), HashMap::new())
            }

            Expr::App(fun, arg) => match eval_(env, es, fun) {
                VClosure(nm, bd, clo) => {
                    let arg_v = eval_(env, es, arg);
                    let mut new_env = clo.clone();
                    new_env.insert(nm, arg_v);
                    eval_(&new_env, es, &bd)
                }
                _ => panic!("impossible: non-closure in function position of app"),
            },

            Expr::Fix(e) => eval_(
                env,
                es,
                &Expr::App(e.clone(), Box::new(Expr::Fix(e.clone()))),
            ),
        },
    }
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

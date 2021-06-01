use crate::eval::{eval_, EvalState, TermEnv, Value, Value::*};
use rep_lang_concrete_syntax::{sp, util::pretty::parens};
use rep_lang_core::{
    abstract_syntax::{primop_arity, Defn, Expr, Expr::*, Lit, Name, PrimOp, Program},
    app, lam,
};

// BK
pub fn is_ssei_able(env: &TermEnv, es: &mut EvalState, expr: &Expr) -> bool {
    match eval_(env, es, expr) {
        VClosure(nm, bd, env_clo) => true,
        _ => false,
    }
}

/// given an expr, perform beta reductions wherever possible.
pub fn beta_reduce(body: &Expr) -> Expr {
    todo!()
}

/// given a var_name, which corresponds to a closure argument name, and an expr
/// corresponding to the closure body, inline any variable which is applied to
/// var_name. we need this because otherwise we can't know if there has been a
/// fold.
pub fn inline_ssei_applications(env: &TermEnv, var_name: &Name, body: &Expr) -> Expr {
    match body {
        Var(_) => body.clone(),
        App(e1, e2) if **e2 == Var(var_name.clone()) => {
            let e1_ = inline_clo(var_name, env, e1);
            App(Box::new(e1_), e2.clone())
        }
        App(e1, e2) => {
            let e1_ = inline_ssei_applications(env, var_name, e1);
            let e2_ = inline_ssei_applications(env, var_name, e2);
            App(Box::new(e1_), Box::new(e2_))
        }
        Lam(nm, bd) => Lam(
            nm.clone(),
            Box::new(inline_ssei_applications(env, var_name, bd)),
        ),
        // if the let simply binds our var_name to a new name, we can eliminate the
        // let and perform substitutions inside the body.
        Let(nm, e, bd) if **e == Var(var_name.clone()) => {
            inline_ssei_applications(env, var_name, &subst_var(nm, var_name, &bd))
        }
        Let(nm, e, bd) => {
            let e_ = inline_ssei_applications(env, var_name, e);
            let bd_ = inline_ssei_applications(env, var_name, bd);
            Let(nm.clone(), Box::new(e_), Box::new(bd_))
        }
        Lit(_) => body.clone(),
        If(tst, thn, els) => {
            let tst_ = inline_ssei_applications(env, var_name, tst);
            let thn_ = inline_ssei_applications(env, var_name, thn);
            let els_ = inline_ssei_applications(env, var_name, els);
            If(Box::new(tst_), Box::new(thn_), Box::new(els_))
        }
        Prim(PrimOp) => body.clone(),
    }
}

pub fn subst_var(old_name: &Name, new_name: &Name, expr: &Expr) -> Expr {
    match expr {
        Var(name) if name == old_name => Var(new_name.clone()),
        Var(_) | Prim(_) | Lit(_) => expr.clone(),

        App(e1, e2) => {
            let e1_ = subst_var(old_name, new_name, e1);
            let e2_ = subst_var(old_name, new_name, e2);
            App(Box::new(e1_), Box::new(e2_))
        }
        Lam(nm, bd) => Lam(nm.clone(), Box::new(subst_var(old_name, new_name, bd))),
        Let(nm, e, bd) => {
            let e_ = subst_var(old_name, new_name, e);
            let bd_ = subst_var(old_name, new_name, bd);
            Let(nm.clone(), Box::new(e_), Box::new(bd_))
        }
        If(tst, thn, els) => {
            let tst_ = subst_var(old_name, new_name, tst);
            let thn_ = subst_var(old_name, new_name, thn);
            let els_ = subst_var(old_name, new_name, els);
            If(Box::new(tst_), Box::new(thn_), Box::new(els_))
        }
    }
}

// problem - how do we inline a closure? it can't have any free variables in the environment...
pub fn inline_clo(var_name: &Name, env: &TermEnv, expr: &Expr) -> Expr {
    match expr {
        Var(nm_clo) => match env.get(&nm_clo) {
            Some(VClosure(nm_arg, bd, env_clo)) if env_clo.is_empty() => {
                subst_var(nm_arg, var_name, bd)
            }
            Some(_) => expr.clone(),
            None => panic!("inline_clo: impossible: clo name not in env"),
        },
        _ => expr.clone(),
    }
}

pub fn ssei(env: &TermEnv, es: &mut EvalState, expr: &Expr) -> Option<Expr> {
    match eval_(env, es, expr) {
        VClosure(nm, bd, env_clo) => Some(inline_ssei_applications(&env, &nm, &bd)),
        _ => None,
    }
}
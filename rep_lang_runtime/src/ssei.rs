use crate::eval::{eval_, expr_free_vars, EvalState, TermEnv, Value, Value::*};
use rep_lang_concrete_syntax::{
    sp,
    util::pretty::{parens, to_pretty},
};
use rep_lang_core::{
    abstract_syntax::{primop_arity, Defn, Expr, Expr::*, Lit, Name, PrimOp, Program},
    app, lam,
};

pub fn is_ssei_able(env: &TermEnv, es: &mut EvalState, expr: &Expr) -> bool {
    match eval_(env, es, expr) {
        VClosure(nm, bd, env_clo) => true,
        _ => false,
    }
}

/// given an expr, perform beta reductions wherever possible.
pub fn beta_reduce(body: &Expr) -> Expr {
    match body {
        Lam(nm, bd) => Lam(nm.clone(), Box::new(beta_reduce(bd))),

        App(e1, e2) => match &**e1 {
            Lam(nm, bd) => beta_reduce(&subst_var(&nm, e2, &bd)),
            _ => {
                let e1_ = beta_reduce(e1);
                let e2_ = beta_reduce(e2);
                App(Box::new(e1_), Box::new(e2_))
            }
        },

        Let(nm, e, bd) => {
            let e_ = beta_reduce(e);
            beta_reduce(&subst_var(nm, &e_, &bd))
        }

        If(tst, thn, els) => {
            let tst_ = beta_reduce(tst);
            let thn_ = beta_reduce(thn);
            let els_ = beta_reduce(els);
            If(Box::new(tst_), Box::new(thn_), Box::new(els_))
        }

        _ => body.clone(),
    }
}

/// given a var_name, which corresponds to a closure argument name, and an expr
/// corresponding to the closure body, inline any variable which is applied to
/// var_name. we need this because otherwise we can't know if there has been a
/// fold.
pub fn inline_ssei_applications(env: &TermEnv, var_name: &Name, body: &Expr) -> Expr {
    match body {
        Var(_) => body.clone(),
        App(e1, e2) => match &**e1 {
            Var(clo_name) if **e2 == Var(var_name.clone()) => {
                let e1_ = inline_clo(var_name, env, clo_name);
                inline_ssei_applications(env, var_name, &App(Box::new(e1_), e2.clone()))
            }
            // var de-aliasing: if our SSEI variable gets trivially renamed, de-alias it.
            Lam(nm, bd) if **e2 == Var(var_name.clone()) => {
                inline_ssei_applications(env, var_name, &subst_var(nm, &Var(var_name.clone()), &bd))
            }
            _ => {
                let e1_ = inline_ssei_applications(env, var_name, e1);
                let e2_ = inline_ssei_applications(env, var_name, e2);
                App(Box::new(e1_), Box::new(e2_))
            }
        },
        Lam(nm, bd) => Lam(
            nm.clone(),
            Box::new(inline_ssei_applications(env, var_name, bd)),
        ),
        // var de-aliasing: if our SSEI variable gets trivially renamed, de-alias it.
        Let(nm, e, bd) if **e == Var(var_name.clone()) => {
            inline_ssei_applications(env, var_name, &subst_var(nm, &Var(var_name.clone()), &bd))
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

pub fn subst_var(name: &Name, binding: &Expr, expr: &Expr) -> Expr {
    match expr {
        Var(nm) if nm == name => binding.clone(),
        Var(_) | Prim(_) | Lit(_) => expr.clone(),

        App(e1, e2) => {
            let e1_ = subst_var(name, binding, e1);
            let e2_ = subst_var(name, binding, e2);
            App(Box::new(e1_), Box::new(e2_))
        }
        Lam(nm, bd) => Lam(nm.clone(), Box::new(subst_var(name, binding, bd))),
        Let(nm, e, bd) => {
            let e_ = subst_var(name, binding, e);
            let bd_ = subst_var(name, binding, bd);
            Let(nm.clone(), Box::new(e_), Box::new(bd_))
        }
        If(tst, thn, els) => {
            let tst_ = subst_var(name, binding, tst);
            let thn_ = subst_var(name, binding, thn);
            let els_ = subst_var(name, binding, els);
            If(Box::new(tst_), Box::new(thn_), Box::new(els_))
        }
    }
}

/// note: we can only inline closures which have no free variables
pub fn inline_clo(var_name: &Name, env: &TermEnv, clo_name: &Name) -> Expr {
    match env.get(&clo_name) {
        Some(VClosure(nm_arg, bd, env_clo)) => {
            let free_vars = expr_free_vars(bd, vec![nm_arg.clone()].into_iter().collect());
            if free_vars.is_empty() {
                subst_var(nm_arg, &Var(var_name.clone()), bd)
            } else {
                panic!("inline_clo: clo body contains free vars: {:?}", free_vars)
            }
        }
        Some(x) => panic!(
            "inline_clo: impossible: clo was not a clo: {}",
            to_pretty(x.ppr(), 80)
        ),
        None => panic!("inline_clo: impossible: clo name not in env"),
    }
}

pub fn fold_lifter(expr: &Expr, env: &TermEnv) -> (Expr, Vec<(Name, Expr)>) {
    todo!()
}

pub enum SseiResult {
    Atom(Value),
    RegularClo(Value),
    SseiClo(Value, Vec<(Name, Expr)>),
}

pub fn ssei_render(env: &TermEnv, es: &mut EvalState, expr: &Expr) -> Option<Expr> {
    match eval_(env, es, expr) {
        VClosure(nm, bd, env_clo) => Some(inline_ssei_applications(&env, &nm, &bd)),
        _ => None,
    }
}

pub fn ssei(env: &TermEnv, es: &mut EvalState, expr: &Expr) -> SseiResult {
    match eval_(env, es, expr) {
        VClosure(nm, bd, env_clo) => {
            let inlined_bd = inline_ssei_applications(&env, &nm, &bd);
            let (subbed_bd, sub_exprs) = fold_lifter(&inlined_bd, &env_clo);
            if sub_exprs.is_empty() {
                let clo = VClosure(nm.clone(), bd, env_clo);
                SseiResult::RegularClo(clo)
            } else {
                let clo = VClosure(nm.clone(), Box::new(subbed_bd), env_clo);
                SseiResult::SseiClo(clo, sub_exprs)
            }
        }
        x => SseiResult::Atom(x),
    }
}

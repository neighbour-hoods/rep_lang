use rep_lang_parser::syntax::{Expr, Name};

use super::{
    eval::Value,
    infer::{run_solve, Constraint, InferState, TypeError},
    types,
};

pub enum ValueInferenceError {
    TyErr(TypeError),
    ClosureError(Name, Box<Expr>),
}

pub fn infer_value(is: &mut InferState, value: &Value) -> Result<types::Type, ValueInferenceError> {
    let (ty, csts) = infer_value_internal(is, value)?;

    let subst = run_solve(csts).map_err(|x| ValueInferenceError::TyErr(x))?;
    Ok(ty.apply(&subst))
}

fn infer_value_internal(
    is: &mut InferState,
    value: &Value,
) -> Result<(types::Type, Vec<Constraint>), ValueInferenceError> {
    match value {
        Value::VInt(_) => Ok((types::type_int(), vec![])),
        Value::VBool(_) => Ok((types::type_bool(), vec![])),
        Value::VClosure(name, expr, _env) => Err(ValueInferenceError::ClosureError(
            name.clone(),
            expr.clone(),
        )),
        // VClosure(Name, Box<Expr>, TermEnv),
        // type TermEnv = HashMap<Name, Value>;
        //
        // fn infer(
        //     env: &Env,
        //     is: &mut InferState,
        //     expr: &Expr,
        // ) -> Result<(Type, Vec<Constraint>), TypeError> {
        //
        // pub struct Env(HashMap<Name, Scheme>);
        //
        // if we can map `infer_value` over the `TermEnv`, we can marshall that into the `env`
        // we provide to `infer`.
        //
        // we may need to convert it to a Scheme kinda like so:
        // let tvars: Vec<types::TV> = free_type_vars(new_ty).collect();
        // Ok(types::Scheme(tvars, new_ty))
        Value::VList(ls) => {
            let t_list = is.fresh();
            let t_element = is.fresh();
            let mut csts = Vec::new();
            for element in ls {
                let (elem_ty, mut elem_csts) = infer_value_internal(is, element)?;
                csts.append(&mut elem_csts);
                let cst = Constraint(elem_ty, t_element.clone());
                csts.push(cst);
            }
            csts.push(Constraint(t_list.clone(), types::type_list(t_element)));
            Ok((t_list, csts))
        }
        Value::VPair(p1, p2) => {
            let (t1, mut csts1) = infer_value_internal(is, p1)?;
            let (t2, mut csts2) = infer_value_internal(is, p2)?;
            csts1.append(&mut csts2);
            let tv = is.fresh();
            csts1.push(Constraint(tv.clone(), types::type_pair(t1, t2)));
            Ok((tv, csts1))
        }
    }
}

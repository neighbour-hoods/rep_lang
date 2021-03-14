use super::{
    eval::Value,
    infer::{run_solve, Constraint, InferState, TypeError},
    types,
};

pub fn infer_value(is: &mut InferState, value: &Value) -> Result<types::Type, TypeError> {
    let (ty, csts) = infer_value_internal(is, value)?;
    let subst = run_solve(csts)?;
    Ok(ty.apply(&subst))
}

fn infer_value_internal(
    is: &mut InferState,
    value: &Value,
) -> Result<(types::Type, Vec<Constraint>), TypeError> {
    match value {
        Value::VInt(_) => Ok((types::type_int(), vec![])),
        Value::VBool(_) => Ok((types::type_bool(), vec![])),
        Value::VClosure(_name, _expr, _env) => todo!(),
        Value::VList(_ls) => {
            let tv = is.fresh();
            todo!();
        }
        Value::VPair(p1, p2) => {
            let (t1, mut csts1) = infer_value_internal(is, p1)?;
            let (t2, mut csts2) = infer_value_internal(is, p2)?;
            csts1.append(&mut csts2);
            Ok((types::type_pair(t1, t2), csts1))
        }
    }
}

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

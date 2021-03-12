use super::{eval::Value, types};

// we will need an evaluation state so we can create fresh TVars
pub fn infer_value(value: &Value) -> types::Type {
    match value {
        Value::VInt(_) => types::type_int(),
        Value::VBool(_) => types::type_bool(),
        Value::VClosure(_name, _expr, _env) => todo!(),
        Value::VList(_ls) => todo!(),
        Value::VPair(p1, p2) => {
            let t1 = infer_value(p1);
            let t2 = infer_value(p2);
            types::type_pair(t1, t2)
        }
    }
}

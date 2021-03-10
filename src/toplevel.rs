use super::{syntax, types, eval};

/// throws an error if the document doesn’t pass type checking
fn parse_calculation(dsl_document: String) -> Result<String, syntax::Expr> {
    todo!()
}

fn get_calculation_input_type(calculation: syntax::Expr) -> types::Type {
    todo!()
}

fn get_calculation_return_type(calculation: syntax::Expr) -> types::Type {
    // types::Type::TRelated(op, Type, Type) needs to be implemented to support calculated / derived units
    todo!()
}

struct ReputationCalculationResult {
    rcr_calculation: syntax::Expr,
    ty: types::Type, // type is a reserved keyword of Rust
    value: eval::Value,
}

fn reduce_calculation<R>(calculation: syntax::Expr, input_data: &dyn Iterator<Item = eval::Value>) -> ReputationCalculationResult {
    todo!()
}

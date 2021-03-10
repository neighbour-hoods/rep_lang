use combine::parser::Parser;
use combine::stream::easy;

use super::{syntax, types, eval, parse::program};

/// throws an error if the document doesn’t pass type checking
fn parse_calculation(dsl_document: String) -> Result<syntax::Program, String> {
    match program().parse(easy::Stream(&dsl_document[..])) {
        // TODO this is janky - perhaps we can just return the error?
        // https://docs.rs/combine/4.5.2/combine/trait.StreamOnce.html#associatedtype.Error
        Err(err) => Err(format!("parse error: {}", err)),
        Ok((prog, _extra_input)) => Ok(prog),
    }
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
use combine::parser::Parser;
use combine::stream::easy;

use super::{
    syntax, types, eval, parse::program, env::Env, infer::infer_program};

/// throws an error if the document doesn’t pass type checking
pub fn parse_calculation(dsl_document: String) -> Result<syntax::Program, String> {
    match program().parse(easy::Stream(&dsl_document[..])) {
        // TODO this is janky - perhaps we can just return the error?
        // https://docs.rs/combine/4.5.2/combine/trait.StreamOnce.html#associatedtype.Error
        Err(err) => Err(format!("parse error: {}", err)),
        Ok((prog, _extra_input)) => Ok(prog),
    }
}

/// return the "scheme" of the body of a program. this may have free type variables in it.
/// the `Type` contained within can be fed to `type_arguments` & `type_return`.
// types::Type::TRelated(op, Type, Type) needs to be implemented to support calculated / derived units
pub fn get_calculation_type(program: syntax::Program) -> Result<types::Scheme, String> {
    match infer_program(Env::new(), &program) {
        Ok((sc, _env)) => Ok(sc),
        Err(err) => Err(format!("type error: {:?}", err)),
    }
}

pub struct ReputationCalculationResult {
    pub rcr_calculation: syntax::Expr,
    pub ty: types::Type, // type is a reserved keyword of Rust
    pub value: eval::Value,
}

pub fn reduce_calculation<R>(calculation: syntax::Expr, input_data: &dyn Iterator<Item = eval::Value>) -> ReputationCalculationResult {
    todo!()
}

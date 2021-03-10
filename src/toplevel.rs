use combine::parser::Parser;
use combine::stream::easy;
use std::collections::HashMap;

use super::{env::Env, eval, infer::infer_program, parse::program, syntax, types};

// META TODO:
// uses of `Result<_, String>` are unprincipled. it's better to return a specific error type.

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

pub struct ReputationCalculationOutput {
    pub rcr_calculation: syntax::Expr,
    pub ty: types::Type, // type is a reserved keyword of Rust
    pub value: eval::Value,
}

pub fn reduce_calculation(
    prog: syntax::Program,
    input_data: &mut dyn Iterator<Item = eval::Value>,
) -> Result<ReputationCalculationOutput, String> {
    // lifted from `eval_program`
    let mut env = HashMap::new();
    let mut es = eval::EvalState::new();
    for syntax::Defn(nm, bd) in prog.p_defns.iter() {
        let val = eval::eval_(&env, &mut es, bd);
        env.insert(nm.clone(), val);
    }

    let paired_name_vals: Vec<(syntax::Name, eval::Value)> =
        input_data.map(|val| (es.fresh(), val)).collect();

    // strategy:
    //
    // conjure up fresh names for the provided `Values` (from the Iterator) using
    // `EvalState::fresh`, if there are any.
    //
    // match the arity of the program body with the # of `Value`s. if mismatch, throw error.
    //
    // if arity matches, then check the types match.
    //
    // if they do, then wrap the body expr in a (potentially series of) applications which
    // apply it to the successive fresh names.
    //
    // bind the freshnames to the values in the TermEnv.
    //
    // lift most of eval::eval_program in order to evaluate the Defns (and be sure to thread state
    // appropriately).
    //
    // evaluate the program body with the set-up TermEnv and EvalState.

    let body_val = eval::eval_(&env, &mut es, &prog.p_body);
    todo!()
}

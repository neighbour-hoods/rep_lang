use combine::parser::Parser;
use combine::stream::easy;
use std::collections::HashMap;

use super::{
    env::Env,
    eval,
    infer::{infer_program, unify_many, TypeError},
    parse::program,
    syntax, types, types_values,
};

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
    pub scheme: types::Scheme,
    pub value: eval::Value,
}

pub enum ReputationCalculationError {
    // format!("arity mismatch: program body: {}; value iterator: {}", body_arity, values_arity)
    ArityMismatch(usize, usize),
    ProgramTypeInferenceError(TypeError),
    ProgramValuesUnificationError(TypeError),
}

pub fn reduce_calculation(
    prog: syntax::Program,
    input_data: &mut dyn Iterator<Item = eval::Value>,
) -> Result<ReputationCalculationOutput, ReputationCalculationError> {
    // infer type of program
    let (prog_scheme, prog_env) = infer_program(Env::new(), &prog)
        .map_err(|x| ReputationCalculationError::ProgramTypeInferenceError(x))?;

    // conjure up fresh names for the provided `Values` (from the Iterator) using
    // `EvalState::fresh`, if there are any.
    let mut es = eval::EvalState::new();
    let paired_name_vals: Vec<(syntax::Name, eval::Value)> =
        input_data.map(|val| (es.fresh(), val)).collect();

    // match the arity of the program body with the # of `Value`s. if mismatch, throw error.
    let types::Scheme(_tvars, ty) = prog_scheme;
    let body_type_arguments = types::type_arguments(&ty);
    let _ = {
        let body_arity = body_type_arguments.len();
        let values_arity = paired_name_vals.len();
        if values_arity == body_arity {
            Ok(())
        } else {
            Err(ReputationCalculationError::ArityMismatch(
                body_arity,
                values_arity,
            ))
        }
    }?;

    // if arity matches, then check that the types unify.
    let values_types: Vec<types::Type> = paired_name_vals
        .into_iter()
        .map(|(_nm, val)| types_values::infer_value(&val))
        .collect();
    let _subst = unify_many(values_types, body_type_arguments)
        .map_err(|x| ReputationCalculationError::ProgramValuesUnificationError(x))?;

    // wrap the body expr in a (potentially series of) applications which apply
    // it to the successive fresh names.
    todo!();

    // evaluate the program defns
    let mut eval_env = HashMap::new();
    for syntax::Defn(nm, bd) in prog.p_defns.iter() {
        let val = eval::eval_(&eval_env, &mut es, bd);
        eval_env.insert(nm.clone(), val);
    }

    // bind the freshnames to the values in the TermEnv.
    todo!();

    // evaluate the program body with the set-up TermEnv and EvalState.
    let body_val = eval::eval_(&eval_env, &mut es, &prog.p_body);

    // package up the result
    Ok(ReputationCalculationOutput {
        rcr_calculation: prog.p_body,
        scheme: prog_scheme,
        value: body_val,
    })
}

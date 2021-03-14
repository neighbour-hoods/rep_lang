use combine::parser::Parser;
use combine::stream::easy;
use std::collections::HashMap;

use super::{
    env::Env,
    eval,
    infer::{infer_program, infer_program_with_is, unify_many, Constraint, TypeError},
    parse::program,
    syntax,
    syntax::Expr,
    types, types_values,
};
use crate::app;

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
    ValuesIterTypeError(TypeError),
}

pub fn reduce_calculation(
    prog: syntax::Program,
    input_data: &mut dyn Iterator<Item = eval::Value>,
) -> Result<ReputationCalculationOutput, ReputationCalculationError> {
    // infer type of program
    let (prog_scheme, _prog_env, ref mut is) = infer_program_with_is(Env::new(), &prog)
        .map_err(|x| ReputationCalculationError::ProgramTypeInferenceError(x))?;

    // conjure up fresh names for the provided `Values` (from the Iterator) using
    // `EvalState::fresh`, if there are any.
    let mut es = eval::EvalState::new();
    let paired_name_vals: Vec<(syntax::Name, eval::Value)> =
        input_data.map(|val| (es.fresh(), val)).collect();

    // match the arity of the program body with the # of `Value`s. if mismatch, throw error.
    let types::Scheme(_tvars, ty) = &prog_scheme;
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
    let values_types_result: Result<Vec<types::Type>, TypeError> = paired_name_vals
        .iter()
        .map(|(_nm, val)| types_values::infer_value(is, &val))
        .collect();
    let values_types =
        values_types_result.map_err(|x| ReputationCalculationError::ValuesIterTypeError(x))?;
    let subst = unify_many(values_types, body_type_arguments)
        .map_err(|x| ReputationCalculationError::ProgramValuesUnificationError(x))?;

    // wrap the body expr in a (potentially series of) applications which apply
    // it to the successive fresh names.
    let new_prog_body = {
        let mut new_body = prog.p_body.clone();
        for (name, _val) in paired_name_vals.iter() {
            new_body = app!(new_body, Expr::Var(name.clone()));
        }
        new_body
    };

    // evaluate the program defns
    let mut eval_env = HashMap::new();
    for syntax::Defn(nm, bd) in prog.p_defns.iter() {
        let val = eval::eval_(&eval_env, &mut es, bd);
        eval_env.insert(nm.clone(), val);
    }

    // bind the freshnames to the values in the TermEnv.
    for (name, val) in paired_name_vals.iter() {
        eval_env.insert(name.clone(), val.clone());
    }

    // evaluate the program body with the set-up TermEnv and EvalState.
    let body_val = eval::eval_(&eval_env, &mut es, &new_prog_body);

    // package up the result
    Ok(ReputationCalculationOutput {
        rcr_calculation: prog.p_body,
        scheme: prog_scheme.apply(&subst),
        value: body_val,
    })
}

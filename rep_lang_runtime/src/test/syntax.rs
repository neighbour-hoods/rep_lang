use quickcheck::quickcheck;

use rep_lang_core::{
    syntax::{primop_arity, PrimOp},
};

use crate::{
    infer::{infer_primop, InferState},
    types::type_arity,
};

#[quickcheck]
fn primop_arity_eql(op: PrimOp) -> bool {
    let mut is = InferState::new();
    let ty = infer_primop(&mut is, &op);
    primop_arity(&op) == type_arity(ty)
}

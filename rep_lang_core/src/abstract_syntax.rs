#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// wrapper around `String`.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub struct Name(pub String);

/// `Expr` is the core type of `rep_lang`. expressions can contain other
/// expressions, and this type defines what can be expressed in the language.
/// inspired by the lambda calculus.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub enum Expr {
    Var(Name),
    App(Box<Expr>, Box<Expr>),
    Lam(Name, Box<Expr>),
    Let(Name, Box<Expr>, Box<Expr>),
    Lit(Lit),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Fix(Box<Expr>),
    Prim(PrimOp),
}

/// a notion of how expensive it is to compute an expression. see Ethereum's
/// notion of gas for context.
pub type Gas = u64;

use Expr::*;
/// this is stubbed out with all 1s and should be made more complex for any
/// real networked model of computation.
pub fn gas_of_expr(expr: &Expr) -> Gas {
    match expr {
        Var(_) => 1,
        App(_, _) => 1,
        Lam(_, _) => 1,
        Let(_, _, _) => 1,
        Lit(_) => 1,
        If(_, _, _) => 1,
        Fix(_) => 1,
        Prim(_) => 1,
    }
}

/// construct an application of 2 exprs.
#[macro_export]
macro_rules! app {
    ( $a: expr, $b: expr ) => {
        Expr::App(Box::new($a), Box::new($b))
    };
}

/// construct a lambda.
#[macro_export]
macro_rules! lam {
    ( $a: expr, $b: expr ) => {
        Expr::Lam($a, Box::new($b))
    };
}

/// literal value.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub enum Lit {
    LInt(i64),
    LBool(bool),
}

/// primitive operation.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub enum PrimOp {
    Add,
    Sub,
    Mul,
    Div,
    Eql,
    Null,
    Pair,
    Fst,
    Snd,
    Cons,
    Nil,
    Head,
    Tail,
    And,
    Or,
    Not,
    Lt,
    Gt,
}

/// definition: pairing a name with an expr. this is not much-used.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub struct Defn(pub Name, pub Expr);

/// program: a list of definitions with an expr "main". this is not much-used.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub struct Program {
    pub p_defns: Vec<Defn>,
    pub p_body: Expr,
}

// helpers

pub fn primop_arity(op: &PrimOp) -> usize {
    match op {
        PrimOp::Add => 2,
        PrimOp::Sub => 2,
        PrimOp::Mul => 2,
        PrimOp::Div => 2,
        PrimOp::Eql => 2,
        PrimOp::Null => 1,
        PrimOp::Pair => 2,
        PrimOp::Fst => 1,
        PrimOp::Snd => 1,
        PrimOp::Cons => 2,
        PrimOp::Nil => 0,
        PrimOp::Head => 1,
        PrimOp::Tail => 1,
        PrimOp::And => 2,
        PrimOp::Or => 2,
        PrimOp::Not => 1,
        PrimOp::Lt => 2,
        PrimOp::Gt => 2,
    }
}

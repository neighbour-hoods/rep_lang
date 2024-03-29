#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub struct Name(pub String);

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

pub type Gas = u64;

use Expr::*;
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

#[macro_export]
macro_rules! app {
    ( $a: expr, $b: expr ) => {
        Expr::App(Box::new($a), Box::new($b))
    };
}

#[macro_export]
macro_rules! lam {
    ( $a: expr, $b: expr ) => {
        Expr::Lam($a, Box::new($b))
    };
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub enum Lit {
    LInt(i64),
    LBool(bool),
}

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

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "holochain_serialized_bytes", derive(SerializedBytes))]
pub struct Defn(pub Name, pub Expr);

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

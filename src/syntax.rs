#[derive(Clone, Debug, PartialEq)]
pub struct Name(pub String);

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    LInt(i64),
    LBool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimOp {
    Add,
    Sub,
    Mul,
    Eql,
}

pub struct Decl(pub String, pub Expr);

#[allow(dead_code)]
pub struct Program {
    p_decls: Vec<Decl>,
    p_body: Expr,
}

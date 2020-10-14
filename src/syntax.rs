#[derive(Clone)]
pub struct Name(pub String);

#[derive(Clone)]
pub enum Expr {
    Var(Name),
    App(Box<Expr>, Box<Expr>),
    Lam(Name, Box<Expr>),
    Let(Name, Box<Expr>, Box<Expr>),
    Lit(Lit),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Fix(Box<Expr>),
    Op(Binop, Box<Expr>, Box<Expr>),
}

#[derive(Clone)]
pub enum Lit {
    LInt(u64),
    LBool(bool),
}

#[derive(Clone)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Eql,
}

pub struct Decl(String, Expr);

pub struct Program {
    p_decls: Vec<Decl>,
    p_body: Expr,
}

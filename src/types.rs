use pretty::RcDoc;

use crate::util::pretty::parens;
use crate::sp;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Hash)]
pub struct TV(pub String);

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Type {
    TVar(TV),
    TCon(String),
    TArr(Box<Type>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Scheme(pub Vec<TV>, pub Type);

// type constructors

pub fn type_int() -> Type {
    Type::TCon("Int".to_string())
}

pub fn type_bool() -> Type {
    Type::TCon("Bool".to_string())
}

impl Scheme {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
            Scheme(tvs, ty) => {
                let quantifier = if tvs.is_empty() {
                    RcDoc::text("")
                } else {
                    let vars: Vec<RcDoc<()>> = tvs.iter().map(|tv| tv.ppr()).collect();
                    RcDoc::text("forall ").append(RcDoc::intersperse(vars, sp!())).append(RcDoc::text(". "))
                };

                quantifier.append(ty.ppr())
            }
        }
    }
}

impl TV {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
            TV(s) => RcDoc::text(s)
        }
    }
}

impl Type {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
            Type::TVar(tv) => tv.ppr(),
            Type::TCon(s) => RcDoc::text(s),
            Type::TArr(a, b) => parens(a.ppr().append(RcDoc::text(" -> ")).append(b.ppr())),
        }
    }
}

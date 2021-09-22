use pretty::RcDoc;
use serde::{Deserialize, Serialize};

use rep_lang_concrete_syntax::{sp, util::pretty::parens};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Hash, Serialize, Deserialize)]
pub struct Tv(pub String);

#[derive(Clone, Debug, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum Type {
    TVar(Tv),
    TCon(String),
    TArr(Box<Type>, Box<Type>),
    TList(Box<Type>),
    TPair(Box<Type>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Scheme(pub Vec<Tv>, pub Type);

// type constructors

pub fn type_int() -> Type {
    Type::TCon("Int".to_string())
}

pub fn type_bool() -> Type {
    Type::TCon("Bool".to_string())
}

pub fn type_list(ty: Type) -> Type {
    Type::TList(Box::new(ty))
}

pub fn type_arr(t1: Type, t2: Type) -> Type {
    Type::TArr(Box::new(t1), Box::new(t2))
}

pub fn type_pair(t1: Type, t2: Type) -> Type {
    Type::TPair(Box::new(t1), Box::new(t2))
}

pub fn type_arr_multi(args: Vec<Type>, ret: Type) -> Type {
    let applicator = |bd, arg: Type| Type::TArr(Box::new(arg), Box::new(bd));
    args.into_iter().rev().fold(ret, applicator)
}

impl Scheme {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
            Scheme(tvs, ty) => {
                let quantifier = if tvs.is_empty() {
                    RcDoc::text("")
                } else {
                    let vars: Vec<RcDoc<()>> = tvs.iter().map(|tv| tv.ppr()).collect();
                    RcDoc::text("forall ")
                        .append(RcDoc::intersperse(vars, sp!()))
                        .append(RcDoc::text(". "))
                };

                quantifier.append(ty.ppr())
            }
        }
    }
}

impl Tv {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
            Tv(s) => RcDoc::text(s),
        }
    }
}

impl Type {
    pub fn ppr(&self) -> RcDoc<()> {
        match self {
            Type::TVar(tv) => tv.ppr(),
            Type::TCon(s) => RcDoc::text(s),
            Type::TArr(a, b) => parens(a.ppr().append(RcDoc::text(" -> ")).append(b.ppr())),
            Type::TList(x) => parens(RcDoc::text("List ").append(x.ppr())),
            Type::TPair(a, b) => parens(a.ppr().append(RcDoc::text(", ")).append(b.ppr())),
        }
    }
}

// helpers

/// arrow types have arity of whatever their return type is, plus 1.
/// all others have arity 0 since they are not functions.
pub fn type_arity(ty: Type) -> usize {
    match ty {
        Type::TArr(_arg, ret) => 1 + type_arity(*ret),
        _ => 0,
    }
}

/// return the arguments to an arrow type. if the type is not an arrow type,
/// then the vector will be empty.
pub fn type_arguments(ty: &Type) -> Vec<Type> {
    let mut vec = Vec::new();
    let mut ty_ref = Some(ty);

    while ty_ref != None {
        match ty_ref {
            Some(Type::TArr(arg, ret)) => {
                vec.push(*arg.clone());
                ty_ref = Some(ret);
            }
            _ => {
                ty_ref = None;
            }
        }
    }
    vec
}

/// return the "return type" of type. for an arrow type this is the result
/// after all arguments have been applied. for all other types this is an
/// identity function.
pub fn type_return(ty: &Type) -> Type {
    match ty {
        Type::TArr(_arg, ret) => type_return(ret),
        _ => ty.clone(),
    }
}

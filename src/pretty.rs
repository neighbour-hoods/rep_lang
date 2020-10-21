use super::syntax::{Decl, Expr, Expr::*, Lit, Lit::*, Name, PrimOp, PrimOp::*};
use pretty::RcDoc;

fn parens<T>(doc: RcDoc<T>) -> RcDoc<T> {
    RcDoc::text("(").append(doc).append(RcDoc::text(")"))
}

macro_rules! sp {
    () => {
        RcDoc::text(" ")
    };
}

impl Expr {
    pub fn ppr(&self) -> RcDoc<()> {
        match &*self {
            Var(name) => name.ppr(),
            App(fun, arg) => {
                let fun_ = fun.ppr();
                let arg_ = arg.ppr();
                parens(fun_.append(sp!()).append(arg_))
            }
            Lam(nm, bd) => {
                let nm_ = nm.ppr();
                let bd_ = bd.ppr();
                parens(
                    RcDoc::text("lam [")
                        .append(nm_)
                        .append(RcDoc::text("] "))
                        .append(bd_),
                )
            }
            Let(nm, e, bd) => {
                let nm_ = nm.ppr();
                let e_ = e.ppr();
                let bd_ = bd.ppr();
                parens(
                    RcDoc::text("let ([")
                        .append(nm_)
                        .append(sp!())
                        .append(e_)
                        .append(RcDoc::text("]) "))
                        .append(bd_),
                )
            }
            Lit(x) => x.ppr(),
            If(tst, thn, els) => {
                let docs = vec![RcDoc::text("if"), tst.ppr(), thn.ppr(), els.ppr()];
                parens(RcDoc::intersperse(docs, sp!()))
            }
            Fix(x) => parens(RcDoc::text("fix ").append(x.ppr())),
            Prim(op) => op.ppr(),
        }
    }
}

impl Lit {
    pub fn ppr(&self) -> RcDoc<()> {
        match *self {
            LInt(i) => RcDoc::as_string(i),
            LBool(true) => RcDoc::text("true"),
            LBool(false) => RcDoc::text("false"),
        }
    }
}

impl PrimOp {
    pub fn ppr(&self) -> RcDoc<()> {
        match *self {
            Add => RcDoc::text("+"),
            Sub => RcDoc::text("-"),
            Mul => RcDoc::text("*"),
            Eql => RcDoc::text("=="),
        }
    }
}

impl Name {
    pub fn ppr(&self) -> RcDoc<()> {
        match &*self {
            Name(s) => RcDoc::text(s),
        }
    }
}

impl Decl {
    pub fn ppr(&self) -> RcDoc<()> {
        match &*self {
            Decl(nm, bd) => RcDoc::text("let ")
                .append(RcDoc::text(nm))
                .append(RcDoc::text(" = "))
                .append(bd.ppr()),
        }
    }
}

pub fn to_pretty(doc: RcDoc<()>, width: usize) -> String {
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

use super::syntax::{Binop, Binop::*, Decl, Expr, Expr::*, Lit, Lit::*, Name, Program};
use pretty::RcDoc;

fn parens_if<T>(tst: bool, doc: RcDoc<T>) -> RcDoc<T> {
    if tst {
        RcDoc::text("(").append(doc).append(RcDoc::text(")"))
    } else {
        doc
    }
}

macro_rules! sp {
    () => {
        RcDoc::text(" ")
    };
}

impl Expr {
    pub fn ppr(&self, p: u64) -> RcDoc<()> {
        match &*self {
            Var(name) => name.ppr(p),
            App(fun, arg) => {
                let tst = p > 0;
                let fun_ = parens_if(tst, fun.ppr(p + 1));
                let arg_ = arg.ppr(p);
                fun_.append(sp!()).append(arg_)
            }
            Lam(nm, bd) => {
                let nm_ = nm.ppr(p);
                let bd_ = bd.ppr(p);
                let arr = RcDoc::text(" -> ");
                RcDoc::text("\\\\").append(nm_).append(arr).append(bd_)
            }
            Let(nm, e, bd) => {
                let nm_ = nm.ppr(p);
                let e_ = e.ppr(p);
                let bd_ = bd.ppr(p);
                RcDoc::text("let ")
                    .append(nm_)
                    .append(RcDoc::text(" = "))
                    .append(e_)
                    .append(RcDoc::text(" in "))
                    .append(bd_)
            }
            _ => todo!("yap"),
        }
    }
}

impl Lit {
    pub fn ppr(&self, _: u64) -> RcDoc<()> {
        match *self {
            LInt(i) => RcDoc::as_string(i),
            LBool(true) => RcDoc::text("True"),
            LBool(false) => RcDoc::text("False"),
        }
    }
}

impl Binop {
    pub fn ppr(&self, _: u64) -> RcDoc<()> {
        match *self {
            Add => RcDoc::text("+"),
            Sub => RcDoc::text("-"),
            Mul => RcDoc::text("*"),
            Eql => RcDoc::text("=="),
        }
    }
}

impl Name {
    pub fn ppr(&self, _: u64) -> RcDoc<()> {
        match &*self {
            Name(s) => RcDoc::text(s),
        }
    }
}

pub fn to_pretty(doc: RcDoc<()>, width: usize) -> String {
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

use super::syntax::{Binop, Binop::*, Decl, Expr, Expr::*, Lit, Lit::*, Name};
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
            Lit(x) => x.ppr(p),
            If(tst, thn, els) => {
                let docs = vec![
                    RcDoc::text("if "),
                    tst.ppr(p),
                    RcDoc::text(" then "),
                    thn.ppr(p),
                    RcDoc::text(" else "),
                    els.ppr(p),
                ];
                parens_if(p > 0, RcDoc::concat(docs))
            }
            Fix(x) => {
                let doc = RcDoc::text("fix ").append(x.ppr(p));
                parens_if(p > 0, doc)
            }
            Op(op, x, y) => {
                let docs = vec![op.ppr(p), x.ppr(p), y.ppr(p)];
                parens_if(p > 0, RcDoc::intersperse(docs, sp!()))
            }
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

impl Decl {
    pub fn ppr(&self, p: u64) -> RcDoc<()> {
        match &*self {
            Decl(nm, bd) => RcDoc::text("let ")
                .append(RcDoc::text(nm))
                .append(RcDoc::text(" = "))
                .append(bd.ppr(p)),
        }
    }
}

pub fn to_pretty(doc: RcDoc<()>, width: usize) -> String {
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

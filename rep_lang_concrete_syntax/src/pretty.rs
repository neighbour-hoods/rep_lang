//! rendering abstract syntax to concrete syntax.

// TODO remember + document why these are not implemented as `ppr` trait
// instances as in `eval`.

use pretty::RcDoc;
use std::iter;

use rep_lang_core::abstract_syntax::{
    Defn, Expr, Expr::*, Lit, Lit::*, Name, PrimOp, PrimOp::*, Program,
};

use crate::sp;
use crate::util::pretty::parens;

pub fn ppr_expr(expr: &Expr) -> RcDoc<()> {
    match &*expr {
        Var(name) => ppr_name(name),
        App(fun, arg) => {
            let fun_ = ppr_expr(fun);
            let arg_ = ppr_expr(arg);
            parens(fun_.append(sp!()).append(arg_))
        }
        Lam(nm, bd) => {
            let nm_ = ppr_name(nm);
            let bd_ = ppr_expr(bd);
            parens(
                RcDoc::text("lam [")
                    .append(nm_)
                    .append(RcDoc::text("] "))
                    .append(bd_),
            )
        }
        Let(nm, e, bd) => {
            let nm_ = ppr_name(nm);
            let e_ = ppr_expr(e);
            let bd_ = ppr_expr(bd);
            parens(
                RcDoc::text("let ([")
                    .append(nm_)
                    .append(sp!())
                    .append(e_)
                    .append(RcDoc::text("]) "))
                    .append(bd_),
            )
        }
        Lit(x) => ppr_lit(x),
        If(tst, thn, els) => {
            let tst_ = ppr_expr(tst);
            let thn_ = ppr_expr(thn);
            let els_ = ppr_expr(els);
            let docs = vec![RcDoc::text("if"), tst_, thn_, els_];
            parens(RcDoc::intersperse(docs, sp!()))
        }
        Fix(x) => parens(RcDoc::text("fix ").append(ppr_expr(x))),
        Prim(op) => ppr_primop(op),
    }
}

pub fn ppr_lit(lit: &Lit) -> RcDoc<()> {
    match *lit {
        LInt(i) => RcDoc::as_string(i),
        LBool(true) => RcDoc::text("true"),
        LBool(false) => RcDoc::text("false"),
    }
}

pub fn ppr_primop(op: &PrimOp) -> RcDoc<()> {
    match *op {
        Add => RcDoc::text("+"),
        Sub => RcDoc::text("-"),
        Mul => RcDoc::text("*"),
        Div => RcDoc::text("/"),
        Eql => RcDoc::text("=="),
        And => RcDoc::text("and"),
        Or => RcDoc::text("or"),
        Not => RcDoc::text("not"),
        Lt => RcDoc::text("<"),
        Gt => RcDoc::text(">"),
        Null => RcDoc::text("null"),
        Pair => RcDoc::text("pair"),
        Fst => RcDoc::text("fst"),
        Snd => RcDoc::text("snd"),
        Cons => RcDoc::text("cons"),
        Nil => RcDoc::text("nil"),
        Head => RcDoc::text("head"),
        Tail => RcDoc::text("tail"),
    }
}

pub fn ppr_name(name: &Name) -> RcDoc<()> {
    match &*name {
        Name(s) => RcDoc::text(s),
    }
}

pub fn ppr_defn(defn: &Defn) -> RcDoc<()> {
    match &*defn {
        Defn(nm, bd) => parens(
            RcDoc::text("defn ")
                .append(ppr_name(nm))
                .append(sp!())
                .append(ppr_expr(bd)),
        ),
    }
}

pub fn ppr_program(prog: &Program) -> RcDoc<()> {
    let docs = prog
        .p_defns
        .iter()
        .map(|d| ppr_defn(d))
        .chain(iter::once(ppr_expr(&prog.p_body)));
    RcDoc::intersperse(docs, "\n\n")
}

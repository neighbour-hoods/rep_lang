//! unit tests as well as round-trip property tests.

macro_rules! check_parse_expr {
    ( $a: expr, $b: expr ) => {
        let result = expr().easy_parse(position::Stream::new(&$a[..]));
        match result {
            Ok((v, stream)) => {
                if stream.is_partial() {
                    assert!(false, "parse left unconsumed input")
                } else {
                    assert_eq!(v, $b)
                }
            }
            Err(err) => assert!(
                false,
                "parse error:\nparse input: {:?}\nexpected value: {:?}\nerror:{:?}",
                $a, $b, err
            ),
        }
    };
}

pub mod parse_unit {
    use combine::{stream::position, EasyParser, StreamOnce};

    use rep_lang_core::abstract_syntax::{Lit, *};

    use crate::{parse::*, pretty::ppr_expr, util::pretty::*};
    use Expr::*;

    fn n() -> Name {
        Name("x".to_string())
    }
    fn e0() -> Expr {
        Var(n())
    }
    fn e1() -> Expr {
        Lam(n(), Box::new(e0()))
    }
    fn e2() -> Expr {
        App(Box::new(e1()), Box::new(e1()))
    }
    fn e3() -> Expr {
        Fix(Box::new(e0()))
    }
    fn e4() -> Expr {
        If(Box::new(e0()), Box::new(e0()), Box::new(e0()))
    }
    fn e5() -> Expr {
        Var(Name("free".to_string()))
    }
    fn e6() -> Expr {
        Let(Name("v".to_string()), Box::new(e0()), Box::new(e0()))
    }

    #[test]
    fn ex0() {
        check_parse_expr!("x", e0());
    }

    #[test]
    fn ex1() {
        check_parse_expr!("(lam [x] x)", e1());
    }

    #[test]
    fn ex2() {
        check_parse_expr!("((lam [x] x) (lam [x] x))", e2());
    }

    #[test]
    fn ex3() {
        check_parse_expr!("(fix x)", e3());
    }

    #[test]
    fn ex4() {
        check_parse_expr!("(if x x x)", e4());
    }

    #[test]
    fn ex5() {
        check_parse_expr!("free", e5());
    }

    #[test]
    fn ex6() {
        check_parse_expr!("(let ([v x]) x)", e6());
    }

    #[test]
    fn ex_lit_1() {
        check_parse_expr!("1", Expr::Lit(Lit::LInt(1)));
    }

    #[test]
    fn ex_lit_2() {
        check_parse_expr!("true", Expr::Lit(Lit::LBool(true)));
    }

    #[test]
    fn ex_lit_3() {
        check_parse_expr!("-2", Expr::Lit(Lit::LInt(-2)));
    }

    #[test]
    fn ex_huh() {
        check_parse_expr!("(x x)", App(Box::new(e0()), Box::new(e0())));
    }

    #[test]
    fn ex_prim_1() {
        check_parse_expr!("+", Prim(PrimOp::Add));
    }

    #[test]
    fn ex_prim_2() {
        check_parse_expr!("-", Prim(PrimOp::Sub));
    }

    #[test]
    fn ex_prim_3() {
        check_parse_expr!("*", Prim(PrimOp::Mul));
    }

    #[test]
    fn ex_prim_4() {
        check_parse_expr!("==", Prim(PrimOp::Eql));
    }

    #[test]
    fn ex_prim_5() {
        check_parse_expr!("(x +)", App(Box::new(e0()), Box::new(Prim(PrimOp::Add))));
    }

    #[test]
    fn ex_prim_6() {
        let f1 = App(Box::new(Prim(PrimOp::Add)), Box::new(Lit(Lit::LInt(4))));
        let f2 = App(Box::new(f1), Box::new(Lit(Lit::LInt(9))));
        check_parse_expr!("((+ 4) 9)", f2);
    }

    #[test]
    fn ex_qc_discovered_0() {
        let e0 = App(Box::new(Prim(PrimOp::Sub)), Box::new(Lit(Lit::LInt(84))));
        let s = to_pretty(ppr_expr(&e0), 80);
        check_parse_expr!(&s[..], e0);
    }

    #[test]
    fn ex_qc_discovered_1() {
        let e0 = Var(Name("fixio".to_string()));
        let e1 = App(Box::new(e0), Box::new(Lit(Lit::LInt(42))));
        let s = to_pretty(ppr_expr(&e1), 80);
        check_parse_expr!(&s[..], e1);
    }

    #[test]
    fn ex_qc_discovered_2() {
        let e0 = Var(Name("letio".to_string()));
        let e1 = App(Box::new(e0), Box::new(Lit(Lit::LInt(42))));
        let s = to_pretty(ppr_expr(&e1), 80);
        check_parse_expr!(&s[..], e1);
    }

    #[test]
    fn ex_qc_discovered_3() {
        let e0 = Var(Name("lamio".to_string()));
        let e1 = App(Box::new(e0), Box::new(Lit(Lit::LInt(42))));
        let s = to_pretty(ppr_expr(&e1), 80);
        check_parse_expr!(&s[..], e1);
    }

    #[test]
    fn ex_qc_discovered_4() {
        let e0 = Var(Name("ifio".to_string()));
        let e1 = App(Box::new(e0), Box::new(Lit(Lit::LInt(42))));
        let s = to_pretty(ppr_expr(&e1), 80);
        check_parse_expr!(&s[..], e1);
    }

    #[test]
    fn ex_qc_discovered_5() {
        let e0 = Var(Name("truej".to_string()));
        let e1 = Var(Name("falsek".to_string()));
        let e2 = App(Box::new(e0), Box::new(e1));
        let s = to_pretty(ppr_expr(&e2), 80);
        check_parse_expr!(&s[..], e2);
    }
}

pub mod roundtrip {
    use combine::{stream::position, EasyParser, StreamOnce};

    use crate::{
        parse::*, pretty::ppr_expr, test_helpers::abstract_syntax::WrappedExpr, util::pretty::*,
    };

    #[quickcheck]
    fn parse_pretty_roundtrip(w_e: WrappedExpr) -> bool {
        let WrappedExpr(e) = w_e;
        let s = to_pretty(ppr_expr(&e), 80);
        let res = expr().easy_parse(position::Stream::new(&s[..]));
        match res {
            Ok((_, stream)) if !stream.is_partial() => true,
            _ => false,
        }
    }
}

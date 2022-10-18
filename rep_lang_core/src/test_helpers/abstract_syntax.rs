//! arbitrary generators for key abstract syntax types.

use quickcheck::{empty_shrinker, single_shrinker, Arbitrary, Gen};
use rand::Rng;
use std::iter;

use crate::abstract_syntax::*;

// this code is unused within this crate because it's library code. we know
// that, so we disable the warnings.
#[allow(dead_code)]
pub fn arbitrary_expr<G: Gen>(g: &mut G, reserved: &[String]) -> Expr {
    gen_expr(g, g.size(), reserved)
}

impl Arbitrary for Expr {
    fn arbitrary<G: Gen>(_g: &mut G) -> Expr {
        panic!("don't use this - use arbitrary_expr")
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Expr>> {
        match &*self {
            Expr::App(f, x) => {
                let pairs = (f.clone(), x.clone())
                    .shrink()
                    .map(|(f_, x_)| Expr::App(f_, x_));
                let fs = single_shrinker(*f.clone()).chain(f.shrink().map(|v| *v));
                let xs = single_shrinker(*x.clone()).chain(x.shrink().map(|v| *v));
                Box::new(pairs.chain(fs).chain(xs))
            }
            Expr::Lam(nm, bd) => {
                let nm_ = nm.clone();
                let chain = bd
                    .clone()
                    .shrink()
                    .map(move |bd_| Expr::Lam(nm_.clone(), bd_));
                let bds = single_shrinker(*bd.clone()).chain(bd.shrink().map(|v| *v));
                Box::new(chain.chain(bds))
            }
            Expr::Let(nm, e, bd) => {
                let nm_ = nm.clone();
                let pairs = (e.clone(), bd.clone())
                    .shrink()
                    .map(move |(e_, bd_)| Expr::Let(nm_.clone(), e_, bd_));
                let es = single_shrinker(*e.clone()).chain(e.shrink().map(|v| *v));
                let bds = single_shrinker(*bd.clone()).chain(bd.shrink().map(|v| *v));
                Box::new(pairs.chain(es).chain(bds))
            }
            Expr::If(tst, thn, els) => {
                let pairs = (tst.clone(), thn.clone(), els.clone())
                    .shrink()
                    .map(|(tst_, thn_, els_)| Expr::If(tst_, thn_, els_));
                let tsts = single_shrinker(*tst.clone()).chain(tst.shrink().map(|v| *v));
                let thns = single_shrinker(*thn.clone()).chain(thn.shrink().map(|v| *v));
                let elss = single_shrinker(*els.clone()).chain(els.shrink().map(|v| *v));
                Box::new(pairs.chain(tsts).chain(thns).chain(elss))
            }
            Expr::Fix(bd) => {
                let chain = bd.shrink().map(Expr::Fix);
                let bds = single_shrinker(*bd.clone()).chain(bd.shrink().map(|v| *v));
                Box::new(chain.chain(bds))
            }
            Expr::Var(_) | Expr::Lit(_) | Expr::Prim(_) => empty_shrinker(),
        }
    }
}

/// I am pulling this out as a separate function because I do not see built-in infrastructure for
/// modifying size parameters, a la Haskell-Quickcheck:
/// <https://hackage.haskell.org/package/QuickCheck-2.14.1/docs/Test-QuickCheck.html#v:scale>
/// we want this because generating a recursive type without a "reduction" in the size parameter
/// for recursive calls will likely lead to unpredictably sized (potentially very large) `Expr`s.
/// by passing an explicit size parameter, we can implement this directly - dividing the size
/// parameter as we recur, and terminating when it hits a bound.
#[allow(dead_code)]
pub fn gen_expr<G: Gen>(g: &mut G, size: usize, reserved: &[String]) -> Expr {
    let upper_bound = if size < 1 { 3 } else { 8 };
    match g.gen_range(0, upper_bound) {
        0 => Expr::Var(arbitrary_name(g, reserved)),
        1 => Expr::Lit(arbitrary_lit(g)),
        2 => Expr::Prim(arbitrary_primop(g)),
        3 => {
            let f = gen_expr(g, size / 2, reserved);
            let a = gen_expr(g, size / 2, reserved);
            Expr::App(Box::new(f), Box::new(a))
        }
        4 => {
            let nm = arbitrary_name(g, reserved);
            let bd = gen_expr(g, size * 5 / 6, reserved);
            Expr::Lam(nm, Box::new(bd))
        }
        5 => {
            let nm = arbitrary_name(g, reserved);
            let e = gen_expr(g, size / 2, reserved);
            let bd = gen_expr(g, size / 2, reserved);
            Expr::Let(nm, Box::new(e), Box::new(bd))
        }
        6 => {
            let tst = gen_expr(g, size / 3, reserved);
            let thn = gen_expr(g, size / 3, reserved);
            let els = gen_expr(g, size / 3, reserved);
            Expr::If(Box::new(tst), Box::new(thn), Box::new(els))
        }
        7 => {
            let bd = gen_expr(g, size * 5 / 6, reserved);
            Expr::Fix(Box::new(bd))
        }
        _ => panic!("impossible: gen_expr: gen out of bounds"),
    }
}

#[allow(dead_code)]
pub fn arbitrary_lit<G: Gen>(g: &mut G) -> Lit {
    match g.gen_range(0, 2) {
        0 => Lit::LInt(i64::arbitrary(g)),
        1 => Lit::LBool(bool::arbitrary(g)),
        _ => panic!("impossible: Arbitrary: Lit: gen out of bounds"),
    }
}

#[allow(dead_code)]
pub fn arbitrary_name<G: Gen>(g: &mut G, reserved: &[String]) -> Name {
    let len = g.gen_range(3, 8);
    loop {
        let s0 = gen_alpha_char(g);
        let mut s: String = iter::repeat(gen_name(g)).take(len - 1).collect();
        s.insert(0, s0);
        if !reserved.contains(&s) {
            return Name(s);
        }
    }
}

pub fn arbitrary_primop<G: Gen>(g: &mut G) -> PrimOp {
    match g.gen_range(0, 17) {
        0 => PrimOp::Add,
        1 => PrimOp::Sub,
        2 => PrimOp::Mul,
        3 => PrimOp::Eql,
        4 => PrimOp::Null,
        5 => PrimOp::Pair,
        6 => PrimOp::Fst,
        7 => PrimOp::Snd,
        8 => PrimOp::Cons,
        9 => PrimOp::Nil,
        10 => PrimOp::Head,
        11 => PrimOp::Tail,
        12 => PrimOp::And,
        13 => PrimOp::Or,
        14 => PrimOp::Not,
        15 => PrimOp::Lt,
        16 => PrimOp::Gt,
        _ => panic!("impossible: Arbitrary: PrimOp: gen out of bounds"),
    }
}

impl Arbitrary for PrimOp {
    fn arbitrary<G: Gen>(g: &mut G) -> PrimOp {
        arbitrary_primop(g)
    }
}

fn gen_alpha_char<G: Gen>(g: &mut G) -> char {
    const ALPHA_CHARSET: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const RANGE: usize = ALPHA_CHARSET.len();
    let idx = g.gen_range(0, RANGE);
    ALPHA_CHARSET[idx as usize] as char
}

fn gen_name<G: Gen>(g: &mut G) -> char {
    const CHARSET: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-";
    const RANGE: usize = CHARSET.len();
    let idx = g.gen_range(0, RANGE);
    CHARSET[idx as usize] as char
}

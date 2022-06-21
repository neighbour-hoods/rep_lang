use quickcheck::{Arbitrary, Gen};

use rep_lang_core::{abstract_syntax::*, test_helpers::abstract_syntax::arbitrary_expr};

use crate::parse::reserved;

// we create these wrapped structs because we want our instances to be
// defined based on reserved concrete syntax keywords from this crate (and
// Rust disallows orphan instances).
#[derive(Clone, Debug)]
pub struct WrappedExpr(pub Expr);

impl Arbitrary for WrappedExpr {
    fn arbitrary(g: &mut Gen) -> WrappedExpr {
        WrappedExpr(arbitrary_expr(g, &reserved()))
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = WrappedExpr>> {
        let WrappedExpr(expr) = &*self;
        Box::new(expr.shrink().map(WrappedExpr))
    }
}

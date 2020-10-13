use poly::{pretty::*, syntax::*};
use Expr::*;

fn main() {
    println!("hello, poly & Rust!");

    let e = Var(Name("hi".to_string()));
    let doc = e.ppr(0);
    println!("{}", to_pretty(doc, 80));
}

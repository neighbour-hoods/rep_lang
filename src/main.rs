use poly::{pretty::*, syntax::*};
use Expr::*;

fn main() {
    println!("hello, poly & Rust!");

    let n = Name("hi".to_string());
    let e = Lam(n.clone(), Box::new(Var(n)));
    let e2 = App(Box::new(e.clone()), Box::new(e));
    let doc = e2.ppr(0);
    println!("{}", to_pretty(doc, 80));
}

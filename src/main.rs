use combine::parser::Parser;
use combine::stream::easy;
use rustyline::{error::ReadlineError, Editor};

use poly::{env::*, infer::*, parse::expr, util::pretty::to_pretty};

fn main() {
    println!("hello, poly & Rust!");

    let mut rl = Editor::<()>::new();
    let (width, _height) = match rl.dimensions() {
        None => panic!("output is not a tty"),
        Some(dims) => dims,
    };
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match expr().parse(easy::Stream(&line[..])) {
                    Err(err) => println!("parse error: {}", err),
                    Ok((e, _)) => {
                        println!("ast: {:?}\n", e);
                        let env = Env::new();
                        match infer_expr(env, e) {
                            Err(err) => println!("type error: {:?}", err),
                            Ok(sc) => {
                                println!("scheme: {}", to_pretty(sc.ppr(), width));
                            }
                        }
                    }
                };
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => {
                println!("\nbye!");
                break;
            }
            Err(err) => {
                println!("error: {:?}", err);
                break;
            }
        }
    }
}

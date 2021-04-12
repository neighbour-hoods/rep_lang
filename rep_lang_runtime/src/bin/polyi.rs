use combine::parser::Parser;
use combine::stream::easy;
use rustyline::{error::ReadlineError, Editor};
use std::collections::HashMap;

use poly::{
    env::*,
    eval::{eval_, EvalState},
    infer::*,
    parse::defn_or_it_expr,
    syntax::Defn,
    util::pretty::to_pretty,
};

const BANNER: &'static str = r#"
                 __
    ____  ____  / /_  __      __________
   / __ \/ __ \/ / / / /_____/ ___/ ___/
  / /_/ / /_/ / / /_/ /_____/ /  (__  )
 / .___/\____/_/\__, /     /_/  /____/
/_/            /____/
"#;

fn main() {
    println!("{}", BANNER);

    let mut rl = Editor::<()>::new();
    let (width, _height) = match rl.dimensions() {
        None => panic!("output is not a tty"),
        Some(dims) => dims,
    };
    let mut type_env = Env::new();
    let mut term_env = HashMap::new();
    let mut es = EvalState::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match defn_or_it_expr().parse(easy::Stream(&line[..])) {
                    Err(err) => println!("parse error: {}", err),
                    Ok((Defn(nm, e), extra_input)) => {
                        if extra_input != easy::Stream("") {
                            println!("error: unconsumed input: {:?}", extra_input);
                        } else {
                            println!("ast: {:?}\n", e);
                            match infer_expr(&type_env, &e) {
                                Err(err) => println!("type error: {:?}", err),
                                Ok(sc) => {
                                    let ty = to_pretty(sc.ppr(), width);
                                    type_env.extend(nm.clone(), sc);
                                    let val = eval_(&term_env, &mut es, &e);
                                    let val_str = to_pretty(val.ppr(), width);
                                    term_env.insert(nm, val);
                                    println!("(: {}\n   {}\n)", val_str, ty);
                                }
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

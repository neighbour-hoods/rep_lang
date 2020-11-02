use combine::parser::Parser;
use combine::stream::easy;
use rustyline::{error::ReadlineError, Editor};

use poly::{env::*, infer::*, parse::expr, util::pretty::to_pretty};

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
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match expr().parse(easy::Stream(&line[..])) {
                    Err(err) => println!("parse error: {}", err),
                    Ok((e, extra_input)) => {
                        if extra_input != easy::Stream("") {
                            println!("error: unconsumed input: {:?}", extra_input);
                        } else {
                            println!("ast: {:?}\n", e);
                            let env = Env::new();
                            match infer_expr(env, e) {
                                Err(err) => println!("type error: {:?}", err),
                                Ok(sc) => {
                                    println!("scheme: {}", to_pretty(sc.ppr(), width));
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

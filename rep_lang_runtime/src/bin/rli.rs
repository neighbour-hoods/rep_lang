use combine::{stream::position, EasyParser, StreamOnce};
use rustyline::{error::ReadlineError, Editor};

use rep_lang_concrete_syntax::{parse::defn_or_it_expr, util::pretty::to_pretty};
use rep_lang_core::abstract_syntax::Defn;

use rep_lang_runtime::{
    env::*,
    eval::{eval_, lookup_sto, new_term_env, value_to_flat_thunk, EvalState, Sto},
    infer::*,
};

const BANNER: &str = r#"
                         __
   ________  ____       / /___ _____  ____ _
  / ___/ _ \/ __ \     / / __ `/ __ \/ __ `/
 / /  /  __/ /_/ /    / / /_/ / / / / /_/ /
/_/   \___/ .___/____/_/\__,_/_/ /_/\__, /
         /_/   /_____/             /____/

"#;

fn main() {
    println!("{}", BANNER);

    let mut rl = Editor::<()>::new();
    let (width, _height) = match rl.dimensions() {
        None => panic!("output is not a tty"),
        Some(dims) => dims,
    };
    let mut type_env = Env::new();
    let mut term_env = new_term_env();
    let mut sto = Sto::new();
    let mut es = EvalState::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match defn_or_it_expr().easy_parse(position::Stream::new(&line[..])) {
                    Err(err) => println!("parse error:\n\n{}\n", err),
                    Ok((Defn(nm, e), extra_input)) => {
                        if extra_input.is_partial() {
                            println!("error: unconsumed input: {:?}", extra_input);
                        } else {
                            println!("ast: {:?}\n", e);
                            match infer_expr(&type_env, &e) {
                                Err(err) => println!("type error: {:?}", err),
                                Ok(sc) => {
                                    let ty = to_pretty(sc.ppr(), width);
                                    type_env.extend(nm.clone(), sc);
                                    let vr = eval_(&term_env, &mut sto, &mut es, &e);
                                    let val = lookup_sto(&vr, &mut sto);
                                    let result_flat_thunk = value_to_flat_thunk(&val, &mut sto);
                                    let val_str = to_pretty(result_flat_thunk.ppr(), width);
                                    term_env.insert(nm, vr);
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

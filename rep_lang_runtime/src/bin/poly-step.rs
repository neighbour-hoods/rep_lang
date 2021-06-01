use combine::parser::Parser;
use combine::stream::easy;
use std::env;
use std::fs::File;
use std::io::prelude::*;

use rep_lang_concrete_syntax::{parse::program, pretty::ppr_expr, util::pretty::to_pretty};

use poly::{
    env::Env,
    eval::eval_defns,
    infer::infer_program,
    ssei::{is_ssei_able, ssei},
};

fn main() -> std::io::Result<()> {
    let width = 80;
    let fp = get_fp()?;
    let mut file = File::open(fp)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match program().parse(easy::Stream(&contents[..])) {
        Err(err) => panic!("parse error: {}", err),
        Ok((prog, extra_input)) => {
            if extra_input != easy::Stream("") {
                panic!("error: unconsumed input: {:?}", extra_input);
            } else {
                match infer_program(Env::new(), &prog) {
                    Ok((sc, env)) => {
                        println!("{:?}\n\n{:?}\n", sc, env);
                        let ty = to_pretty(sc.ppr(), width);
                        let (mut es, env) = eval_defns(&prog);
                        println!("es: {:?}", es);
                        for (k, v) in env.iter() {
                            let val_str = to_pretty(v.ppr(), width);
                            println!("{:?} : {}", k, val_str);
                        }
                        println!(
                            "is_ssei_able: {}",
                            is_ssei_able(&env, &mut es, &prog.p_body)
                        );
                        println!(
                            "ssei: {}",
                            ssei(&env, &mut es, &prog.p_body)
                                .map_or("".into(), |expr| to_pretty(ppr_expr(&expr), width))
                        );
                        Ok(())
                    }
                    Err(err) => panic!("type error: {:?}", err),
                }
            }
        }
    }
}

fn get_fp() -> std::io::Result<String> {
    let args: Vec<String> = env::args().collect();
    match args.as_slice() {
        [_, fp] => Ok(fp.clone()),
        _ => panic!("wanted one filepath, got {:?}", &args[1..]),
    }
}

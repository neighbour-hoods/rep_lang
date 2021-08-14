use combine::{stream::position, EasyParser, StreamOnce};
use std::env;
use std::fs::File;
use std::io::prelude::*;

use rep_lang_concrete_syntax::{parse::program, util::pretty::to_pretty};

use rep_lang_runtime::{
    env::Env,
    eval::{eval_program, lookup_sto, ppr_value_ref},
    infer::infer_program,
};

fn main() -> std::io::Result<()> {
    let width = 80;
    let fp = get_fp()?;
    let mut file = File::open(fp)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    match program().easy_parse(position::Stream::new(&contents[..])) {
        Err(err) => panic!("parse error:\n\n{}\n", err),
        Ok((prog, extra_input)) => {
            if extra_input.is_partial() {
                panic!("error: unconsumed input: {:?}", extra_input);
            } else {
                // println!("{}", to_pretty(prog.ppr(), width));
                match infer_program(Env::new(), &prog) {
                    Ok((sc, env)) => {
                        println!("{:?}\n\n{:?}\n", sc, env);
                        let ty = to_pretty(sc.ppr(), width);
                        let (vr, _env, sto) = eval_program(&prog);
                        let val = lookup_sto(&vr, &sto);
                        let val_str = to_pretty(ppr_value_ref(val, &sto), width);
                        println!("sto: {:?}\nsto len: {}\n", sto.sto_vec, sto.sto_vec.len());
                        println!("(: {}\n   {}\n)", val_str, ty);
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

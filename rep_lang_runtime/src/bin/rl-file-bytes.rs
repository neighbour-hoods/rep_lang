use combine::{stream::position, EasyParser, StreamOnce};
use std::env;

use rep_lang_concrete_syntax::{parse::program, util::pretty::to_pretty};
use rep_lang_core::{
    abstract_syntax::{Expr, Name, Program},
    app,
};
use rep_lang_runtime::{
    env::Env,
    eval::{
        add_to_sto, eval_program, lookup_sto, new_term_env, value_to_flat_value, EvalState, Sto,
    },
    infer::infer_program,
    thunk_util::file_byte_to_flat_thunk_list,
};

fn main() -> std::io::Result<()> {
    let width = 80;

    let mut env = new_term_env();
    let mut es = EvalState::new();
    let mut sto = Sto::new();

    let fp = get_fp()?;
    let bytes_thnk = file_byte_to_flat_thunk_list(fp);

    let byte_thnk_vr = add_to_sto(bytes_thnk, &mut sto);
    let byte_thunk_nm = es.fresh();
    env.insert(byte_thunk_nm.clone(), byte_thnk_vr);

    let dummy_prog = r#"(defn foldl
                          (fix (lam [foldl]
                            (lam [f acc xs]
                              (if (null xs)
                                acc
                                (foldl
                                  f
                                  (f acc (head xs))
                                  (tail xs)))))))
                         (defn length (foldl (lam [acc x] (+ 1 acc)) 0))
                         1"#;
    let prog_prog = match program().easy_parse(position::Stream::new(&dummy_prog[..])) {
        Err(err) => panic!("parse error:\n\n{}\n", err),
        Ok((prog, extra_input)) => {
            if extra_input.is_partial() {
                panic!("error: unconsumed input: {:?}", extra_input);
            } else {
                match infer_program(Env::new(), &prog) {
                    Err(err) => panic!("type error: {:?}", err),
                    Ok((_sc, _infer_env)) => prog,
                }
            }
        }
    };
    let prog_expr = app!(
        Expr::Var(Name("length".to_string())),
        Expr::Var(byte_thunk_nm)
    );
    let prog_real = Program {
        p_body: prog_expr,
        ..prog_prog
    };
    let prog_vr = eval_program(&mut env, &mut sto, &mut es, &prog_real);

    let prog_val = lookup_sto(&mut es, &prog_vr, &mut sto);

    let result_flat_value = value_to_flat_value(&mut es, &prog_val, &mut sto);
    let val_str = to_pretty(result_flat_value.ppr(), width);
    println!("sto: [");
    for (idx, elem) in sto.sto_vec.iter().enumerate() {
        println!("\t{} : {:?}", idx, elem);
    }
    println!("]");
    println!("sto len: {}\n", sto.sto_vec.len());
    println!("{}\n)", val_str);
    Ok(())
}

fn get_fp() -> std::io::Result<String> {
    let args: Vec<String> = env::args().collect();
    match args.as_slice() {
        [_, fp] => Ok(fp.clone()),
        _ => panic!("wanted one filepath, got {:?}", &args[1..]),
    }
}

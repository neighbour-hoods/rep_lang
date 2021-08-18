use rep_lang_concrete_syntax::{util::pretty::to_pretty};

use rep_lang_runtime::{
    eval::{add_to_sto, lookup_sto, new_term_env, value_to_flat_thunk, EvalState, Sto},
    thunk_util::i64_vec_to_flat_thunk_list,
};

fn main() -> std::io::Result<()> {
    let width = 80;

    let mut env = new_term_env();
    let mut es = EvalState::new();
    let mut sto = Sto::new();
    let vec: Vec<i64> = vec![1,2,3,4];
    let itr_thnk = i64_vec_to_flat_thunk_list(vec);
    let vr = add_to_sto(itr_thnk, &mut sto);
    let nm = es.fresh();
    env.insert(nm, vr);
    let val = lookup_sto(&mut es, &vr, &mut sto);
    let result_flat_thunk = value_to_flat_thunk(&mut es, &val, &mut sto);
    let val_str = to_pretty(result_flat_thunk.ppr(), width);
    println!("sto: [");
    for (idx, elem) in sto.sto_vec.iter().enumerate() {
        println!("\t{} : {:?}", idx, elem);
    }
    println!("]");
    println!("sto len: {}\n", sto.sto_vec.len());
    println!("{}\n)", val_str);
    Ok(())
}

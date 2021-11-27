#[cfg(test)]
macro_rules! check_eval_expr {
    ( $str: expr, $expected_val: expr ) => {
        match expr().easy_parse(position::Stream::new(&$str[..])) {
            Err(err) => panic!("parse error:\n\n{}\n", err),
            Ok((expr, extra_input)) => {
                if extra_input.is_partial() {
                    panic!("error: unconsumed input: {:?}", extra_input);
                } else {
                    match infer_expr(&Env::new(), &expr) {
                        Ok(_sc) => {
                            let env = new_term_env();
                            let mut es = EvalState::new();
                            let mut sto = Sto::<()>::new();
                            let actual_ref = eval_(&env, &mut sto, &mut es, &expr);
                            let actual_value = lookup_sto(&mut es, &actual_ref, &mut sto);
                            let actual_flat_value =
                                value_to_flat_value(&mut es, &actual_value, &mut sto);
                            assert_eq!(
                                $expected_val, actual_flat_value,
                                "interpreted value differs from give expected value"
                            )
                        }
                        Err(err) => panic!("type error: {:?}", err),
                    }
                }
            }
        }
    };
}

#[cfg(test)]
macro_rules! test_list {
    ($( ($fn_name:ident, $str:expr, $expected_val:expr) ),+ $(,)?) => (
        $(
            #[test]
            fn $fn_name() {
                check_eval_expr!($str, $expected_val)
            }
        )*
    );
}

#[cfg(test)]
pub mod eval_unit {
    use combine::{stream::position, EasyParser, StreamOnce};

    use crate::{
        env::*,
        eval::{
            eval_, lookup_sto, new_term_env, value_to_flat_value, EvalState, FlatValue, Sto, VRef,
            Value, Value::*,
        },
        infer::*,
        vcons,
    };
    use rep_lang_concrete_syntax::parse::expr;

    fn ifv<M: Clone>(v: Value<VRef, VRef>) -> FlatValue<M> {
        value_to_flat_value(&mut EvalState::new(), &v, &mut Sto::<M>::new())
    }

    test_list![
        (ex0, "1", ifv(VInt(1))),
        (ex1, "(- (/ (* (+ 0 1) 6) 3) 2)", ifv(VInt(0))),
        (ex2, "(((lam [x] x) (lam [x] x)) 9)", ifv(VInt(9))),
        (ex3, "((lam [x] (if x 2 7)) (== 1 2))", ifv(VInt(7))),
        (
            ex4,
            r#"(let ([x (* 2 2)])
                 (cons x (cons 3 (cons 2 (cons 1 nil)))))"#,
            vcons!(
                ifv(VInt(4)),
                vcons!(
                    ifv(VInt(3)),
                    vcons!(ifv(VInt(2)), vcons!(ifv(VInt(1)), ifv(VNil)))
                )
            )
        ),
        (ex5, "true", ifv(VBool(true))),
        (
            ex6,
            r#"(let ([pr (pair 1 2)]
                     [f fst]
                     [s snd])
                 (+ (f pr)
                    (s pr)))"#,
            ifv(VInt(3))
        ),
        (ex8, "(null nil)", ifv(VBool(true))),
        (ex7, "(null (cons 1 nil))", ifv(VBool(false))),
    ];
}

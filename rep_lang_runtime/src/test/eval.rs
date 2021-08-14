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
                            let mut sto = Vec::new();
                            let actual_ref = eval(&mut sto, &expr);
                            let actual_val = lookup_sto(&actual_ref, &sto);
                            let actual_flat_val = value_to_flat_value(actual_val, &sto);
                            assert_eq!(
                                $expected_val, actual_flat_val,
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
        eval::{eval, lookup_sto, value_to_flat_value, FlatValue, Value, Value::*},
        infer::*,
        vcons,
    };

    test_list![
        (ex0, "1", FlatValue(VInt(1))),
        (ex1, "(- (/ (* (+ 0 1) 6) 3) 2)", FlatValue(VInt(0))),
        (ex2, "(((lam [x] x) (lam [x] x)) 9)", FlatValue(VInt(9))),
        (ex3, "((lam [x] (if x 2 7)) (== 1 2))", FlatValue(VInt(7))),
        (
            ex4,
            r#"(let ([x (* 2 2)])
                 (cons x (cons 3 (cons 2 (cons 1 nil)))))"#,
            vcons!(
                FlatValue(VInt(4)),
                vcons!(
                    FlatValue(VInt(3)),
                    vcons!(
                        FlatValue(VInt(2)),
                        vcons!(FlatValue(VInt(1)), FlatValue(VNil))
                    )
                )
            )
        ),
        (ex5, "true", FlatValue(VBool(true))),
        (
            ex6,
            r#"(let ([pr (pair 1 2)]
                     [f fst]
                     [s snd])
                 (+ (f pr)
                    (s pr)))"#,
            FlatValue(VInt(3))
        ),
        (ex8, "(null nil)", FlatValue(VBool(true))),
        (ex7, "(null (cons 1 nil))", FlatValue(VBool(false))),
    ];
}

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
                            assert_eq!(
                                $expected_val,
                                eval(&expr),
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

pub mod eval_unit {
    use combine::{stream::position, EasyParser, StreamOnce};

    use crate::{
        env::*,
        eval::{eval, Value::*},
        infer::*,
    };
    use rep_lang_concrete_syntax::parse::expr;

    test_list![
        (ex0, "1", VInt(1)),
        (ex1, "(foldl + 0 (list 1 2 3))", VInt(6)),
        (ex2, "(((lam [x] x) (lam [x] x)) 9)", VInt(9)),
        (ex3, "((lam [x] (if x 2 7)) (== 1 2))", VInt(7)),
        (
            ex4,
            r#"(let ([reverse
                      (lam [ls]
                        (let ([f (lam [acc x] (cons x acc))])
                          (foldl f nil ls)))
                     ])
                   (reverse (list 1 2 3 4)))"#,
            VList(vec![VInt(4), VInt(3), VInt(2), VInt(1)])
        ),
        (ex5, "true", VBool(true)),
        (
            ex6,
            r#"(let ([pr (pair 1 2)]
                     [f fst])
                 (f pr))"#,
            VInt(1)
        ),
    ];
}
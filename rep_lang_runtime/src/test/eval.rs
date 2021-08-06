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

pub mod eval_unit {
    use combine::{stream::position, EasyParser, StreamOnce};

    use crate::{
        env::*,
        eval::{eval, Value::*},
        infer::*,
    };
    use rep_lang_concrete_syntax::parse::expr;

    #[test]
    fn ex0() {
        check_eval_expr!("1", VInt(1))
    }

    #[test]
    fn ex1() {
        let expr = "(foldl + 0 (list 1 2 3))";
        check_eval_expr!(expr, VInt(6))
    }

    #[test]
    fn ex2() {
        let expr = "(((lam [x] x) (lam [x] x)) 9)";
        check_eval_expr!(expr, VInt(9))
    }

    #[test]
    fn ex3() {
        let expr = "((lam [x] (if x 2 7)) (== 1 2))";
        check_eval_expr!(expr, VInt(7))
    }

    #[test]
    fn ex4() {
        let expr = r#"(let ([reverse
                             (lam [ls]
                               (let ([f (lam [acc x] (cons x acc))])
                                 (foldl f nil ls)))
                            ])
                          (reverse (list 1 2 3 4)))"#;
        let out = VList(vec![VInt(4), VInt(3), VInt(2), VInt(1)]);
        check_eval_expr!(expr, out)
    }

    #[test]
    fn ex5() {
        check_eval_expr!("true", VBool(true))
    }
}

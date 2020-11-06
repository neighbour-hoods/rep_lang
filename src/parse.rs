use combine::error::{ParseError, StreamError};
use combine::parser::char::{alpha_num, char, digit, letter, spaces, string};
use combine::stream::{Stream, StreamErrorFor};
use combine::{attempt, between, choice, many, many1, not_followed_by, optional, parser, Parser};

use super::syntax::*;

// `impl Parser` can be used to create reusable parsers with zero overhead
pub fn expr_<Input>() -> impl Parser<Input, Output = Expr>
where
    Input: Stream<Token = char>,
    // Necessary due to rust-lang/rust#24159
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let l_bool = choice((
        res_str("true").map(|_| Lit::LBool(true)),
        (res_str("false").map(|_| Lit::LBool(false))),
    ));
    let l_int = (optional(char('-')), integer()).map(|t| {
        // TODO handle this error, even though it should be impossible
        let string: String = t.1;
        let num = string.parse::<i64>().unwrap();
        match t.0 {
            Some(_) => Lit::LInt(-num),
            None => Lit::LInt(num),
        }
    });
    let lit = choice((l_bool, l_int)).map(|v| Expr::Lit(v));

    let prim_op = choice((
        res_str("+").map(|_| PrimOp::Add),
        res_str("-").map(|_| PrimOp::Sub),
        res_str("*").map(|_| PrimOp::Mul),
        res_str("==").map(|_| PrimOp::Eql),
        attempt(res_str("null").map(|_| PrimOp::Null)),
        res_str("map").map(|_| PrimOp::Map),
        attempt(res_str("foldl").map(|_| PrimOp::Foldl)),
        res_str("pair").map(|_| PrimOp::Pair),
        res_str("fst").map(|_| PrimOp::Fst),
        res_str("snd").map(|_| PrimOp::Snd),
        res_str("cons").map(|_| PrimOp::Cons),
        res_str("nil").map(|_| PrimOp::Nil),
    ))
    .map(|v| Expr::Prim(v));

    let app = (expr(), many1::<Vec<_>, _, _>(expr())).map(|t| {
        let applicator = |fun, arg: Expr| Expr::App(Box::new(fun), Box::new(arg));
        t.1.into_iter().fold(t.0, applicator)
    });

    let lam = (
        res_str("lam"),
        lex_char('['),
        many1::<Vec<_>, _, _>(name()),
        lex_char(']'),
        expr(),
    )
        .map(|t| {
            let applicator = |bd, nm: Name| Expr::Lam(nm, Box::new(bd));
            t.2.into_iter().rev().fold(t.4, applicator)
        });

    let let_ = {
        let binder = (lex_char('['), name(), expr(), lex_char(']')).map(|t| (t.1, t.2));
        (
            res_str("let"),
            lex_char('('),
            many1::<Vec<_>, _, _>(binder),
            lex_char(')'),
            expr(),
        )
            .map(|t| {
                let applicator = |bd, (nm, e)| Expr::Let(nm, Box::new(e), Box::new(bd));
                t.2.into_iter().rev().fold(t.4, applicator)
            })
    };

    // here we introduce a special syntactic form for lists, which we desugar
    // into successive applications of `cons` to `nil`.
    let list = (res_str("list"), many::<Vec<_>, _, _>(expr())).map(|t| {
        let applicator = |ls, e| {
            let cons = Expr::Prim(PrimOp::Cons);
            let f = Expr::App(Box::new(cons.clone()), Box::new(e));
            Expr::App(Box::new(f), Box::new(ls))
        };
        t.1.into_iter()
            .rev()
            .fold(Expr::Prim(PrimOp::Nil), applicator)
    });

    let if_ = (res_str("if"), expr(), expr(), expr())
        .map(|t| Expr::If(Box::new(t.1), Box::new(t.2), Box::new(t.3)));

    let fix = (res_str("fix"), expr()).map(|t| Expr::Fix(Box::new(t.1)));

    let parenthesized = choice((
        attempt(lam),
        attempt(let_),
        attempt(list),
        attempt(if_),
        attempt(fix),
        app,
    ));

    choice((
        attempt(lit),
        attempt(prim_op),
        attempt(var()),
        between(lex_char('('), lex_char(')'), parenthesized),
    ))
    .skip(skip_spaces())
}

// As this expression parser needs to be able to call itself recursively `impl Parser` can't
// be used on its own as that would cause an infinitely large type. We can avoid this by using
// the `parser!` macro which erases the inner type and the size of that type entirely which
// lets it be used recursively.
//
// (This macro does not use `impl Trait` which means it can be used in rust < 1.26 as well to
// emulate `impl Parser`)
parser! {
    pub fn expr[Input]()(Input) -> Expr
    where [Input: Stream<Token = char>]
    {
        expr_()
    }
}

pub fn defn_<Input>() -> impl Parser<Input, Output = Defn>
where
    Input: Stream<Token = char>,
    // Necessary due to rust-lang/rust#24159
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let defn_ = (res_str("defn"), name(), expr()).map(|t| Defn(t.1, t.2));

    between(lex_char('('), lex_char(')'), defn_).skip(skip_spaces())
}

parser! {
    pub fn defn[Input]()(Input) -> Defn
    where [Input: Stream<Token = char>]
    {
        defn_()
    }
}

parser! {
    pub fn defn_or_it_expr[Input]()(Input) -> Defn
    where [Input: Stream<Token = char>]
    {
        choice(( attempt(defn()), expr().map(|e| Defn(Name("it".to_string()), e)) ))
    }
}

pub fn program_<Input>() -> impl Parser<Input, Output = Program>
where
    Input: Stream<Token = char>,
    // Necessary due to rust-lang/rust#24159
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (many(attempt(defn())), expr()).map(|t| Program {
        p_defns: t.0,
        p_body: t.1,
    })
}

parser! {
    pub fn program[Input]()(Input) -> Program
    where [Input: Stream<Token = char>]
    {
        program_()
    }
}

////////////////////////////////////////////////////////////////////////////////
// helpers
////////////////////////////////////////////////////////////////////////////////

// Creates a parser which parses a char and skips any trailing whitespace
fn lex_char<Input>(c: char) -> impl Parser<Input, Output = char>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    char(c).skip(skip_spaces())
}

// A parser which skips past whitespace.
// Since we aren't interested in knowing that our expression parser
// could have accepted additional whitespace between the tokens we also silence the error.
fn skip_spaces<Input>() -> impl Parser<Input, Output = ()>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    spaces().silent()
}

fn word<Input>() -> impl Parser<Input, Output = String>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(letter()).skip(skip_spaces())
}

fn integer<Input>() -> impl Parser<Input, Output = String>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(digit()).skip(skip_spaces())
}

fn res_str<'a, Input>(x: &'static str) -> impl Parser<Input, Output = &'a str>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string(x)
        .skip(not_followed_by(alpha_num()))
        .skip(skip_spaces())
}

pub fn reserved() -> Vec<String> {
    [
        "let", "lam", "fix", "true", "false", "if", "null", "map", "foldl", "pair", "fst", "snd",
        "cons", "defn", "list", "nil",
    ]
    .iter()
    .map(|x| x.to_string())
    .collect()
}

fn name<Input>() -> impl Parser<Input, Output = Name>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    word().and_then(move |s: String| {
        if reserved().contains(&s) {
            Err(StreamErrorFor::<Input>::unexpected_static_message(
                "reserved keyword",
            ))
        } else {
            Ok(Name(s))
        }
    })
}

fn var<Input>() -> impl Parser<Input, Output = Expr>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    name().map(Expr::Var)
}

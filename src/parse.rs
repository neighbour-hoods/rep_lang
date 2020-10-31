use combine::error::{ParseError, StreamError};
use combine::parser::char::{alpha_num, char, digit, letter, spaces, string};
use combine::stream::{Stream, StreamErrorFor};
use combine::{attempt, between, choice, many1, not_followed_by, optional, parser, Parser};

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

    let p_add = res_str("+").map(|_| PrimOp::Add);
    let p_sub = res_str("-").map(|_| PrimOp::Sub);
    let p_mul = res_str("*").map(|_| PrimOp::Mul);
    let p_eql = res_str("==").map(|_| PrimOp::Eql);
    let prim_op = choice((p_add, p_sub, p_mul, p_eql)).map(|v| Expr::Prim(v));

    let app = (expr(), expr()).map(|t| Expr::App(Box::new(t.0), Box::new(t.1)));

    let lam = (res_str("lam"), lex_char('['), name(), lex_char(']'), expr())
        .map(|t| Expr::Lam(t.2, Box::new(t.4)));

    let let_ = (
        res_str("let"),
        lex_char('('),
        lex_char('['),
        name(),
        expr(),
        lex_char(']'),
        lex_char(')'),
        expr(),
    )
        .map(|t| Expr::Let(t.3, Box::new(t.4), Box::new(t.7)));

    let if_ = (res_str("if"), expr(), expr(), expr())
        .map(|t| Expr::If(Box::new(t.1), Box::new(t.2), Box::new(t.3)));

    let fix = (res_str("fix"), expr()).map(|t| Expr::Fix(Box::new(t.1)));

    let parenthesized = choice((attempt(lam), attempt(let_), attempt(if_), attempt(fix), app));

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
    ["let", "lam", "fix", "true", "false", "if"]
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

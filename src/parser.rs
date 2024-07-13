use chumsky::{extra::Err, input::ValueInput, prelude::*};

use crate::lang::{Expr, Stmt};

// // type P<'a, T> = Parser<'a, str, T, Err<Rich<'a, char>>>;

// fn parser<'a>() -> impl Parser<'a, &'a str, Stmt, Err<Rich<'a, char>>> {

// }

use logos::Logos;

#[derive(Logos, Clone, Copy, PartialEq)]
enum Token<'a> {
    Error,

    #[token("assert")]
    Assert,
    #[token("assume")]
    Assume,
    #[token("var")]
    Var,
    #[token("fresh")]
    Fresh,

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[regex(r"[a-zA-Z][a-zA-Z0-9_]*")]
    Ident(&'a str),

    #[regex(r"[+-]?([0-9]*[.])?[0-9]+")]
    Numeric(&'a str),

    #[regex(r"[ \t\f\n]+", logos::skip)]
    Whitespace,
}


fn atom<'a, I>() -> impl Parser<'a, I, Expr, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
  select! {
    Token::Ident(s) => Expr::Var(s.to_string()),
  }
}

fn expr<'a, I>() -> impl Parser<'a, I, Expr, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
  use chumsky::pratt::*;

  atom().pratt((
    infix(left(1), just(Token::Mul), Expr::div),
    infix(left(1), just(Token::Div), Expr::div),
    infix(left(0), just(Token::Add), Expr::div),
    infix(left(0), just(Token::Sub), Expr::div),
  ))
}
// fn parser<'a, I>() -> impl Parser<'a, &'a [Token<'a>], Stmt, Err<Rich<'a, char>>>
// where
//     I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
//  {
//   just(Token::Assume)
// }
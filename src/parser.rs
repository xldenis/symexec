use chumsky::{extra::Err, input::ValueInput, prelude::*};

use crate::lang::{BasicBlock, Block, Expr, Stmt, Term, Terminator, Var};

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
    #[token("goto")]
    Goto,
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
    #[token("==")]
    EqEq,
    #[token("=")]
    Eq,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
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

fn expr<'a, I>() -> impl Parser<'a, I, Expr, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    use chumsky::pratt::*;

    recursive(|expr| {
        let atom = select! {
          Token::Ident(s) => Expr::Var(s.to_string()),
        };

        let leaf_expr = atom.or(expr.delimited_by(just(Token::LParen), just(Token::RParen)));

        leaf_expr.pratt((
            infix(left(1), just(Token::Mul), Expr::div),
            infix(left(1), just(Token::Div), Expr::div),
            infix(left(0), just(Token::Add), Expr::div),
            infix(left(0), just(Token::Sub), Expr::div),
        ))
    })
    .boxed()
}

fn ident<'a, I>() -> impl Parser<'a, I, Var, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
      Token::Ident(s) => s.to_string()
    }
}

fn stmt<'a, I>() -> impl Parser<'a, I, Stmt, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    choice((
        just(Token::Assume)
            .ignore_then(expr().delimited_by(just(Token::LParen), just(Token::RParen)))
            .map(|expr| Stmt::Assume { expr }),
        just(Token::Assert)
            .ignore_then(expr().delimited_by(just(Token::LParen), just(Token::RParen)))
            .map(|expr| Stmt::Assert { expr }),
        just(Token::Var)
            .ignore_then(ident())
            .then_ignore(just(Token::Eq))
            .then(expr())
            .map(|(nm, e)| Stmt::Assign { var: nm, expr: e }),
    ))
}

fn term<'a, I>() -> impl Parser<'a, I, Terminator, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    choice((just(Token::Goto)
        .ignore_then(
            ident().map(BasicBlock).then(
                ident()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            ),
        )
        .map(|(dest, args)| Terminator::Goto { dest, args }),))
}

fn block<'a, I>() -> impl Parser<'a, I, Block, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    ident()
        .then(
            ident()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(
            stmt()
                .separated_by(just(Token::Semi))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .then(term())
        .map(|(((nm, args), stmts), term)| Block {
            arguments: args,
            stmts,
            term,
        })
}

fn prog<'a, I>() -> impl Parser<'a, I, Vec<Block>, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
  block().repeated().collect()
}
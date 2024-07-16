use std::fmt::Display;

use chumsky::{
    extra::Err,
    input::{MapExtra, ValueInput},
    prelude::*,
};

use crate::lang::{BasicBlock, Block, Expr, ExprKind, Program, Stmt, Terminator, Var};

// // type P<'a, T> = Parser<'a, str, T, Err<Rich<'a, char>>>;

// fn parser<'a>() -> impl Parser<'a, &'a str, Stmt, Err<Rich<'a, char>>> {

// }

use logos::Logos;

#[derive(Logos, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    Error,
    #[token("assert")]
    Assert,
    #[token("assume")]
    Assume,
    #[token("goto")]
    Goto,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
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
    #[token("<")]
    Lt,
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

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Error => write!(f, "error"),
            Token::Assert => write!(f, "assert"),
            Token::Assume => write!(f, "assume"),
            Token::Goto => write!(f, "goto"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Var => write!(f, "var"),
            Token::Fresh => write!(f, "fresh"),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::EqEq => write!(f, "=="),
            Token::Eq => write!(f, "="),
            Token::Comma => write!(f, ","),
            Token::Semi => write!(f, ";"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Ident(i) => write!(f, "{i}"),
            Token::Numeric(n) => write!(f, "{n}"),
            Token::Whitespace => write!(f, " "),
            Token::Lt => write!(f, "<"),
        }
    }
}

fn expr<'a, I>() -> impl Parser<'a, I, Expr, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    use chumsky::pratt::*;

    recursive(|expr| {
        let atom = select! {
          Token::Ident(s) = e  => Expr { span: e.span() , kind: ExprKind::Var(s.to_string()) },
          Token::Numeric(n) = e => Expr { span: e.span(), kind: ExprKind::Const(n.parse().unwrap()) }
          ,
          Token::Fresh = e => Expr { span: e.span(), kind: ExprKind::Fresh } ,
        };

        let leaf_expr = atom.or(expr.delimited_by(just(Token::LParen), just(Token::RParen)));

        leaf_expr.pratt((
            infix(left(10), just(Token::Mul), |l, _, r, e : &mut MapExtra<'a, '_, _, _>| Expr {
                span: e.span(),
                kind: ExprKind::mul(l, r),
            }),
            infix(left(10), just(Token::Div), |l, _, r, e : &mut MapExtra<'a, '_, _, _>| Expr {
                span: e.span(),
                kind: ExprKind::div(l, r),
            }),
            infix(left(9), just(Token::Add), |l, _, r, e : &mut MapExtra<'a, '_, _, _>| Expr {
                span: e.span(),
                kind: ExprKind::add(l, r),
            }),
            infix(left(9), just(Token::Sub), |l, _, r, e : &mut MapExtra<'a, '_, _, _>| Expr {
                span: e.span(),
                kind: ExprKind::sub(l, r),
            }),
            infix(left(0), just(Token::Eq), |l, _, r, e : &mut MapExtra<'a, '_, _, _>| Expr {
                span: e.span(),
                kind: ExprKind::eq(l, r),
            }),
            infix(left(0), just(Token::Lt), |l, _, r, e : &mut MapExtra<'a, '_, _, _>| Expr {
                span: e.span(),
                kind: ExprKind::lt(l, r),
            }),
        ))
    })
    .boxed().labelled("expression")
}

fn ident<'a, I>() -> impl Parser<'a, I, Var, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    select! {
      Token::Ident(s) => s.to_string()
    }
    .labelled("identifier")
}

fn stmt<'a, I>() -> impl Parser<'a, I, Stmt, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    let assume = just(Token::Assume)
        .ignore_then(expr().delimited_by(just(Token::LParen), just(Token::RParen)))
        .map(|expr| Stmt::Assume { expr });
    let assert = just(Token::Assert)
        .ignore_then(expr().delimited_by(just(Token::LParen), just(Token::RParen)))
        .map(|expr| Stmt::Assert { expr });
    let assign = ident()
        .then_ignore(just(Token::Eq))
        .then(expr())
        .map(|(nm, e)| Stmt::Assign { var: nm, expr: e });

    assume.or(assert).or(assign).labelled("statement")
}

fn block_call<'a, I>() -> impl Parser<'a, I, (BasicBlock, Vec<Var>), Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
        ident()
        .map(BasicBlock)
        .then(
            ident()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .labelled("block call")

}

fn term<'a, I>() -> impl Parser<'a, I, Terminator, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{

    let goto = just(Token::Goto)
        .ignore_then(block_call())
        .map(|(dest, args)| Terminator::Goto { dest, args })
        .labelled("goto");

    let return_ = just(Token::Return)
        .ignore_then(ident())
        .map(Terminator::Return)
        .labelled("return");

    let if_ = just(Token::If)
        .ignore_then(ident())
        .then_ignore(just(Token::Then))
        .then(block_call())
        .then_ignore(just(Token::Else))
        .then(block_call())
        .map(|((i, t), e)|  Terminator::If { var: i, true_: t, false_: e});

    goto.or(return_).or(if_).labelled("terminator")
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
                .then_ignore(just(Token::Semi))
                .repeated()
                .collect::<Vec<_>>()
                .then(term())
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map(|((name, args), (stmts, term))| Block {
            name: BasicBlock(name),
            arguments: args,
            stmts,
            term,
        })
        .labelled("block")
}

pub fn prog<'a, I>() -> impl Parser<'a, I, Program, Err<Rich<'a, Token<'a>>>>
where
    I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>,
{
    block()
        .map(|b: Block| (b.name.clone(), b))
        .repeated()
        .collect::<Vec<_>>()
        .map(|blocks| Program {
            blocks: blocks.into_iter().collect(),
        })
}

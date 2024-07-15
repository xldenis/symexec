use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    path::PathBuf,
};

use chumsky::{
    input::{Input as _, Stream},
    Parser as _,
};
use clap::Parser;
use lang::{BasicBlock, Block, Expr, Stmt, Var};
use logos::Logos;
use smtlib::{backend::z3_binary::Z3Binary, Backend, Bool, SatResult, Solver};
use smtlib_lowlevel::{
    ast::{self, Identifier, QualIdentifier, Sort},
    lexicon::{Numeral, Symbol},
};

use crate::{lang::BinOp, parser::Token};

mod lang;
mod parser;

#[derive(Debug, Clone)]
struct State {
    path: Vec<Expr>,
    store: HashMap<Var, Expr>,
}

impl State {
    fn store(&self, v: &Var) -> &Expr {
        &self.store[v]
    }

    fn write_store(&mut self, v: &Var, e: Expr) {
        if let Expr::Fresh = e {
            self.store.insert(v.clone(), Expr::Var(v.clone()));
        } else {
            self.store.insert(v.clone(), e);
        }
    }

    /// Solve an expression using SMT
    /// Factor this out to be held by the engine not the state
    fn assert(&self, expr: Expr) -> bool {
        let mut solver = Solver::new(Z3Binary::new("z3").unwrap()).unwrap();

        store_to_smt(&self.store, &mut solver).unwrap();
        self.path.iter().for_each(|e| {
            solver.assert(expr_to_term(e).unwrap().into()).unwrap();
        });

        eprintln!("Asserting {expr}");

        solver.assert(expr_to_term(&expr).unwrap().into()).unwrap();
        matches!(solver.check_sat().unwrap(), SatResult::Sat)
    }
}

pub(crate) fn qual_ident(s: String, sort: Option<ast::Sort>) -> QualIdentifier {
    if let Some(sort) = sort {
        QualIdentifier::Sorted(Identifier::Simple(Symbol(s)), sort)
    } else {
        QualIdentifier::Identifier(Identifier::Simple(Symbol(s)))
    }
}

fn store_to_smt<B: Backend>(
    store: &HashMap<Var, Expr>,
    solver: &mut Solver<B>,
) -> Result<(), smtlib::Error> {
    for (v, e) in store {
        solver.run_command(&ast::Command::DeclareConst(
            Symbol(v.clone()),
            Sort::parse("Int").unwrap(),
        ))?;

        if let Some(term) = expr_to_term(&e.clone().eq(Expr::Var(v.clone()))) {
            solver.assert(term.into())?;
        }
    }
    Ok(())
}

fn expr_to_term(expr: &Expr) -> Option<smtlib_lowlevel::ast::Term> {
    use smtlib_lowlevel::ast::{SpecConstant, Term};
    let e = match expr {
        Expr::BinOp(BinOp::Add, a, b) => Term::Application(
            qual_ident("+".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        Expr::Var(v) => Term::parse(v).unwrap(),
        Expr::BinOp(BinOp::Eq, a, b) => Term::Application(
            qual_ident("=".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        Expr::BinOp(BinOp::Div, a, b) => Term::Application(
            qual_ident("div".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        Expr::BinOp(BinOp::Sub, a, b) => Term::Application(
            qual_ident("-".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        Expr::BinOp(BinOp::Mul, a, b) => Term::Application(
            qual_ident("*".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        Expr::Const(i) => Term::SpecConstant(SpecConstant::Numeral(Numeral(format!("{i}")))),
        Expr::Bool(b) => Bool::from(*b).into(),
        Expr::Fresh => return None,
    };
    Some(e)
}

type SymExec<T> = Result<Vec<T>, Err>;

struct Err {
    msg: String,
}

type Label = String;
fn eval_block(
    prog: &HashMap<BasicBlock, Block>,
    b: &Block,
    mut state: State,
) -> SymExec<(BasicBlock, State)> {
    for s in &b.stmts {
        match eval_stmt(s, state)? {
            Some(s) => state = s,
            None => return Ok(vec![]),
        }
    }

    match &b.term {
        lang::Terminator::Goto { dest, args } => {
            let dest_block = &prog[dest];
            let mut store = HashMap::new();
            for (ix, (v, e)) in state.store.into_iter().enumerate() {
                if args.contains(&v) {
                    store.insert(dest_block.arguments[ix].clone(), e);
                }
            }

            Ok(vec![(
                dest.clone(),
                State {
                    store,
                    path: state.path,
                },
            )])
        }
        lang::Terminator::If { var, true_, false_ } => todo!(),
        lang::Terminator::Return(_) => Ok(vec![]),
    }
}

macro_rules! fail {
    ($($msg:tt)*) => {
        return Err(Err { msg: format!($($msg)*)})
    };
}

fn eval_stmt(b: &Stmt, mut state: State) -> Result<Option<State>, Err> {
    match b {
        Stmt::Assert { expr } => {
            eprintln!("eval assert");
            let e = eval_expr(expr, &state)?;
            if state.assert(e) {
                Ok(Some(state))
            } else {
                fail!("assert failed")
            }
        }
        Stmt::Assume { expr } => {
            eprintln!("assuming {expr}");
            let e = eval_expr(expr, &state)?;
            if state.assert(e.clone()) {
                state.path.push(e);
                Ok(Some(state))
            } else {
                Ok(None)
            }
        }
        Stmt::Assign { var, expr } => {
            eprintln!("eval assign");
            let expr = eval_expr(expr, &state)?;
            state.write_store(var, expr);

            Ok(Some(state))
        }
    }
}

fn eval_expr(e: &Expr, state: &State) -> Result<Expr, Err> {
    match e {
        Expr::BinOp(BinOp::Add, a, b) => {
            let a = eval_expr(a, state)?;
            let b = eval_expr(b, state)?;

            match (&a, &b) {
                (Expr::Const(i), Expr::Const(j)) => Ok(Expr::Const(i + j)),
                _ => Ok(Expr::add(a, b)),
            }
        }
        Expr::Var(v) => Ok(state.store(v).clone()),
        Expr::BinOp(BinOp::Eq, a, b) => {
            let a = eval_expr(a, state)?;
            let b = eval_expr(b, state)?;

            if a == b {
                Ok(Expr::Bool(true))
            } else {
                Ok(Expr::eq(a, b))
            }
        }
        Expr::BinOp(op, a, b) => {
            let a = eval_expr(a, state)?;
            let b = eval_expr(b, state)?;

            let e = match op {
                BinOp::Add => a.add(b),
                BinOp::Div => a.div(b),
                BinOp::Mul => a.mul(b),
                BinOp::Sub => a.sub(b),
                BinOp::Eq => a.eq(b),
            };

            if let BinOp::Div = op {
                if state.assert(e.clone().eq(0.into())) {
                    fail!("divisor is 0")
                }
            }

            Ok(e)
        }
        Expr::Fresh => Ok(Expr::Fresh),
        Expr::Const(_) => Ok(e.clone()),
        Expr::Bool(_) => Ok(e.clone()),
    }
}

#[derive(clap::Parser)]
struct Options {
    #[arg(value_name = "FILE")]
    path: PathBuf,
}

fn main() -> std::io::Result<()> {
    let opts = <Options as clap::Parser>::parse();
    let file = read_to_string(opts.path)?;
    let tokens = Token::lexer(&file).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });

    let stream = Stream::from_iter(tokens).spanned((file.len()..file.len()).into());
    let parse = parser::prog().parse(stream);

    let result = match parse.into_result() {
        Ok(re) => re,
        Err(errs) => {
            use ariadne::{Color, Label, Report, ReportKind, Source};
            for err in errs {
                Report::build(ReportKind::Error, (), err.span().start)
                    .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span().into_range())
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(&file))
                    .unwrap();
            }
            return Ok(());
        }
    };

    let first_name = result[0].name.clone();
    let prog = result
        .into_iter()
        .map(|b| (b.name.clone(), b))
        .collect::<HashMap<_, _>>();

    let state = State {
        path: Default::default(),
        store: Default::default(),
    };

    println!("evaluating program");
    let mut states = vec![(first_name, state)];
    let mut visited = HashSet::new();
    while let Some(s) = states.pop() {
        if !visited.insert(s.0.clone()) {
            panic!("loop detected. setup `anyhow` crate for prettier messages");
        }
        eprintln!("eval block ");

        let res = eval_block(&prog, &prog[&s.0], s.1);
        match res {
            Err(e) => {
                eprintln!("error: {}", e.msg);
                break;
            }
            Ok(s) => {
                states.extend(s);
            }
        }
    }

    // opts.path
    println!("Done!");
    Ok(())
}

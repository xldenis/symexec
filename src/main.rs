use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    path::PathBuf,
};

use chumsky::{
    input::{Input as _, Stream},
    Parser as _,
};
use eyre::Result;
use lang::{BasicBlock, Block, Expr, ExprKind, Program, Stmt, Var};
use logos::Logos;
use smtlib::{backend::z3_binary::Z3Binary, Backend, Bool, SatResult, Solver};
use smtlib_lowlevel::{
    ast::{self, Identifier, QualIdentifier, Sort},
    lexicon::{Numeral, Symbol},
};
use tracing::instrument;

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
        if let ExprKind::Fresh = e.kind {
            self.store.insert(v.clone(), Expr::var(v.clone()));
        } else {
            self.store.insert(v.clone(), e);
        }
    }

    /// Solve an expression using SMT
    /// Factor this out to be held by the engine not the state
    #[instrument]
    fn assert(&self, expr: Expr) -> bool {
        let mut solver = Solver::new(Z3Binary::new("z3").unwrap()).unwrap();

        store_to_smt(&self.store, &mut solver).unwrap();
        self.path.iter().for_each(|e| {
            solver.assert(expr_to_term(e).unwrap().into()).unwrap();
        });

        solver.assert(expr_to_term(&expr).unwrap().into()).unwrap();
        matches!(solver.check_sat().unwrap(), SatResult::Sat)
    }

    fn assume(&mut self, expr: Expr) {
        self.path.push(expr);
    }

    fn goto(mut self, prog: &Program, dest: &BasicBlock, args: &[String]) -> Self {
        let dest_block = &prog.blocks[dest];
        let mut store = HashMap::new();
        for (ix, (v, e)) in self.store.into_iter().enumerate() {
            if args.contains(&v) {
                store.insert(dest_block.arguments[ix].clone(), e);
            }
        }

        self.store = store;
        self
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

        if let Some(term) = expr_to_term(&e.clone().eq(Expr::var(v.clone()))) {
            solver.assert(term.into())?;
        }
    }
    Ok(())
}

fn expr_to_term(expr: &Expr) -> Option<smtlib_lowlevel::ast::Term> {
    use smtlib_lowlevel::ast::{SpecConstant, Term};
    let e = match &expr.kind {
        ExprKind::BinOp(BinOp::Add, a, b) => Term::Application(
            qual_ident("+".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        ExprKind::Var(v) => Term::parse(v).unwrap(),
        ExprKind::BinOp(BinOp::Eq, a, b) => Term::Application(
            qual_ident("=".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        ExprKind::BinOp(BinOp::Lt, a, b) => Term::Application(
            qual_ident("<".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        ExprKind::BinOp(BinOp::Div, a, b) => Term::Application(
            qual_ident("div".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        ExprKind::BinOp(BinOp::Sub, a, b) => Term::Application(
            qual_ident("-".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        ExprKind::BinOp(BinOp::Mul, a, b) => Term::Application(
            qual_ident("*".into(), None),
            vec![expr_to_term(a)?, expr_to_term(b)?],
        ),
        ExprKind::Const(i) => Term::SpecConstant(SpecConstant::Numeral(Numeral(format!("{i}")))),
        ExprKind::Bool(b) => Bool::from(*b).into(),
        ExprKind::Fresh => return None,
        ExprKind::Not(e) => {
            Term::Application(qual_ident("not".into(), None), vec![expr_to_term(e)?])
        }
    };
    Some(e)
}

type M<T> = eyre::Result<SymExec<T>>;
type SymExec<T> = std::result::Result<T, Err>;

#[derive(Debug)]
struct Err {
    msg: String,
}

type Label = String;

fn eval_block(prog: &Program, b: &Block, mut state: State) -> SymExec<Vec<(BasicBlock, State)>> {
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
        lang::Terminator::If { var, true_, false_ } => {
            let expr = eval_expr(state.store(var), &state)?;

            let mut r_state = state.clone();

            state.assume(expr.clone());
            state = state.goto(prog, &true_.0, &true_.1);
            r_state.assume(expr.not());
            r_state = r_state.goto(prog, &false_.0, &false_.1);

            Ok(vec![(true_.0.clone(), state), (false_.0.clone(), r_state)])
        }
        lang::Terminator::Return(_) => Ok(vec![]),
        lang::Terminator::Dummy => todo!(),
    }
}

macro_rules! sym_fail {
    ($($msg:tt)*) => {
        return Err(Err { msg: format!($($msg)*)})
    };
}

#[instrument]
fn eval_stmt(b: &Stmt, mut state: State) -> SymExec<Option<State>> {
    match b {
        Stmt::Assert { expr } => {
            let e = eval_expr(expr, &state)?;
            if state.assert(e) {
                Ok(Some(state))
            } else {
                sym_fail!("assert failed")
            }
        }
        Stmt::Assume { expr } => {
            let e = eval_expr(expr, &state)?;
            if state.assert(e.clone()) {
                state.path.push(e);
                Ok(Some(state))
            } else {
                Ok(None)
            }
        }
        Stmt::Assign { var, expr } => {
            let expr = eval_expr(expr, &state)?;
            state.write_store(var, expr);

            Ok(Some(state))
        }
    }
}

#[instrument]
fn eval_expr(e: &Expr, state: &State) -> SymExec<Expr> {
    match &e.kind {
        ExprKind::BinOp(BinOp::Add, a, b) => {
            let a = eval_expr(a, state)?;
            let b = eval_expr(b, state)?;

            match (&a.kind, &b.kind) {
                (ExprKind::Const(i), ExprKind::Const(j)) => Ok(Expr::int(i + j)),
                _ => Ok(Expr::add(a, b)),
            }
        }
        ExprKind::Var(v) => Ok(state.store(v).clone()),
        ExprKind::BinOp(BinOp::Eq, a, b) => {
            let a = eval_expr(a, state)?;
            let b = eval_expr(b, state)?;

            if a == b {
                Ok(Expr::bool(true))
            } else {
                Ok(Expr::eq(a, b))
            }
        }
        ExprKind::BinOp(op, a, b) => {
            let a = eval_expr(a, state)?;
            let b = eval_expr(b, state)?;

            let e = match op {
                BinOp::Add => a.add(b),
                BinOp::Div => a.div(b),
                BinOp::Mul => a.mul(b),
                BinOp::Sub => a.sub(b),
                BinOp::Eq => a.eq(b),
                BinOp::Lt => a.lt(b),
            };

            if let BinOp::Div = op {
                if state.assert(e.clone().eq(Expr::int(0))) {
                    sym_fail!("divisor is 0")
                }
            }

            Ok(e)
        }
        ExprKind::Fresh => Ok(Expr::fresh()),
        ExprKind::Const(_) => Ok(e.clone()),
        ExprKind::Bool(_) => Ok(e.clone()),
        ExprKind::Not(e) => {
            let e = eval_expr(e, state)?;

            if let ExprKind::Bool(b) = e.kind {
                Ok(Expr::bool(!b))
            } else {
                Ok(e.not())
            }
        }
    }
}

#[derive(clap::Parser)]
struct Options {
    #[arg(value_name = "FILE")]
    path: PathBuf,
}

fn install_tracing() {
    use tracing_error::ErrorLayer;
    use tracing_subscriber::prelude::*;
    use tracing_subscriber::{fmt, EnvFilter};

    let fmt_layer = fmt::layer().with_target(false);
    let filter_layer = EnvFilter::try_from_default_env()
        .or_else(|_| EnvFilter::try_new("info"))
        .unwrap();

    tracing_subscriber::registry()
        .with(filter_layer)
        .with(fmt_layer)
        .with(ErrorLayer::default())
        .init();
}

fn main() -> color_eyre::eyre::Result<()> {
    install_tracing();
    color_eyre::install()?;

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

    let first_name = result.blocks.first().unwrap().0.clone();
    let prog = result;

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

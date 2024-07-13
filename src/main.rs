use std::collections::HashMap;

use lang::{BasicBlock, Block, Expr, Stmt, Term, Var};
use smtlib::{
    backend::z3_binary::Z3Binary, terms::Dynamic, Backend, Bool, Int, SatResult, Solver, Sort as _,
};
use smtlib_lowlevel::{
    ast::{self, Identifier, QualIdentifier, Sort},
    lexicon::{Numeral, Symbol},
};

use crate::lang::BinOp;

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

    /// Solve an expression using SMT
    /// Factor this out to be held by the engine not the state
    fn assert(&self, expr: Expr) -> bool {
        let mut solver = Solver::new(Z3Binary::new("z3").unwrap()).unwrap();
        store_to_smt(&self.store, &mut solver).unwrap();

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
            Sort::parse("bool").unwrap(),
        ))?;
        solver.assert(expr_to_term(e).into())?;
    }
    Ok(())
}

fn expr_to_term(expr: &Expr) -> smtlib_lowlevel::ast::Term {
    use smtlib_lowlevel::ast::{SpecConstant, Term};
    match expr {
        Expr::BinOp(BinOp::Add, a, b) => Term::Application(
            qual_ident("+".into(), None),
            vec![expr_to_term(a), expr_to_term(b)],
        ),
        Expr::Var(v) => Dynamic::from_name(v).into(),
        Expr::BinOp(BinOp::Eq, a, b) => Term::Application(
            qual_ident("=".into(), None),
            vec![expr_to_term(a), expr_to_term(b)],
        ),
        Expr::BinOp(BinOp::Div, a, b) => Term::Application(
            qual_ident("/".into(), None),
            vec![expr_to_term(a), expr_to_term(b)],
        ),
        Expr::BinOp(BinOp::Sub, a, b) => Term::Application(
            qual_ident("-".into(), None),
            vec![expr_to_term(a), expr_to_term(b)],
        ),
        Expr::BinOp(BinOp::Mul, a, b) => Term::Application(
            qual_ident("*".into(), None),
            vec![expr_to_term(a), expr_to_term(b)],
        ),
        Expr::Const(i) => Term::SpecConstant(SpecConstant::Numeral(Numeral(format!("{i}")))),
        Expr::Bool(b) => Bool::from(*b).into(),
    }
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
        lang::Terminator::Return(_) => todo!(),
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
            let e = eval_expr(expr, &state)?;
            if state.assert(e) {
                return Ok(Some(state));
            } else {
                fail!("assert failed")
            }
        }
        Stmt::Assume { expr } => {
            let e = eval_expr(expr, &state)?;
            if state.assert(e.clone()) {
                state.path.push(e);
                return Ok(Some(state));
            } else {
                Ok(None)
            }
        }
        Stmt::Assign { var, expr } => {
            let expr = eval_expr(expr, &state)?;
            if let Some(expr) = state.store.insert(var.clone(), expr) {
                fail!("{var} already assigned {expr:?}");
            }
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
        Expr::Const(_) => Ok(e.clone()),
        Expr::Bool(_) => Ok(e.clone()),
    }
}

fn main() {
    println!("Hello, world!");
}

use std::collections::HashMap;

use lang::{Block, Expr, Stmt, Term, Var};

mod lang;


#[derive(Debug, Clone)]
struct State {
    path: Vec<Term>,
    store: HashMap<Var, Expr>,
}

impl State {
    fn store(&self, v: &Var) -> &Expr {
        &self.store[v]
    }

    /// Solve an expression using SMT
    /// Factor this out to be held by the engine not the state
    fn assert(&self, expr: Expr) -> bool {
        false
    }
}

type SymExec<T> = Result<Vec<T>, Err>;

struct Err {
    msg: String,
}

fn eval_block(b : &Block, mut state: State) -> SymExec<State> {
    for s in &b.stmts {
        state = eval_stmt(s, state)?;
    }

    match &b.term {
        lang::Terminator::Goto { dest, args } => todo!(),
        lang::Terminator::If { var, true_, false_ } => todo!(),
        lang::Terminator::Return(_) => todo!(),
    }
}

fn eval_stmt(b: &Stmt, state: State) -> Result<State, Err> {
    match b {
        Stmt::Assert { expr } => todo!(),
        Stmt::Assume { expr } => todo!(),
    }
}

fn eval_expr(e: &Expr, state: &State) -> Result<Expr, Err> {
    match e {
        Expr::Add(_, _) => todo!(),
        Expr::Var(v) => Ok(state.store(v).clone()),
        Expr::Eq(_, _) => todo!(),
        Expr::Div(a, b) => {
            let a = eval_expr(a, state)?;
            let b = eval_expr(b, state)?;

            state.assert()
        }
        Expr::Const(i) => Ok(e.clone()),
    }
}


fn main() {
    println!("Hello, world!");
}

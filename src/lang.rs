use std::collections::HashMap;

type Ptr<T> = Box<T>;
type Seq<T> = Vec<T>;

type Var = String;

#[derive(Debug, Clone)]
pub(crate) enum Expr {
	Add(Ptr<Expr>, Ptr<Expr>),
	Var(Var),
	Leq(Var, Var),
	Const(i128),
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
	Assign { var: Var, expr: Expr },
}

#[derive(Debug, Clone)]
pub(crate) enum Terminator {
	Call { name: String, args: Seq<Var>, dest: BasicBlock },
	Goto { dest: BasicBlock },
	If { var: Var, true_: BasicBlock, false_: BasicBlock },
}

#[derive(Debug, Clone)]
pub(crate) struct BasicBlock(Var);

/// Basic blocks are structured in SSA with basic block arguments. 
#[derive(Debug, Clone)]
pub(crate) struct Block {
	arguments: Seq<Var>,
	stmts: Seq<Stmt>,
	term: Terminator,
}

#[derive(Debug, Clone)]
pub(crate) struct Program {
	blocks: HashMap<Var, Block>,
}

/// Pure facts
/// 
/// A `Term` is a pure, first-order formula used in path conditions and assertions.
#[derive(Debug, Clone)]
pub(crate) enum Term {
	Var(Var),
	Eq(Ptr<Term>, Ptr<Term>),
	Not(Ptr<Term>),
	Add(Ptr<Term>, Ptr<Term>),
	Lt(Ptr<Term>, Ptr<Term>),
}


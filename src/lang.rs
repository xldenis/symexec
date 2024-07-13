use std::collections::HashMap;

type Ptr<T> = Box<T>;
type Seq<T> = Vec<T>;

pub type Var = String;

#[derive(Debug, Clone)]
pub(crate) enum Expr {
	Add(Ptr<Expr>, Ptr<Expr>),
	Var(Var),
	Eq(Var, Var),
	Div(Ptr<Expr>, Ptr<Expr>),
	Const(i128),
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
	Assert { expr: Expr },
	Assume { expr: Expr },
}

#[derive(Debug, Clone)]
pub(crate) enum Terminator {
	Goto { dest: BasicBlock, args: Vec<Var> },
	If { var: Var, true_: (BasicBlock, Vec<Var>), false_: (BasicBlock, Vec<Var>) },
	Return(Var),
}

#[derive(Debug, Clone)]
pub(crate) struct BasicBlock(Var);

/// Basic blocks are structured in SSA with basic block arguments. 
#[derive(Debug, Clone)]
pub(crate) struct Block {
	pub arguments: Seq<Var>,
	pub stmts: Seq<Stmt>,
	pub term: Terminator,
}

#[derive(Debug, Clone)]
pub(crate) struct Program {
	pub blocks: HashMap<Var, Block>,
}

/// Pure FOL terms
/// This is unneeded?
#[derive(Debug, Clone)]
pub(crate) enum Term {
	Var(Var),
	Eq(Ptr<Term>, Ptr<Term>),
	Not(Ptr<Term>),
	Add(Ptr<Term>, Ptr<Term>),
	Lt(Ptr<Term>, Ptr<Term>),
}


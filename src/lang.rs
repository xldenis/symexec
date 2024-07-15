use std::collections::HashMap;

type Ptr<T> = Box<T>;
type Seq<T> = Vec<T>;

pub type Var = String;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Var(Var),
    BinOp(BinOp, Ptr<Expr>, Ptr<Expr>),
    Const(i128),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinOp {
    Add,
    Div,
    Mul,
    Sub,
    Eq,
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    Assert { expr: Expr },
    Assume { expr: Expr },
    Assign { var: Var, expr: Expr },
}

#[derive(Debug, Clone)]
pub(crate) enum Terminator {
    Goto {
        dest: BasicBlock,
        args: Vec<Var>,
    },
    If {
        var: Var,
        true_: (BasicBlock, Vec<Var>),
        false_: (BasicBlock, Vec<Var>),
    },
    Return(Var),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct BasicBlock(pub(crate) Var);

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

impl Expr {
    pub(crate) fn div(self, o: Self) -> Self {
        Expr::BinOp(BinOp::Div, Box::new(self), Box::new(o))
    }

    pub(crate) fn mul(self, o: Self) -> Self {
        Expr::BinOp(BinOp::Mul, Box::new(self), Box::new(o))
    }

    pub(crate) fn add(self, o: Self) -> Self {
        Expr::BinOp(BinOp::Add, Box::new(self), Box::new(o))
    }

    pub(crate) fn sub(self, o: Self) -> Self {
        Expr::BinOp(BinOp::Sub, Box::new(self), Box::new(o))
    }

    pub(crate) fn eq(self, o: Self) -> Self {
        Expr::BinOp(BinOp::Eq, Box::new(self), Box::new(o))
    }
}

impl From<i128> for Expr {
    fn from(value: i128) -> Self {
        Expr::Const(value)
    }
}

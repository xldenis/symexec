use std::collections::HashMap;

use pretty::{DocAllocator, Pretty};

type Ptr<T> = Box<T>;
type Seq<T> = Vec<T>;

pub type Var = String;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Var(Var),
    BinOp(BinOp, Ptr<Expr>, Ptr<Expr>),
    Const(i128),
    Bool(bool),
    /// Havoc / Any
    Fresh,
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
    pub name: BasicBlock,
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

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty(&pretty::Arena::new()).1.render_fmt(80, f)
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Expr {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            Expr::Var(v) => alloc.text(v),
            Expr::Const(i) => alloc.text(i.to_string()),
            Expr::Bool(b) => alloc.text(b.to_string()),
            Expr::Fresh => alloc.text("fresh"),
            Expr::BinOp(op, a, b) => {
                let a = a.pretty(alloc);
                let b = b.pretty(alloc);
                let op = match op {
                    BinOp::Add => "+",
                    BinOp::Div => "/",
                    BinOp::Mul => "*",
                    BinOp::Sub => "-",
                    BinOp::Eq => "==",
                };
                alloc.concat(vec![a, alloc.space(), alloc.text(op), alloc.space(), b])
            }
        }
    }
}

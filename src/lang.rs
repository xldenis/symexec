use std::ops::Index;

use chumsky::span::SimpleSpan;
use indexmap::IndexMap;
use pretty::{docs, DocAllocator, Pretty};

type Ptr<T> = Box<T>;
type Seq<T> = Vec<T>;

pub type Var = String;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ExprKind {
    Var(Var),
    BinOp(BinOp, Ptr<Expr>, Ptr<Expr>),
    Const(i128),
    Bool(bool),
    /// Havoc / Any
    Fresh,
    Not(Ptr<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Expr {
    pub kind: ExprKind,
    pub span: chumsky::span::SimpleSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinOp {
    Add,
    Div,
    Mul,
    Sub,
    Eq,
    Lt,
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
    Dummy,
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
    pub blocks: IndexMap<BasicBlock, Block>,
}

impl<'a> Index<&'a BasicBlock> for Program {
    type Output = Block;

    fn index(&self, ix: &'a BasicBlock) -> &Self::Output {
        &self.blocks[ix]
    }
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

// const DUMMY_SP : SimpleSpan = SimpleSpan::new(0, 0);

impl Expr {
    pub(crate) fn div(self, o: Expr) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::div(self, o),
        }
    }

    pub(crate) fn mul(self, o: Expr) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::mul(self, o),
        }
    }

    pub(crate) fn add(self, o: Expr) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::add(self, o),
        }
    }

    pub(crate) fn sub(self, o: Expr) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::sub(self, o),
        }
    }

    /// Tests whether `self = o` in the Liebniz sense.
    pub(crate) fn eq(self, o: Expr) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::eq(self, o),
        }
    }

    /// Tests whether `self < o`, both arguments are expected to be integers
    pub(crate) fn lt(self, o: Expr) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::lt(self, o),
        }
    }

    /// Negates `self`. `self` is expected to be of boolean type
    pub(crate) fn not(self) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::not(self),
        }
    }

    pub(crate) fn var(name: Var) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::Var(name),
        }
    }

    pub(crate) fn bool(b: bool) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::Bool(b),
        }
    }

    pub(crate) fn int(i: i128) -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::Const(i),
        }
    }

    /// Returns a `fresh` expression which produces a non-determinstic value
    pub(crate) fn fresh() -> Self {
        Expr {
            span: SimpleSpan::new(0, 0),
            kind: ExprKind::Fresh,
        }
    }
}

impl ExprKind {
    pub(crate) fn div(lhs: Expr, o: Expr) -> Self {
        ExprKind::BinOp(BinOp::Div, Ptr::new(lhs), Ptr::new(o))
    }

    pub(crate) fn mul(lhs: Expr, o: Expr) -> Self {
        ExprKind::BinOp(BinOp::Mul, Ptr::new(lhs), Ptr::new(o))
    }

    pub(crate) fn add(lhs: Expr, o: Expr) -> Self {
        ExprKind::BinOp(BinOp::Add, Ptr::new(lhs), Ptr::new(o))
    }

    pub(crate) fn sub(lhs: Expr, o: Expr) -> Self {
        ExprKind::BinOp(BinOp::Sub, Ptr::new(lhs), Ptr::new(o))
    }

    pub(crate) fn eq(lhs: Expr, o: Expr) -> Self {
        ExprKind::BinOp(BinOp::Eq, Ptr::new(lhs), Ptr::new(o))
    }

    pub(crate) fn lt(lhs: Expr, o: Expr) -> Self {
        ExprKind::BinOp(BinOp::Lt, Ptr::new(lhs), Ptr::new(o))
    }

    pub(crate) fn not(lhs: Expr) -> ExprKind {
        ExprKind::Not(Ptr::new(lhs))
    }
}

impl From<i128> for ExprKind {
    fn from(value: i128) -> Self {
        ExprKind::Const(value)
    }
}

impl std::fmt::Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty(&pretty::Arena::new()).1.render_fmt(80, f)
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a ExprKind {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        match self {
            ExprKind::Var(v) => alloc.text(v),
            ExprKind::Const(i) => alloc.text(i.to_string()),
            ExprKind::Bool(b) => alloc.text(b.to_string()),
            ExprKind::Fresh => alloc.text("fresh"),
            ExprKind::BinOp(op, a, b) => {
                let a = a.pretty(alloc);
                let b = b.pretty(alloc);
                let op = match op {
                    BinOp::Add => "+",
                    BinOp::Div => "/",
                    BinOp::Mul => "*",
                    BinOp::Sub => "-",
                    BinOp::Eq => "==",
                    BinOp::Lt => "<",
                };
                alloc.concat(vec![a, alloc.space(), alloc.text(op), alloc.space(), b])
            }
            ExprKind::Not(e) => docs![alloc, "not", alloc.space(), &**e],
        }
    }
}

impl<'a, D: DocAllocator<'a>> Pretty<'a, D> for &'a Expr {
    fn pretty(self, alloc: &'a D) -> pretty::DocBuilder<'a, D, ()> {
        self.kind.pretty(alloc)
    }
}

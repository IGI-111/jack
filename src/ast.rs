use crate::types::Type;

pub struct RawFunction {
    pub name: String,
    pub root: RawNode,
    pub ty: Type,
}

#[derive(Debug, PartialEq)]
pub enum RawExpression {
    Int(u64),
    Bool(bool),
    Array(Vec<Box<RawNode>>),
    FunCall(String, Vec<Box<RawNode>>),
    // Id(String),
    BinaryOp(BinaryOp, Box<RawNode>, Box<RawNode>),
    UnaryOp(UnaryOp, Box<RawNode>),
    Conditional(Box<RawNode>, Box<RawNode>, Box<RawNode>),
}
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Minus,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Multiply,
    Divide,
    Add,
    Sub,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    ArrayDeref,
}

pub trait Node: PartialEq + Sized {
    fn expr(&self) -> &RawExpression;
    fn into_expr(self) -> RawExpression;
}

#[derive(PartialEq, Debug)]
pub struct RawNode {
    expr: RawExpression,
}

impl RawNode {
    pub fn new(expr: RawExpression) -> Self {
        Self { expr }
    }
}

impl Node for RawNode {
    fn expr(&self) -> &RawExpression {
        &self.expr
    }
    fn into_expr(self) -> RawExpression {
        self.expr
    }
}

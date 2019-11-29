use super::*;

#[derive(Debug, Clone)]
pub struct RawFunction {
    pub name: String,
    pub root: RawNode,
    pub args: Vec<(String, Type)>,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RawExpression {
    Int(u64),
    Bool(bool),
    Float(f64),
    Array(Vec<Box<RawNode>>),
    FunCall(String, Vec<Box<RawNode>>),
    Id(String), // TODO: semantic checking step
    BinaryOp(BinaryOp, Box<RawNode>, Box<RawNode>),
    UnaryOp(UnaryOp, Box<RawNode>),
    Conditional(Box<RawNode>, Box<RawNode>, Box<RawNode>),
    Let(String, Box<RawNode>, Box<RawNode>),
}
pub trait Node: PartialEq + Sized {
    fn expr(&self) -> &RawExpression;
    fn into_expr(self) -> RawExpression;
}

#[derive(PartialEq, Debug, Clone)]
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

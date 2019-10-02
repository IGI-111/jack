#[derive(Debug, PartialEq)]
pub enum Expression<N> {
    Int(u64),
    Bool(bool),
    // Array(Vec<Box<N>>),
    // Id(String),
    BinaryOp(BinaryOp, Box<N>, Box<N>),
    UnaryOp(UnaryOp, Box<N>),
    Conditional(Box<N>, Box<N>, Box<N>),
}
#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Minus,
    Not,
}

#[derive(Debug, PartialEq)]
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
    // ArrayDeref,
}

pub trait Node: PartialEq + Sized {
    fn expr(&self) -> &Expression<Self>;
    fn into_expr(self) -> Expression<Self>;
}

#[derive(PartialEq, Debug)]
pub struct RawNode {
    expr: Expression<Self>,
}

impl RawNode {
    pub fn new(expr: Expression<Self>) -> Self {
        Self { expr }
    }
}

impl Node for RawNode {
    fn expr(&self) -> &Expression<Self> {
        &self.expr
    }
    fn into_expr(self) -> Expression<Self> {
        self.expr
    }
}

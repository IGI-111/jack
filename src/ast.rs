#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(u64),
    Bool(bool),
    Id(String),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    UnaryOp(UnaryOp, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
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
}

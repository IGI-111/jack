#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub imports: Vec<Import>,
    pub export: Export,
}

#[derive(Debug, PartialEq)]
pub struct Import {
    pub module: String,
}

#[derive(Debug, PartialEq)]
pub struct Export {
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(i64),
    Bool(bool),
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

#[derive(Debug, PartialEq)]
pub struct Module {
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
    Minus(Box<Expression>),
    Not(Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    LessThanOrEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterThanOrEqual(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Option<Expression>>),
}

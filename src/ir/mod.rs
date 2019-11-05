pub mod raw;
pub mod sem;

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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Array(u64, Box<Type>),
    Function(Box<Type>, Vec<Box<(String, Type)>>),
}

impl Type {
    pub fn as_function(&self) -> (&Box<Type>, &Vec<Box<(String, Type)>>) {
        match self {
            Type::Function(ret, args) => (&ret, &args),
            _ => panic!("Not a Function type"),
        }
    }
    pub fn into_function(self) -> (Box<Type>, Vec<Box<(String, Type)>>) {
        match self {
            Type::Function(ret, args) => (ret, args),
            _ => panic!("Not a Function type"),
        }
    }
}

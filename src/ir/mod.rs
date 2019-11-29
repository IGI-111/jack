use crate::error::{CompilerError, Result};
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
    Float,
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
    pub fn assert_eq(&self, other: &Self) -> Result<()> {
        if self != other {
            return Err(CompilerError::TypeConflict(self.clone(), other.clone()).into());
        }
        Ok(())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Int => "int".to_string(),
                Type::Bool => "bool".to_string(),
                Type::Float => "float".to_string(),
                Type::Array(len, ty) => format!("[{};{}]", ty, len),
                Type::Function(ret, args) => format!(
                    "fun ({}): {}",
                    args.iter()
                        .map(|arg| {
                            let (name, ty) = arg.as_ref();
                            format!("{}: {}", name, ty)
                        })
                        .collect::<Vec<String>>()
                        .join(","),
                    ret,
                ),
            }
        )
    }
}

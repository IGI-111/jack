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

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Minus => "-",
                UnaryOp::Not => "!",
            }
        )
    }
}

impl UnaryOp {
    pub fn check_ty(&self, operand: &Type) -> Result<()> {
        let allowed_types = match self {
            UnaryOp::Minus => vec![Type::Int, Type::Float],
            UnaryOp::Not => vec![Type::Bool],
        };
        if !allowed_types.contains(operand) {
            return Err(
                CompilerError::WrongUnaryOperatorType(operand.clone(), self.clone()).into(),
            );
        }
        Ok(())
    }
}

impl BinaryOp {
    pub fn check_ty(&self, left_operand: &Type, right_operand: &Type) -> Result<()> {
        if let BinaryOp::ArrayDeref = self {
            if let Type::Array(_, _) = left_operand {
                if right_operand != &Type::Int {
                    return Err(CompilerError::WrongBinaryOperatorType(
                        right_operand.clone(),
                        self.clone(),
                    )
                    .into());
                }
            } else {
                return Err(CompilerError::WrongBinaryOperatorType(
                    left_operand.clone(),
                    self.clone(),
                )
                .into());
            }
        } else {
            let allowed_types = match self {
                BinaryOp::LessThan
                | BinaryOp::GreaterThan
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Add
                | BinaryOp::Sub => vec![Type::Float, Type::Int],

                BinaryOp::LessThanOrEqual
                | BinaryOp::GreaterThanOrEqual
                | BinaryOp::Equal
                | BinaryOp::NotEqual => vec![Type::Float, Type::Int],
                BinaryOp::Or | BinaryOp::And => vec![Type::Bool],
                BinaryOp::ArrayDeref => unreachable!(),
            };

            left_operand.assert_eq(right_operand)?;
            let operand = left_operand; // same types
            if !allowed_types.contains(operand) {
                return Err(
                    CompilerError::WrongBinaryOperatorType(operand.clone(), self.clone()).into(),
                );
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Multiply => "*",
                BinaryOp::Divide => "/",
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::LessThan => "<",
                BinaryOp::LessThanOrEqual => "<=",
                BinaryOp::GreaterThan => ">",
                BinaryOp::GreaterThanOrEqual => ">=",
                BinaryOp::Equal => "==",
                BinaryOp::NotEqual => "!=",
                BinaryOp::And => "&&",
                BinaryOp::Or => "||",
                BinaryOp::ArrayDeref => "[]",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Float,
    Array(u64, Box<Type>),
    Function(Box<Type>, Vec<(String, Type)>),
}

impl Type {
    pub fn as_function(&self) -> (&Type, &Vec<(String, Type)>) {
        match self {
            Type::Function(ret, args) => (&ret, &args),
            _ => panic!("Not a Function type"),
        }
    }
    pub fn into_function(self) -> (Box<Type>, Vec<(String, Type)>) {
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
                            let (name, ty) = arg;
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

use crate::ir::{BinaryOp, Type, UnaryOp};
use err_derive::Error;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error(display = "Syntax Error\n{}", 0)]
    Syntax(String),
    #[error(display = "Type Error:\n{} is incompatible with {}", 0, 1)]
    TypeConflict(Type, Type),
    #[error(display = "Type Error:\n{} can't be used by operator {}", 0, 1)]
    WrongBinaryOperatorType(Type, BinaryOp),
    #[error(display = "Type Error:\n{} can't be used by operator {}", 0, 1)]
    WrongUnaryOperatorType(Type, UnaryOp),
    #[error(display = "Unknown Variable: {} ", 0)]
    UnknownVariable(String),
    #[error(display = "Unknown Function: {} ", 0)]
    UnknownFunction(String),
    #[error(
        display = "Wrong number of arguments in call to {}.\nExpected {}, got {}.",
        0,
        1,
        2
    )]
    WrongNumberOfArguments(String, usize, usize),
    #[error(display = "Can't index into non array type: {} ", 0)]
    CannotIndex(Type),
    #[error(display = "Backend error: {}", 0)]
    BackendError(String),
}

use crate::ast::*;

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Bool(bool),
}

impl Value {
    fn unwrap_int(self) -> i64 {
        match self {
            Value::Int(val) => val,
            _ => panic!(format!("Invalid type: {:?} is not an Int", self)),
        }
    }
    fn unwrap_bool(self) -> bool {
        match self {
            Value::Bool(val) => val,
            _ => panic!(format!("Invalid type: {:?} is not a Bool", self)),
        }
    }
}

pub fn eval(expr: &Expression) -> Value {
    match expr {
        Expression::BinaryOp(op, a, b) => match op {
            BinaryOp::Multiply => Value::Int(eval(a).unwrap_int() * eval(b).unwrap_int()),
            BinaryOp::Divide => Value::Int(eval(a).unwrap_int() / eval(b).unwrap_int()),
            BinaryOp::Add => Value::Int(eval(a).unwrap_int() + eval(b).unwrap_int()),
            BinaryOp::Sub => Value::Int(eval(a).unwrap_int() - eval(b).unwrap_int()),
            BinaryOp::LessThan => Value::Bool(eval(a).unwrap_int() < eval(b).unwrap_int()),
            BinaryOp::LessThanOrEqual => Value::Bool(eval(a).unwrap_int() <= eval(b).unwrap_int()),
            BinaryOp::GreaterThan => Value::Bool(eval(a).unwrap_int() > eval(b).unwrap_int()),
            BinaryOp::GreaterThanOrEqual => {
                Value::Bool(eval(a).unwrap_int() >= eval(b).unwrap_int())
            }
            BinaryOp::Equal => match (eval(a), eval(b)) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                (Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                _ => panic!("Invalid type"),
            },
            BinaryOp::NotEqual => match (eval(a), eval(b)) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a != b),
                (Value::Int(a), Value::Int(b)) => Value::Bool(a != b),
                _ => panic!("Invalid type"),
            },
            BinaryOp::And => Value::Bool(eval(a).unwrap_bool() && eval(b).unwrap_bool()),
            BinaryOp::Or => Value::Bool(eval(a).unwrap_bool() || eval(b).unwrap_bool()),
        },
        Expression::UnaryOp(op, a) => match op {
            UnaryOp::Minus => Value::Int(-eval(a).unwrap_int()),
            UnaryOp::Not => Value::Bool(!eval(a).unwrap_bool()),
        },

        Expression::Int(val) => Value::Int(*val),
        Expression::Bool(val) => Value::Bool(*val),
        Expression::Conditional(cond, expr, alt) => {
            if eval(cond).unwrap_bool() {
                eval(expr)
            } else {
                eval(alt)
            }
        }
    }
}

pub fn run(ast: &Module) -> Value {
    // TODO: resolve modules
    eval(&ast.export.expr)
}

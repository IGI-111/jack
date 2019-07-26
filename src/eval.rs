use crate::ast::*;
use std::collections::HashMap;
use std::path::Path;

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

pub struct Context<'a> {
    modules: &'a HashMap<String, Module>,
}

impl<'a> Context<'a> {
    pub fn new(modules: &'a HashMap<String, Module>) -> Self {
        Self { modules }
    }
    pub fn get_module(&self, name: &str) -> &Module {
        self.modules
            .get(name)
            .expect(&format!("Unknown module {}", name))
    }
}

pub fn eval(expr: &Expression, ctx: &Context) -> Value {
    match expr {
        Expression::Id(id) => eval(&ctx.get_module(id).export.expr, ctx),
        Expression::BinaryOp(op, a, b) => match op {
            BinaryOp::Multiply => Value::Int(eval(a, ctx).unwrap_int() * eval(b, ctx).unwrap_int()),
            BinaryOp::Divide => Value::Int(eval(a, ctx).unwrap_int() / eval(b, ctx).unwrap_int()),
            BinaryOp::Add => Value::Int(eval(a, ctx).unwrap_int() + eval(b, ctx).unwrap_int()),
            BinaryOp::Sub => Value::Int(eval(a, ctx).unwrap_int() - eval(b, ctx).unwrap_int()),
            BinaryOp::LessThan => {
                Value::Bool(eval(a, ctx).unwrap_int() < eval(b, ctx).unwrap_int())
            }
            BinaryOp::LessThanOrEqual => {
                Value::Bool(eval(a, ctx).unwrap_int() <= eval(b, ctx).unwrap_int())
            }
            BinaryOp::GreaterThan => {
                Value::Bool(eval(a, ctx).unwrap_int() > eval(b, ctx).unwrap_int())
            }
            BinaryOp::GreaterThanOrEqual => {
                Value::Bool(eval(a, ctx).unwrap_int() >= eval(b, ctx).unwrap_int())
            }
            BinaryOp::Equal => match (eval(a, ctx), eval(b, ctx)) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
                (Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
                _ => panic!("Invalid type"),
            },
            BinaryOp::NotEqual => match (eval(a, ctx), eval(b, ctx)) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a != b),
                (Value::Int(a), Value::Int(b)) => Value::Bool(a != b),
                _ => panic!("Invalid type"),
            },
            BinaryOp::And => Value::Bool(eval(a, ctx).unwrap_bool() && eval(b, ctx).unwrap_bool()),
            BinaryOp::Or => Value::Bool(eval(a, ctx).unwrap_bool() || eval(b, ctx).unwrap_bool()),
        },
        Expression::UnaryOp(op, a) => match op {
            UnaryOp::Minus => Value::Int(-eval(a, ctx).unwrap_int()),
            UnaryOp::Not => Value::Bool(!eval(a, ctx).unwrap_bool()),
        },

        Expression::Int(val) => Value::Int(*val),
        Expression::Bool(val) => Value::Bool(*val),
        Expression::Conditional(cond, expr, alt) => {
            if eval(cond, ctx).unwrap_bool() {
                eval(expr, ctx)
            } else {
                eval(alt, ctx)
            }
        }
    }
}

pub fn run(main_module_name: &str, modules: &HashMap<String, Module>) -> Value {
    let ctx = Context::new(modules);
    let main_module = modules.get(main_module_name).unwrap();
    eval(&main_module.export.expr, &ctx)
}

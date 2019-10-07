use crate::ast::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int,
    Bool,
}

#[derive(Debug, PartialEq)]
pub enum TypedExpression {
    Int(u64),
    Bool(bool),
    BinaryOp(BinaryOp, Box<TypedNode>, Box<TypedNode>),
    UnaryOp(UnaryOp, Box<TypedNode>),
    Conditional(Box<TypedNode>, Box<TypedNode>, Box<TypedNode>),
}

#[derive(PartialEq, Debug)]
pub struct TypedNode {
    ty: Type,
    expr: TypedExpression,
}

impl TypedNode {
    pub fn infer_types(node: RawNode) -> Self {
        let expr = match node.into_expr() {
            RawExpression::Int(val) => TypedExpression::Int(val),
            RawExpression::Bool(val) => TypedExpression::Bool(val),
            RawExpression::BinaryOp(op, a, b) => TypedExpression::BinaryOp(
                op,
                Box::new(Self::infer_types(*a)),
                Box::new(Self::infer_types(*b)),
            ),
            RawExpression::UnaryOp(op, a) => {
                TypedExpression::UnaryOp(op, Box::new(Self::infer_types(*a)))
            }
            RawExpression::Conditional(cond, then, alt) => TypedExpression::Conditional(
                Box::new(Self::infer_types(*cond)),
                Box::new(Self::infer_types(*then)),
                Box::new(Self::infer_types(*alt)),
            ),
        };

        let ty = match &expr {
            TypedExpression::Int(_) => Type::Int,
            TypedExpression::Bool(_) => Type::Bool,
            TypedExpression::UnaryOp(op, a) => match op {
                UnaryOp::Minus => {
                    assert_eq!(a.ty(), &Type::Int);
                    Type::Int
                }
                UnaryOp::Not => {
                    assert_eq!(a.ty(), &Type::Bool);
                    Type::Bool
                }
            },
            TypedExpression::BinaryOp(op, a, b) => match op {
                BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Add | BinaryOp::Sub => {
                    assert_eq!(a.ty(), &Type::Int);
                    assert_eq!(b.ty(), &Type::Int);
                    Type::Int
                }
                BinaryOp::LessThan
                | BinaryOp::LessThanOrEqual
                | BinaryOp::GreaterThan
                | BinaryOp::GreaterThanOrEqual => {
                    assert_eq!(a.ty(), &Type::Int);
                    assert_eq!(b.ty(), &Type::Int);
                    Type::Bool
                }
                BinaryOp::Equal | BinaryOp::NotEqual => {
                    assert_eq!(a.ty(), b.ty());
                    Type::Bool
                }
                BinaryOp::And | BinaryOp::Or => {
                    assert_eq!(a.ty(), &Type::Bool);
                    assert_eq!(b.ty(), &Type::Bool);
                    Type::Bool
                }
            },
            TypedExpression::Conditional(cond, then, alt) => {
                assert_eq!(cond.ty(), &Type::Bool);
                assert_eq!(then.ty(), alt.ty());
                then.ty().clone()
            }
        };

        Self { expr, ty }
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
    pub fn expr(&self) -> &TypedExpression {
        &self.expr
    }
}

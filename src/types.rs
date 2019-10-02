use crate::ast::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int,
    Bool,
}

#[derive(PartialEq, Debug)]
pub struct TypedNode {
    ty: Type,
    expr: Expression<Self>,
}

impl TypedNode {
    pub fn infer_types(node: RawNode) -> Self {
        let expr = match node.into_expr() {
            Expression::Int(val) => Expression::Int(val),
            Expression::Bool(val) => Expression::Bool(val),
            Expression::BinaryOp(op, a, b) => Expression::BinaryOp(
                op,
                Box::new(Self::infer_types(*a)),
                Box::new(Self::infer_types(*b)),
            ),
            Expression::UnaryOp(op, a) => Expression::UnaryOp(op, Box::new(Self::infer_types(*a))),
            Expression::Conditional(cond, then, alt) => Expression::Conditional(
                Box::new(Self::infer_types(*cond)),
                Box::new(Self::infer_types(*then)),
                Box::new(Self::infer_types(*alt)),
            ),
        };

        let ty = match &expr {
            Expression::Int(_) => Type::Int,
            Expression::Bool(_) => Type::Bool,
            Expression::UnaryOp(op, a) => match op {
                UnaryOp::Minus => {
                    assert_eq!(a.ty(), &Type::Int);
                    Type::Int
                }
                UnaryOp::Not => {
                    assert_eq!(a.ty(), &Type::Bool);
                    Type::Bool
                }
            },
            Expression::BinaryOp(op, a, b) => match op {
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
            Expression::Conditional(cond, then, alt) => {
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
}

impl Node for TypedNode {
    fn expr(&self) -> &Expression<Self> {
        &self.expr
    }
    fn into_expr(self) -> Expression<Self> {
        self.expr
    }
}

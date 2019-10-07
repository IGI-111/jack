use crate::ast::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Array(u64, Box<Type>),
}

// impl PartialEq for Type {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => true,
//             (Type::Array(a), Type::Array(b)) => **a == **b,
//             _ => false,
//         }
//     }
// }

#[derive(Debug, PartialEq)]
pub enum TypedExpression {
    Int(u64),
    Bool(bool),
    Array(Vec<Box<TypedNode>>),
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
            RawExpression::Array(val) => TypedExpression::Array(
                val.into_iter()
                    .map(|n| Box::new(Self::infer_types(*n)))
                    .collect(),
            ),
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
            TypedExpression::Array(vals) => {
                let inner_type = if vals.len() > 0 {
                    vals[0].ty().clone()
                } else {
                    Type::Int
                };
                for val in vals.iter() {
                    assert_eq!(val.ty(), &inner_type);
                }
                Type::Array(vals.len() as u64, Box::new(inner_type))
            }
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
                BinaryOp::ArrayDeref => {
                    if let Type::Array(_, elem_type) = a.ty() {
                        if let TypedExpression::Int(_) = b.expr() {
                            (**elem_type).clone()
                        } else {
                            panic!("Cannot index by anything else than a literal integer");
                        }
                    } else {
                        panic!(format!("{:?} is not an array", a.ty()))
                    }
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

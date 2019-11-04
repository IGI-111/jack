use crate::ast::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Array(u64, Box<Type>),
    Function(Box<Type>, Vec<Box<Type>>),
}

impl Type {
    pub fn as_function(&self) -> (&Box<Type>, &Vec<Box<Type>>) {
        match self {
            Type::Function(ret, args) => (&ret, &args),
            _ => panic!("Not a Function type"),
        }
    }
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
    FunCall(String, Vec<Box<TypedNode>>),
    BinaryOp(BinaryOp, Box<TypedNode>, Box<TypedNode>),
    UnaryOp(UnaryOp, Box<TypedNode>),
    Conditional(Box<TypedNode>, Box<TypedNode>, Box<TypedNode>),
}

#[derive(PartialEq, Debug)]
pub struct TypedFunction {
    name: String,
    root: TypedNode,
    ty: Type,
}

impl TypedFunction {
    pub fn infer_types(fun: &RawFunction, funcs: &[RawFunction]) -> Self {
        let name = fun.name.to_string();
        let root = TypedNode::infer_types(&fun.root, funcs);
        let (ret, args) = fun.ty.as_function();
        assert_eq!(root.ty(), &**ret);
        let ty = Type::Function(ret.clone(), args.clone());
        Self { root, name, ty }
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn root(&self) -> &TypedNode {
        &self.root
    }
}

#[derive(PartialEq, Debug)]
pub struct TypedNode {
    ty: Type,
    expr: TypedExpression,
}

impl TypedNode {
    pub fn infer_types(node: &RawNode, funcs: &[RawFunction]) -> Self {
        let expr = match node.expr() {
            RawExpression::Int(val) => TypedExpression::Int(*val),
            RawExpression::Bool(val) => TypedExpression::Bool(*val),
            RawExpression::Array(val) => TypedExpression::Array(
                val.into_iter()
                    .map(|n| Box::new(Self::infer_types(&*n, funcs)))
                    .collect(),
            ),
            RawExpression::BinaryOp(op, a, b) => TypedExpression::BinaryOp(
                op.clone(),
                Box::new(Self::infer_types(&*a, funcs)),
                Box::new(Self::infer_types(&*b, funcs)),
            ),
            RawExpression::UnaryOp(op, a) => {
                TypedExpression::UnaryOp(op.clone(), Box::new(Self::infer_types(&*a, funcs)))
            }
            RawExpression::Conditional(cond, then, alt) => TypedExpression::Conditional(
                Box::new(Self::infer_types(&*cond, funcs)),
                Box::new(Self::infer_types(&*then, funcs)),
                Box::new(Self::infer_types(&*alt, funcs)),
            ),
            RawExpression::FunCall(id, args) => TypedExpression::FunCall(
                id.to_string(),
                args.into_iter()
                    .map(|a| Box::new(Self::infer_types(&*a, funcs)))
                    .collect::<Vec<_>>(),
            ),
        };

        let ty = match &expr {
            TypedExpression::Int(_) => Type::Int,
            TypedExpression::Bool(_) => Type::Bool,
            TypedExpression::FunCall(id, args) => {
                let fun_def = funcs
                    .iter()
                    .find(|f| &f.name == id)
                    .expect("Unknown function");

                let (ret, argdefs) = fun_def.ty.as_function();
                assert_eq!(args.len(), argdefs.len());

                for (arg_type, arg_node) in argdefs.iter().zip(args.iter()) {
                    assert_eq!(&**arg_type, arg_node.ty());
                }
                (**ret).clone()
            }
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

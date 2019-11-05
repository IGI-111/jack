use super::raw::*;
use super::*;

#[derive(Debug, PartialEq)]
pub enum SemExpression {
    Int(u64),
    Bool(bool),
    Array(Vec<Box<SemNode>>),
    Id(String), // TODO: semantic checking step
    FunCall(String, Vec<Box<SemNode>>),
    BinaryOp(BinaryOp, Box<SemNode>, Box<SemNode>),
    UnaryOp(UnaryOp, Box<SemNode>),
    Conditional(Box<SemNode>, Box<SemNode>, Box<SemNode>),
}

#[derive(PartialEq, Debug)]
pub struct SemFunction {
    name: String,
    root: SemNode,
    args: Vec<(String, Type)>,
    ty: Type,
}

impl SemFunction {
    pub fn analyze(fun: RawFunction, funcs: &[RawFunction]) -> Self {
        let name = fun.name.to_string();
        let root = SemNode::analyze(fun.root, funcs);
        let (ret, args) = fun.ty.into_function();
        assert_eq!(*root.ty(), *ret);
        let ty = Type::Function(ret, args);
        let args = fun.args;
        Self {
            root,
            name,
            ty,
            args,
        }
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn root(&self) -> &SemNode {
        &self.root
    }
    pub fn args(&self) -> &Vec<(String, Type)> {
        &self.args
    }
}

#[derive(PartialEq, Debug)]
pub struct SemNode {
    ty: Type,
    expr: SemExpression,
}

impl SemNode {
    pub fn analyze(node: RawNode, funcs: &[RawFunction]) -> Self {
        let expr = match node.into_expr() {
            RawExpression::Int(val) => SemExpression::Int(val),
            RawExpression::Bool(val) => SemExpression::Bool(val),
            RawExpression::Id(val) => SemExpression::Id(val),
            RawExpression::Array(val) => SemExpression::Array(
                val.into_iter()
                    .map(|n| Box::new(Self::analyze(*n, funcs)))
                    .collect(),
            ),
            RawExpression::BinaryOp(op, a, b) => SemExpression::BinaryOp(
                op.clone(),
                Box::new(Self::analyze(*a, funcs)),
                Box::new(Self::analyze(*b, funcs)),
            ),
            RawExpression::UnaryOp(op, a) => {
                SemExpression::UnaryOp(op.clone(), Box::new(Self::analyze(*a, funcs)))
            }
            RawExpression::Conditional(cond, then, alt) => SemExpression::Conditional(
                Box::new(Self::analyze(*cond, funcs)),
                Box::new(Self::analyze(*then, funcs)),
                Box::new(Self::analyze(*alt, funcs)),
            ),
            RawExpression::FunCall(id, args) => SemExpression::FunCall(
                id.to_string(),
                args.into_iter()
                    .map(|a| Box::new(Self::analyze(*a, funcs)))
                    .collect::<Vec<_>>(),
            ),
        };

        let ty = match &expr {
            SemExpression::Int(_) => Type::Int,
            SemExpression::Bool(_) => Type::Bool,
            SemExpression::Id(name) => {
                // FIXME: proper semantic analysis
                Type::Bool
            }
            SemExpression::FunCall(id, args) => {
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
            SemExpression::Array(vals) => {
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
            SemExpression::UnaryOp(op, a) => match op {
                UnaryOp::Minus => {
                    assert_eq!(a.ty(), &Type::Int);
                    Type::Int
                }
                UnaryOp::Not => {
                    assert_eq!(a.ty(), &Type::Bool);
                    Type::Bool
                }
            },
            SemExpression::BinaryOp(op, a, b) => match op {
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
                        if let SemExpression::Int(_) = b.expr() {
                            (**elem_type).clone()
                        } else {
                            panic!("Cannot index by anything else than a literal integer");
                        }
                    } else {
                        panic!(format!("{:?} is not an array", a.ty()))
                    }
                }
            },
            SemExpression::Conditional(cond, then, alt) => {
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
    pub fn expr(&self) -> &SemExpression {
        &self.expr
    }
}

use super::raw::*;
use super::*;
use std::collections::HashMap;

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
    Let(String, Box<SemNode>, Box<SemNode>),
}

#[derive(PartialEq, Debug)]
pub struct SemFunction {
    name: String,
    root: SemNode,
    args: Vec<(String, Type)>,
    ty: Type,
}

impl SemFunction {
    pub fn analyze(fun: RawFunction, ctx: &SemContext) -> Self {
        let name = fun.name.to_string();
        let (ret, arg_types) = fun.ty.into_function();
        let args = fun.args;

        let available_funs = ctx.funs().clone();
        let mut available_vars = ctx.vars().clone();
        for (arg_name, arg_type) in args.iter() {
            available_vars.insert(arg_name.clone(), arg_type.clone());
        }
        let ctx = SemContext::new(available_funs, available_vars);
        let root = SemNode::analyze(fun.root, &ctx);
        assert_eq!(*root.ty(), *ret);
        let ty = Type::Function(ret, arg_types);

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

#[derive(Debug)]
pub struct SemContext {
    funs: HashMap<String, Type>,
    vars: HashMap<String, Type>,
}

impl SemContext {
    pub fn new(funs: HashMap<String, Type>, vars: HashMap<String, Type>) -> Self {
        Self { funs, vars }
    }
    pub fn funs(&self) -> &HashMap<String, Type> {
        &self.funs
    }
    pub fn vars(&self) -> &HashMap<String, Type> {
        &self.vars
    }
}

#[derive(PartialEq, Debug)]
pub struct SemNode {
    ty: Type,
    expr: SemExpression,
}

impl SemNode {
    pub fn analyze(node: RawNode, ctx: &SemContext) -> Self {
        let expr = match node.into_expr() {
            RawExpression::Int(val) => SemExpression::Int(val),
            RawExpression::Bool(val) => SemExpression::Bool(val),
            RawExpression::Id(val) => SemExpression::Id(val),
            RawExpression::Let(id, val, expr) => {
                let val = Box::new(Self::analyze(*val, ctx));

                let available_funs = ctx.funs().clone();
                let mut available_vars = ctx.vars().clone();
                available_vars.insert(id.clone(), val.ty().clone());
                let ctx = SemContext::new(available_funs, available_vars);

                let expr = Box::new(Self::analyze(*expr, &ctx));
                SemExpression::Let(id, val, expr)
            }
            RawExpression::Array(val) => SemExpression::Array(
                val.into_iter()
                    .map(|n| Box::new(Self::analyze(*n, ctx)))
                    .collect(),
            ),
            RawExpression::BinaryOp(op, a, b) => SemExpression::BinaryOp(
                op.clone(),
                Box::new(Self::analyze(*a, ctx)),
                Box::new(Self::analyze(*b, ctx)),
            ),
            RawExpression::UnaryOp(op, a) => {
                SemExpression::UnaryOp(op.clone(), Box::new(Self::analyze(*a, ctx)))
            }
            RawExpression::Conditional(cond, then, alt) => SemExpression::Conditional(
                Box::new(Self::analyze(*cond, ctx)),
                Box::new(Self::analyze(*then, ctx)),
                Box::new(Self::analyze(*alt, ctx)),
            ),
            RawExpression::FunCall(id, args) => SemExpression::FunCall(
                id.to_string(),
                args.into_iter()
                    .map(|a| Box::new(Self::analyze(*a, ctx)))
                    .collect::<Vec<_>>(),
            ),
        };

        let ty = match &expr {
            SemExpression::Int(_) => Type::Int,
            SemExpression::Bool(_) => Type::Bool,
            SemExpression::Id(name) => ctx
                .vars()
                .get(name)
                .expect(&format!("Unknown variable {}", name))
                .clone(),
            SemExpression::Let(_id, _val, expr) => expr.ty().clone(),
            SemExpression::FunCall(id, args) => {
                let ftype = ctx.funs().get(id).expect("Unknown function");

                let (ret, argdefs) = ftype.as_function();
                assert_eq!(args.len(), argdefs.len());

                for ((_arg_name, arg_type), arg_node) in
                    argdefs.iter().map(|a| a.as_ref()).zip(args.iter())
                {
                    assert_eq!(arg_type, arg_node.ty());
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

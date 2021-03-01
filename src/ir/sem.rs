use super::raw::*;
use super::*;
use crate::error::{CompilerError, Result};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum SemExpression {
    Int(i64),
    Bool(bool),
    Float(f64),
    Array(Vec<SemNode>),
    Id(String), // TODO: semantic checking step
    FunCall(String, Vec<SemNode>),
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
    pub fn analyze(fun: RawFunction, ctx: &SemContext) -> Result<Self> {
        let name = fun.name.to_string();
        let (ret, arg_types) = fun.ty.into_function();
        let args = fun.args;

        let ctx = ctx.extend_vars(args.iter().cloned());
        let root = SemNode::analyze(fun.root, &ctx)?;
        (&*root.ty()).assert_eq(&*ret)?;
        let ty = Type::Function(ret, arg_types);

        Ok(Self {
            root,
            name,
            ty,
            args,
        })
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
    pub fn from_funs(funs: impl IntoIterator<Item = (String, Type)>) -> Self {
        let funs = funs.into_iter().collect::<HashMap<_, _>>();
        let vars = HashMap::new();
        Self { funs, vars }
    }
    pub fn funs(&self) -> &HashMap<String, Type> {
        &self.funs
    }
    pub fn vars(&self) -> &HashMap<String, Type> {
        &self.vars
    }
    pub fn extend_var(&self, id: String, ty: Type) -> Self {
        let funs = self.funs.clone();
        let mut vars = self.vars.clone();
        vars.insert(id, ty);
        Self { funs, vars }
    }
    pub fn extend_vars(&self, it: impl IntoIterator<Item = (String, Type)>) -> Self {
        let funs = self.funs.clone();
        let mut vars = self.vars.clone();
        for (id, ty) in it {
            vars.insert(id, ty);
        }
        Self { funs, vars }
    }
}

#[derive(PartialEq, Debug)]
pub struct SemNode {
    ty: Type,
    expr: SemExpression,
}

impl SemNode {
    pub fn analyze(node: RawNode, ctx: &SemContext) -> Result<Self> {
        let expr = match node.into_expr() {
            RawExpression::Int(val) => SemExpression::Int(val),
            RawExpression::Bool(val) => SemExpression::Bool(val),
            RawExpression::Float(val) => SemExpression::Float(val),
            RawExpression::Id(val) => SemExpression::Id(val),
            RawExpression::Let(id, val, expr) => {
                let val = Box::new(Self::analyze(*val, ctx)?);
                let ctx = ctx.extend_var(id.clone(), val.ty().clone());
                let expr = Box::new(Self::analyze(*expr, &ctx)?);
                SemExpression::Let(id, val, expr)
            }
            RawExpression::Array(val) => {
                let mut out = Vec::new();
                for n in val {
                    out.push(Self::analyze(n, ctx)?);
                }
                SemExpression::Array(out)
            }
            RawExpression::BinaryOp(op, a, b) => SemExpression::BinaryOp(
                op,
                Box::new(Self::analyze(*a, ctx)?),
                Box::new(Self::analyze(*b, ctx)?),
            ),
            RawExpression::UnaryOp(op, a) => {
                SemExpression::UnaryOp(op, Box::new(Self::analyze(*a, ctx)?))
            }
            RawExpression::Conditional(cond, then, alt) => SemExpression::Conditional(
                Box::new(Self::analyze(*cond, ctx)?),
                Box::new(Self::analyze(*then, ctx)?),
                Box::new(Self::analyze(*alt, ctx)?),
            ),
            RawExpression::FunCall(id, args) => {
                let mut out_args = Vec::new();
                for a in args {
                    out_args.push(Self::analyze(a, ctx)?);
                }
                SemExpression::FunCall(id, out_args)
            }
        };

        let ty = match &expr {
            SemExpression::Int(_) => Type::Int,
            SemExpression::Bool(_) => Type::Bool,
            SemExpression::Float(_) => Type::Float,
            SemExpression::Id(name) => match ctx.vars().get(name) {
                Some(ty) => ty.clone(),
                None => return Err(CompilerError::UnknownVariable(name.to_string())),
            },
            SemExpression::Let(_id, _val, expr) => expr.ty().clone(),
            SemExpression::FunCall(id, args) => {
                let ftype = match ctx.funs().get(id) {
                    Some(ty) => ty.clone(),
                    None => return Err(CompilerError::UnknownFunction(id.to_string())),
                };

                let (ret, argdefs) = ftype.as_function();
                if args.len() != argdefs.len() {
                    return Err(CompilerError::WrongNumberOfArguments(
                        id.to_string(),
                        argdefs.len(),
                        args.len(),
                    ));
                }

                for ((_arg_name, arg_type), arg_node) in argdefs.iter().zip(args.iter()) {
                    arg_type.assert_eq(arg_node.ty())?;
                }
                (*ret).clone()
            }
            SemExpression::Array(vals) => {
                let inner_type = if !vals.is_empty() {
                    vals[0].ty().clone()
                } else {
                    Type::Int
                };
                for val in vals.iter() {
                    val.ty().assert_eq(&inner_type)?;
                }
                Type::Array(vals.len() as u64, Box::new(inner_type))
            }
            SemExpression::UnaryOp(op, a) => {
                op.check_ty(a.ty())?;
                match op {
                    UnaryOp::Minus => Type::Int,
                    UnaryOp::Not => Type::Bool,
                }
            }
            SemExpression::BinaryOp(op, a, b) => {
                op.check_ty(a.ty(), b.ty())?;
                match op {
                    BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Add | BinaryOp::Sub => {
                        a.ty().clone()
                    }
                    BinaryOp::LessThan
                    | BinaryOp::LessThanOrEqual
                    | BinaryOp::GreaterThan
                    | BinaryOp::GreaterThanOrEqual
                    | BinaryOp::Equal
                    | BinaryOp::NotEqual
                    | BinaryOp::And
                    | BinaryOp::Or => Type::Bool,
                    BinaryOp::ArrayDeref => {
                        if let Type::Array(_, elem_type) = a.ty() {
                            if let SemExpression::Int(_) = b.expr() {
                                (**elem_type).clone()
                            } else {
                                unreachable!()
                            }
                        } else {
                            return Err(CompilerError::CannotIndex(a.ty().clone()));
                        }
                    }
                }
            }
            SemExpression::Conditional(cond, then, alt) => {
                cond.ty().assert_eq(&Type::Bool)?;
                then.ty().assert_eq(alt.ty())?;
                then.ty().clone()
            }
        };

        Ok(Self { expr, ty })
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
    pub fn expr(&self) -> &SemExpression {
        &self.expr
    }
}

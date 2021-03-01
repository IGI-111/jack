use super::real_type;
use crate::error::{CompilerError, Result};
use crate::ir;
use crate::ir::sem::*;
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use std::collections::HashMap;

pub(crate) struct FunctionTranslator<'a, M: Module> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut M,
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    pub fn new(
        builder: FunctionBuilder<'a>,
        variables: HashMap<String, Variable>,
        module: &'a mut M,
    ) -> Self {
        Self {
            builder,
            variables,
            module,
        }
    }
    pub fn translate_expr(&mut self, node: &SemNode) -> Result<Value> {
        let real_ty = real_type(node.ty())?;
        match node.expr() {
            SemExpression::FunCall(func_name, args) => {
                self.translate_funcall(func_name, args, real_ty)
            }
            SemExpression::Conditional(cond, then, alt) => {
                self.translate_conditional(cond, then, alt, real_ty)
            }
            // BinaryOp(BinaryOp, Box<SemNode>, Box<SemNode>),
            SemExpression::UnaryOp(op, n) => {
                let operand = self.translate_expr(n)?;
                match (op, node.ty()) {
                    (ir::UnaryOp::Minus, ir::Type::Int) => Ok(self.builder.ins().ineg(operand)),
                    (ir::UnaryOp::Minus, ir::Type::Float) => Ok(self.builder.ins().fneg(operand)),
                    (ir::UnaryOp::Not, ir::Type::Bool) => Ok(self.builder.ins().bnot(operand)),
                    _ => Err(CompilerError::BackendError(format!(
                        "Invalid unary operation {:?} for type {:?}",
                        op,
                        node.ty()
                    ))),
                }
            }
            // Let(String, Box<SemNode>, Box<SemNode>),
            // Array(Vec<SemNode>),
            SemExpression::Id(id) => {
                let variable: &Variable = self.variables.get(id).ok_or_else(|| {
                    CompilerError::BackendError(format!("No variable {} available", id))
                })?;
                Ok(self.builder.use_var(*variable))
            }
            SemExpression::Int(val) => Ok(self.builder.ins().iconst(real_ty, *val)),
            SemExpression::Bool(val) => Ok(self.builder.ins().bconst(real_ty, *val)),
            SemExpression::Float(val) => Ok(self.builder.ins().f64const(*val)),
            _ => Err(CompilerError::BackendError(format!(
                "Unsupported node {:?}",
                node
            ))),
        }
    }

    fn translate_conditional(
        &mut self,
        cond: &Box<SemNode>,
        then: &Box<SemNode>,
        alt: &Box<SemNode>,
        out_ty: cranelift::prelude::Type,
    ) -> Result<Value> {
        let cond_val = self.translate_expr(cond)?;

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, out_ty);

        self.builder.ins().brz(cond_val, else_block, &[]);
        self.builder.ins().jump(then_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_val = self.translate_expr(then)?;

        self.builder.ins().jump(merge_block, &[then_val]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let else_val = self.translate_expr(alt)?;

        self.builder.ins().jump(merge_block, &[else_val]);

        self.builder.switch_to_block(merge_block);

        self.builder.seal_block(merge_block);
        let phi = self.builder.block_params(merge_block)[0];

        Ok(phi)
    }

    fn translate_funcall(
        &mut self,
        func_name: &str,
        args: &[SemNode],
        return_ty: cranelift::prelude::Type,
    ) -> Result<Value> {
        let args_values = args
            .iter()
            .map(|arg| self.translate_expr(arg))
            .collect::<Result<Vec<_>>>()?;

        // generate target signature
        let mut sig = self.module.make_signature();
        for arg in args {
            sig.params.push(AbiParam::new(real_type(arg.ty())?));
        }
        sig.returns.push(AbiParam::new(return_ty));

        let callee = self
            .module
            .declare_function(&func_name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let call = self.builder.ins().call(local_callee, &args_values);
        Ok(self.builder.inst_results(call)[0])
    }
    pub fn builder(&mut self) -> &mut FunctionBuilder<'a> {
        &mut self.builder
    }
}

use crate::error::{CompilerError, Result};
use crate::ir;
use crate::ir::sem::*;
use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_simplejit::{SimpleJITBuilder, SimpleJITModule};
use std::collections::HashMap;

pub struct Jit {
    builder_context: FunctionBuilderContext,
    _data_ctx: DataContext,
    module: SimpleJITModule,
}

impl Jit {
    pub fn new() -> Self {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = SimpleJITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            _data_ctx: DataContext::new(),
            module,
        }
    }

    pub fn compile(&mut self, funcs: &[SemFunction]) -> Result<*const u8> {
        // translate AST to cranelift
        let mut sigs = HashMap::new();
        let mut contexts = HashMap::new();
        for fun in funcs {
            let mut ctx = self.module.make_context();

            self.translate_function(fun, &mut ctx)?;
            let name = fun.name();

            let sig = ctx.func.signature.clone();
            sigs.insert(name, sig);

            contexts.insert(name, ctx);
        }

        // declare functions
        let mut ids = HashMap::new();
        for fun in funcs {
            let name = fun.name();

            let sig = sigs.get(name).unwrap();
            let id = self
                .module
                .declare_function(&name, Linkage::Export, &sig)
                .map_err(|e| e.to_string())?;
            ids.insert(name, id);
        }

        // define functions
        for fun in funcs {
            let name = fun.name();
            let id = ids.get(name).unwrap();
            let ctx = contexts.get_mut(name).unwrap();

            self.module
                .define_function(*id, ctx, &mut codegen::binemit::NullTrapSink {})
                .map_err(|e| {
                    CompilerError::BackendError(format!("Can't define function: {}", e))
                })?;
        }

        let main_id = ids
            .get("main")
            .ok_or_else(|| CompilerError::BackendError("No main function".to_string()))?;

        // finalize relocation
        self.module.finalize_definitions();
        let code = self.module.get_finalized_function(*main_id);
        Ok(code)
    }

    fn translate_function(&mut self, func: &SemFunction, ctx: &mut Context) -> Result<()> {
        // setup signature
        for (_name, ty) in func.args() {
            ctx.func
                .signature
                .params
                .push(AbiParam::new(real_type(ty)?));
        }
        let ret_type = if let ir::Type::Function(ret, _args) = func.ty() {
            real_type(ret)?
        } else {
            return Err(Box::new(CompilerError::BackendError(format!(
                "{:?} is not a function type",
                func.ty()
            ))));
        };
        ctx.func.signature.returns.push(AbiParam::new(ret_type));

        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // declare function block level variables
        let mut variable_types = HashMap::new();
        for (name, ty) in func.args() {
            variable_types.insert(name, real_type(ty)?);
        }
        let mut variables: HashMap<String, _> = HashMap::new();
        for (i, (name, _ty)) in func.args().iter().enumerate() {
            let val = builder.block_params(entry_block)[i];
            let var = Variable::new(i);
            variables.insert(name.into(), var);
            builder.declare_var(var, *variable_types.get(name).unwrap());
            builder.def_var(var, val);
        }
        let mut trans = FunctionTranslator {
            builder,
            _variables: variables,
            module: &mut self.module,
        };

        let return_value = trans.translate_expr(func.root())?;

        trans.builder.ins().return_(&[return_value]);
        trans.builder.finalize();
        Ok(())
    }
}

struct FunctionTranslator<'a, M: Module> {
    builder: FunctionBuilder<'a>,
    _variables: HashMap<String, Variable>,
    module: &'a mut M,
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    fn translate_expr(&mut self, node: &SemNode) -> Result<Value> {
        let real_ty = real_type(node.ty())?;
        match node.expr() {
            SemExpression::Int(val) => Ok(self.builder.ins().iconst(real_ty, *val)),
            SemExpression::Bool(val) => Ok(self.builder.ins().bconst(real_ty, *val)),
            SemExpression::Float(val) => Ok(self.builder.ins().f64const(*val)),
            // Id(String), // TODO: semantic checking step
            SemExpression::FunCall(func_name, args) => {
                self.translate_funcall(func_name, args, real_ty)
            }
            // BinaryOp(BinaryOp, Box<SemNode>, Box<SemNode>),
            SemExpression::UnaryOp(op, n) => {
                let operand = self.translate_expr(n)?;
                match (op, node.ty()) {
                    (ir::UnaryOp::Minus, ir::Type::Int) => Ok(self.builder.ins().ineg(operand)),
                    (ir::UnaryOp::Minus, ir::Type::Float) => Ok(self.builder.ins().fneg(operand)),
                    (ir::UnaryOp::Not, ir::Type::Bool) => Ok(self.builder.ins().bnot(operand)),
                    _ => Err(Box::new(CompilerError::BackendError(format!(
                        "Invalid unary operation {:?} for type {:?}",
                        op,
                        node.ty()
                    )))),
                }
            }
            // Conditional(Box<SemNode>, Box<SemNode>, Box<SemNode>),
            // Let(String, Box<SemNode>, Box<SemNode>),
            // Array(Vec<SemNode>),
            _ => Err(Box::new(CompilerError::BackendError(format!(
                "Unsupported node {:?}",
                node
            )))),
        }
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
}

fn real_type(ty: &ir::Type) -> Result<cranelift::prelude::Type> {
    match ty {
        ir::Type::Int => Ok(types::I64),
        ir::Type::Bool => Ok(types::B1),
        ir::Type::Float => Ok(types::F64),
        _ => Err(Box::new(CompilerError::BackendError(format!(
            "Unsupported type {:?}",
            ty
        )))),
    }
}

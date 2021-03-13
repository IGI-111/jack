use crate::error::{CompilerError, Result};
use crate::ir;
use crate::ir::sem::*;
use cranelift::codegen::{ir::ArgumentPurpose, Context};
use cranelift::prelude::*;
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_simplejit::{SimpleJITBuilder, SimpleJITModule};
use function::FunctionTranslator;
use std::collections::HashMap;

mod function;

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
                .declare_function(&name, Linkage::Local, &sig)
                .map_err(|e| CompilerError::BackendError(e.to_string()))?;
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
                    CompilerError::BackendError(format!("Can't define function: {:?}", e))
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
                .push(AbiParam::new(real_type(ty, &self.module)?));
        }
        let ret_type = if let ir::Type::Function(ret, _args) = func.ty() {
            real_type(ret, &self.module)?
        } else {
            return Err(CompilerError::BackendError(format!(
                "{:?} is not a function type",
                func.ty()
            )));
        };
        ctx.func.signature.returns.push(AbiParam::new(ret_type));
        // add special vmcontext param to potentiallu use global values
        let vmcontext = AbiParam::special(
            self.module.target_config().pointer_type(),
            ArgumentPurpose::VMContext,
        );
        ctx.func.signature.params.push(vmcontext);

        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // declare function block level variables
        let mut variable_types = HashMap::new();
        for (name, ty) in func.args() {
            variable_types.insert(name, real_type(ty, &self.module)?);
        }
        let mut variables: HashMap<String, _> = HashMap::new();
        for (i, (name, _ty)) in func.args().iter().enumerate() {
            let val = builder.block_params(entry_block)[i];
            let var = Variable::new(i);
            variables.insert(name.into(), var);
            builder.declare_var(var, *variable_types.get(name).unwrap());
            builder.def_var(var, val);
        }
        let mut trans = FunctionTranslator::new(builder, &mut self.module, variables);

        let return_value = trans.translate_expr(func.root())?;

        trans.builder().ins().return_(&[return_value]);
        trans.builder().finalize();
        println!("{}", trans.builder().display(None));
        Ok(())
    }
}

fn real_type<M: Module>(ty: &ir::Type, module: &M) -> Result<cranelift::prelude::Type> {
    match ty {
        ir::Type::Int => Ok(types::I64),
        ir::Type::Bool => Ok(types::B1),
        ir::Type::Float => Ok(types::F64),
        ir::Type::Array(_num, _inner) => Ok(module.target_config().pointer_type()),
        _ => Err(CompilerError::BackendError(format!(
            "Unsupported type {:?}",
            ty
        ))),
    }
}

use crate::ir::sem::*;
use crate::ir::*;
use expr::gen_expr;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue};
use std::collections::HashMap;

mod expr;

struct GenerationContext<'a> {
    pub builder: &'a Builder,
    pub context: &'a Context,
    pub value_store: &'a HashMap<String, BasicValueEnum>,
    pub current_function: &'a FunctionValue,
    pub functions: &'a HashMap<String, (FunctionValue, &'a SemFunction)>,
}

trait Realizable {
    fn real_type(&self, context: &Context) -> BasicTypeEnum;
}

impl Realizable for Type {
    fn real_type(&self, context: &Context) -> BasicTypeEnum {
        match self {
            Type::Bool => BasicTypeEnum::IntType(context.bool_type()),
            Type::Int => BasicTypeEnum::IntType(context.i64_type()),
            Type::Array(n, elem) => BasicTypeEnum::ArrayType(match elem.real_type(context) {
                BasicTypeEnum::IntType(t) => t.array_type(*n as u32),
                BasicTypeEnum::ArrayType(t) => t.array_type(*n as u32),
                BasicTypeEnum::FloatType(_)
                | BasicTypeEnum::PointerType(_)
                | BasicTypeEnum::VectorType(_)
                | BasicTypeEnum::StructType(_) => panic!(),
            }),
            Type::Function(_, _) => panic!("Function types are not Basic types"),
        }
    }
}

pub fn gen(funcs: &[SemFunction]) -> Module {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let functions = funcs
        .iter()
        .map(|fun| {
            let fn_name = fun.name();
            let fn_type = match fun.ty() {
                Type::Function(ty, args) => match **ty {
                    Type::Bool | Type::Int => ty.real_type(&context).into_int_type().fn_type(
                        &args
                            .into_iter()
                            .map(|arg| match **arg {
                                Type::Bool | Type::Int => {
                                    arg.real_type(&context).into_int_type().as_basic_type_enum()
                                }
                                _ => panic!("Argument of function is not integer type"),
                            })
                            .collect::<Vec<_>>(),
                        false,
                    ),
                    _ => panic!("Function does not return integer type"),
                },
                _ => panic!("Function does not have a function type"),
            };
            (
                fn_name.to_string(),
                (module.add_function(fn_name, fn_type, None), fun),
            )
        })
        .collect::<HashMap<_, _>>();

    for (function, fun) in functions.values() {
        let mut value_store = HashMap::new();
        for (param_val, param_name) in function
            .get_param_iter()
            .zip(fun.args().iter().map(|(name, _)| name))
        {
            value_store.insert(param_name.to_string(), param_val);
        }

        let ctx = GenerationContext {
            value_store: &value_store,
            context: &context,
            builder: &builder,
            current_function: &function,
            functions: &functions,
        };

        let main_block = ctx.context.append_basic_block(&function, "");
        ctx.builder.position_at_end(&main_block);
        let ret_val = match gen_expr(fun.root(), &ctx) {
            BasicValueEnum::IntValue(val) => val,
            _ => unreachable!(),
        };

        ctx.builder.build_return(Some(&ret_val));
    }

    module
}

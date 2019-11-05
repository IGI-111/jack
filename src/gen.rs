use crate::ir::typed::*;
use crate::ir::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{ArrayValue, BasicValue, BasicValueEnum, FunctionValue, IntValue};
use inkwell::IntPredicate;
use std::collections::HashMap;

struct GenerationContext<'a> {
    pub builder: &'a Builder,
    pub context: &'a Context,
    pub current_function: &'a FunctionValue,
    pub functions: &'a HashMap<String, (FunctionValue, &'a TypedFunction)>,
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

fn build_noop(ctx: &GenerationContext) {
    ctx.builder.build_int_add(
        ctx.context.i64_type().const_zero(),
        ctx.context.i64_type().const_zero(),
        "",
    );
}

pub fn gen(funcs: &[TypedFunction]) -> Module {
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
        let ctx = GenerationContext {
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

fn gen_expr(root: &TypedNode, ctx: &GenerationContext) -> BasicValueEnum {
    match root.expr() {
        TypedExpression::FunCall(name, args) => ctx
            .builder
            .build_call(
                ctx.functions.get(name).unwrap().0,
                &args.iter().map(|a| gen_expr(a, ctx)).collect::<Vec<_>>(),
                "",
            )
            .try_as_basic_value()
            .left()
            .expect("Callsite is not convertible to a BasicValue"),
        TypedExpression::Array(vals) => gen_array(vals, root.ty(), ctx),
        TypedExpression::Bool(val) => BasicValueEnum::IntValue(
            ctx.context
                .bool_type()
                .const_int(if *val { 1 } else { 0 }, false),
        ),
        TypedExpression::Int(val) => {
            BasicValueEnum::IntValue(ctx.context.i64_type().const_int(*val, false))
        }
        TypedExpression::UnaryOp(op, a) => match op {
            UnaryOp::Minus => BasicValueEnum::IntValue(ctx.builder.build_int_sub(
                ctx.context.i64_type().const_zero(),
                gen_expr(a, ctx).into_int_value(),
                "",
            )),
            UnaryOp::Not => BasicValueEnum::IntValue(ctx.builder.build_xor(
                ctx.context.bool_type().const_int(1, false),
                gen_expr(a, ctx).into_int_value(),
                "",
            )),
        },
        TypedExpression::BinaryOp(op, a, b) => match op {
            BinaryOp::ArrayDeref => gen_array_deref(a, b, ctx),
            BinaryOp::Add => BasicValueEnum::IntValue(ctx.builder.build_int_add(
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::Sub => BasicValueEnum::IntValue(ctx.builder.build_int_sub(
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::Multiply => BasicValueEnum::IntValue(ctx.builder.build_int_mul(
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::Divide => BasicValueEnum::IntValue(ctx.builder.build_int_signed_div(
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::LessThan => BasicValueEnum::IntValue(ctx.builder.build_int_compare(
                IntPredicate::SLT,
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::LessThanOrEqual => BasicValueEnum::IntValue(ctx.builder.build_int_compare(
                IntPredicate::SLE,
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::GreaterThan => BasicValueEnum::IntValue(ctx.builder.build_int_compare(
                IntPredicate::SGT,
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::GreaterThanOrEqual => {
                BasicValueEnum::IntValue(ctx.builder.build_int_compare(
                    IntPredicate::SGE,
                    gen_expr(a, ctx).into_int_value(),
                    gen_expr(b, ctx).into_int_value(),
                    "",
                ))
            }
            BinaryOp::Equal => BasicValueEnum::IntValue(ctx.builder.build_int_compare(
                IntPredicate::EQ,
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::NotEqual => BasicValueEnum::IntValue(ctx.builder.build_int_compare(
                IntPredicate::NE,
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            // using binary AND and OR because since bools are i1, it's the same as logical
            // operations
            BinaryOp::And => BasicValueEnum::IntValue(ctx.builder.build_and(
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
            BinaryOp::Or => BasicValueEnum::IntValue(ctx.builder.build_or(
                gen_expr(a, ctx).into_int_value(),
                gen_expr(b, ctx).into_int_value(),
                "",
            )),
        },
        TypedExpression::Conditional(cond, then, alt) => gen_conditional(cond, then, alt, ctx),
    }
}
fn gen_conditional(
    cond: &TypedNode,
    then: &TypedNode,
    alt: &TypedNode,
    ctx: &GenerationContext,
) -> BasicValueEnum {
    let cond_block = ctx
        .builder
        .get_insert_block()
        .unwrap_or_else(|| ctx.context.append_basic_block(&ctx.current_function, "if"));
    let then_block = ctx.context.insert_basic_block_after(&cond_block, "then");
    let else_block = ctx.context.insert_basic_block_after(&then_block, "else");
    let cont_block = ctx.context.insert_basic_block_after(&else_block, "fi");

    ctx.builder.position_at_end(&cond_block);
    let cond_val = gen_expr(cond, ctx).into_int_value();
    ctx.builder
        .build_conditional_branch(cond_val, &then_block, &else_block);

    ctx.builder.position_at_end(&then_block);
    build_noop(ctx);
    let then_val = gen_expr(then, ctx);
    ctx.builder.build_unconditional_branch(&cont_block);
    let then_exit = ctx.builder.get_insert_block().unwrap();

    ctx.builder.position_at_end(&else_block);
    build_noop(ctx);
    let else_val = gen_expr(alt, ctx);
    ctx.builder.build_unconditional_branch(&cont_block);
    let else_exit = ctx.builder.get_insert_block().unwrap();

    ctx.builder.position_at_end(&cont_block);
    let phi = ctx
        .builder
        .build_phi(then_val.as_basic_value_enum().get_type(), "phi");
    phi.add_incoming(&[(&then_val, &then_exit), (&else_val, &else_exit)]);

    phi.as_basic_value()
}

fn gen_array_deref(
    array: &TypedNode,
    index: &TypedNode,
    ctx: &GenerationContext,
) -> BasicValueEnum {
    ctx.builder
        .build_extract_value(
            gen_expr(array, ctx).into_array_value(),
            match index.expr() {
                TypedExpression::Int(val) => *val as u32,
                _ => unreachable!(),
            },
            "",
        )
        .expect("Invalid index")
}

fn gen_array(vals: &Vec<Box<TypedNode>>, ty: &Type, ctx: &GenerationContext) -> BasicValueEnum {
    let elems = vals
        .into_iter()
        .map(|val| gen_expr(val, ctx))
        .collect::<Vec<BasicValueEnum>>();

    let elem_type = if let Type::Array(_, t) = ty {
        t.real_type(&ctx.context)
    } else {
        panic!("Generating an array that isn't an Array")
    };

    let array_val = match elem_type {
        BasicTypeEnum::IntType(t) => t.const_array(
            &elems
                .into_iter()
                .map(|e| e.into_int_value())
                .collect::<Vec<IntValue>>(),
        ),
        BasicTypeEnum::ArrayType(t) => t.const_array(
            &elems
                .into_iter()
                .map(|e| e.into_array_value())
                .collect::<Vec<ArrayValue>>(),
        ),
        BasicTypeEnum::FloatType(_)
        | BasicTypeEnum::PointerType(_)
        | BasicTypeEnum::VectorType(_)
        | BasicTypeEnum::StructType(_) => panic!(),
    };
    BasicValueEnum::ArrayValue(array_val)
}

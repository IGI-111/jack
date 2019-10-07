use crate::ast::*;
use crate::types::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{ArrayValue, BasicValue, BasicValueEnum, FunctionValue, IntValue};
use inkwell::IntPredicate;

struct GenerationContext {
    pub builder: Builder,
    pub context: Context,
    pub function: FunctionValue,
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
        }
    }
}

pub fn generate(root: &TypedNode) {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let fn_type = match root.ty() {
        Type::Bool => context.bool_type().fn_type(&[], false),
        Type::Int => context.i64_type().fn_type(&[], false),
        Type::Array(_, _) => panic!("Can't return an Array from main"),
    };

    let function = module.add_function("main", fn_type, None);
    let ctx = GenerationContext {
        context,
        builder,
        function,
    };

    let main_block = ctx.context.append_basic_block(&function, "");
    ctx.builder.position_at_end(&main_block);
    let ret_val = match eval(root, &ctx) {
        BasicValueEnum::IntValue(val) => val,
        _ => unreachable!(),
    };

    ctx.builder.build_return(Some(&ret_val));

    module.print_to_file("out.bc").unwrap();
}

fn eval(root: &TypedNode, ctx: &GenerationContext) -> BasicValueEnum {
    match root.ty() {
        Type::Int | Type::Bool => BasicValueEnum::IntValue(eval_int(root, ctx)),
        Type::Array(_, _) => BasicValueEnum::ArrayValue(eval_arr(root, ctx)),
    }
}
fn eval_arr(root: &TypedNode, ctx: &GenerationContext) -> ArrayValue {
    match root.expr() {
        TypedExpression::Array(vals) => {
            let elems = vals
                .into_iter()
                .map(|val| eval(val, ctx))
                .collect::<Vec<BasicValueEnum>>();

            let elem_type = if let Type::Array(_, t) = root.ty() {
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
            array_val
        }
        TypedExpression::BinaryOp(op, a, b) => match op {
            BinaryOp::ArrayDeref => generate_array_deref(a, b, ctx).into_array_value(),
            _ => unreachable!(),
        },
        TypedExpression::Conditional(cond, then, alt) => {
            generate_conditional(cond, then, alt, ctx).into_array_value()
        }
        _ => unreachable!(),
    }
}

fn eval_int(root: &TypedNode, ctx: &GenerationContext) -> IntValue {
    match root.expr() {
        TypedExpression::Array(_) => unreachable!(),
        TypedExpression::Bool(val) => ctx
            .context
            .bool_type()
            .const_int(if *val { 1 } else { 0 }, false),
        TypedExpression::Int(val) => ctx.context.i64_type().const_int(*val, false),
        TypedExpression::UnaryOp(op, a) => match op {
            UnaryOp::Minus => {
                ctx.builder
                    .build_int_sub(ctx.context.i64_type().const_zero(), eval_int(a, ctx), "")
            }
            UnaryOp::Not => ctx.builder.build_xor(
                ctx.context.bool_type().const_int(1, false),
                eval_int(a, ctx),
                "",
            ),
        },
        TypedExpression::BinaryOp(op, a, b) => match op {
            BinaryOp::ArrayDeref => generate_array_deref(a, b, ctx).into_int_value(),
            BinaryOp::Add => ctx
                .builder
                .build_int_add(eval_int(a, ctx), eval_int(b, ctx), ""),
            BinaryOp::Sub => ctx
                .builder
                .build_int_sub(eval_int(a, ctx), eval_int(b, ctx), ""),
            BinaryOp::Multiply => ctx
                .builder
                .build_int_mul(eval_int(a, ctx), eval_int(b, ctx), ""),
            BinaryOp::Divide => {
                ctx.builder
                    .build_int_signed_div(eval_int(a, ctx), eval_int(b, ctx), "")
            }
            BinaryOp::LessThan => ctx.builder.build_int_compare(
                IntPredicate::SLT,
                eval_int(a, ctx),
                eval_int(b, ctx),
                "",
            ),
            BinaryOp::LessThanOrEqual => ctx.builder.build_int_compare(
                IntPredicate::SLE,
                eval_int(a, ctx),
                eval_int(b, ctx),
                "",
            ),
            BinaryOp::GreaterThan => ctx.builder.build_int_compare(
                IntPredicate::SGT,
                eval_int(a, ctx),
                eval_int(b, ctx),
                "",
            ),
            BinaryOp::GreaterThanOrEqual => ctx.builder.build_int_compare(
                IntPredicate::SGE,
                eval_int(a, ctx),
                eval_int(b, ctx),
                "",
            ),
            BinaryOp::Equal => ctx.builder.build_int_compare(
                IntPredicate::EQ,
                eval_int(a, ctx),
                eval_int(b, ctx),
                "",
            ),
            BinaryOp::NotEqual => ctx.builder.build_int_compare(
                IntPredicate::NE,
                eval_int(a, ctx),
                eval_int(b, ctx),
                "",
            ),
            // using binary AND and OR because since bools are i1, it's the same as logical
            // operations
            BinaryOp::And => ctx
                .builder
                .build_and(eval_int(a, ctx), eval_int(b, ctx), ""),
            BinaryOp::Or => ctx.builder.build_or(eval_int(a, ctx), eval_int(b, ctx), ""),
        },
        TypedExpression::Conditional(cond, then, alt) => {
            generate_conditional(cond, then, alt, ctx).into_int_value()
        }
    }
}

fn generate_conditional(
    cond: &TypedNode,
    then: &TypedNode,
    alt: &TypedNode,
    ctx: &GenerationContext,
) -> BasicValueEnum {
    let cond_block = ctx
        .builder
        .get_insert_block()
        .unwrap_or_else(|| ctx.context.append_basic_block(&ctx.function, "if"));
    let then_block = ctx.context.insert_basic_block_after(&cond_block, "then");
    let else_block = ctx.context.insert_basic_block_after(&then_block, "else");
    let cont_block = ctx.context.insert_basic_block_after(&else_block, "fi");

    ctx.builder.position_at_end(&cond_block);
    let cond_val = eval_int(cond, ctx);
    ctx.builder
        .build_conditional_branch(cond_val, &then_block, &else_block);

    ctx.builder.position_at_end(&then_block);
    generate_noop(ctx);
    let then_val = eval(then, ctx);
    ctx.builder.build_unconditional_branch(&cont_block);
    let then_exit = ctx.builder.get_insert_block().unwrap();

    ctx.builder.position_at_end(&else_block);
    generate_noop(ctx);
    let else_val = eval(alt, ctx);
    ctx.builder.build_unconditional_branch(&cont_block);
    let else_exit = ctx.builder.get_insert_block().unwrap();

    ctx.builder.position_at_end(&cont_block);
    let phi = ctx
        .builder
        .build_phi(then_val.as_basic_value_enum().get_type(), "phi");
    phi.add_incoming(&[(&then_val, &then_exit), (&else_val, &else_exit)]);

    phi.as_basic_value()
}

fn generate_array_deref(
    array: &TypedNode,
    index: &TypedNode,
    ctx: &GenerationContext,
) -> BasicValueEnum {
    ctx.builder
        .build_extract_value(
            eval_arr(array, ctx),
            match index.expr() {
                TypedExpression::Int(val) => *val as u32,
                _ => unreachable!(),
            },
            "",
        )
        .expect("Invalid index")
}

fn generate_noop(ctx: &GenerationContext) {
    ctx.builder.build_int_add(
        ctx.context.i64_type().const_zero(),
        ctx.context.i64_type().const_zero(),
        "",
    );
}

use super::GenerationContext;
use crate::ir::typed::*;
use crate::ir::*;
use array::{gen_array, gen_array_deref};
use conditional::gen_conditional;
use inkwell::values::BasicValueEnum;
use inkwell::IntPredicate;

mod array;
mod conditional;

pub(super) fn gen_expr(root: &TypedNode, ctx: &GenerationContext) -> BasicValueEnum {
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

use super::super::GenerationContext;
use super::gen_expr;
use crate::ir::typed::*;
use inkwell::values::{BasicValue, BasicValueEnum};

pub(super) fn gen_conditional(
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

fn build_noop(ctx: &GenerationContext) {
    ctx.builder.build_int_add(
        ctx.context.i64_type().const_zero(),
        ctx.context.i64_type().const_zero(),
        "",
    );
}

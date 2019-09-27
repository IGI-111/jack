use crate::ast::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::{FunctionValue, IntValue};
use inkwell::IntPredicate;

pub fn generate(expr: &Expression) {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let i64_type = context.i64_type();
    let fn_type = i64_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);
    let main_block = context.append_basic_block(&function, "");
    builder.position_at_end(&main_block);
    let ret_val = walk(expr, &builder, &context, &function);
    builder.build_return(Some(&ret_val));

    module.print_to_file("out.bc").unwrap();
}

pub fn walk(
    expr: &Expression,
    builder: &Builder,
    context: &Context,
    function: &FunctionValue,
) -> IntValue {
    match expr {
        Expression::Bool(val) => context
            .bool_type()
            .const_int(if *val { 1 } else { 0 }, false),
        Expression::Int(val) => context.i64_type().const_int(*val, false),
        Expression::UnaryOp(op, a) => match op {
            UnaryOp::Minus => builder.build_int_sub(
                context.i64_type().const_zero(),
                walk(a, builder, context, function),
                "",
            ),
            UnaryOp::Not => builder.build_xor(
                context.bool_type().const_int(1, false),
                walk(a, builder, context, function),
                "",
            ),
        },
        Expression::BinaryOp(op, a, b) => match op {
            BinaryOp::Add => builder.build_int_add(
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::Sub => builder.build_int_sub(
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::Multiply => builder.build_int_mul(
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::Divide => builder.build_int_signed_div(
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::LessThan => builder.build_int_compare(
                IntPredicate::SLT,
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::LessThanOrEqual => builder.build_int_compare(
                IntPredicate::SLE,
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::GreaterThan => builder.build_int_compare(
                IntPredicate::SGT,
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::GreaterThanOrEqual => builder.build_int_compare(
                IntPredicate::SGE,
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::Equal => builder.build_int_compare(
                IntPredicate::EQ,
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::NotEqual => builder.build_int_compare(
                IntPredicate::NE,
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            // using binary AND and OR because since bools are i1, it's the same as logical
            // operations
            BinaryOp::And => builder.build_and(
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
            BinaryOp::Or => builder.build_or(
                walk(a, builder, context, function),
                walk(b, builder, context, function),
                "",
            ),
        },
        Expression::Conditional(cond, then, alt) => {
            let cond_block = builder
                .get_insert_block()
                .unwrap_or_else(|| context.append_basic_block(&function, "if"));
            let then_block = context.insert_basic_block_after(&cond_block, "then");
            let else_block = context.insert_basic_block_after(&then_block, "else");
            let cont_block = context.insert_basic_block_after(&else_block, "fi");

            builder.position_at_end(&cond_block);
            let cond_val = walk(cond, builder, context, function);
            builder.build_conditional_branch(cond_val, &then_block, &else_block);

            builder.position_at_end(&then_block);
            builder.build_int_add(
                context.i64_type().const_zero(),
                context.i64_type().const_zero(),
                "",
            ); // noop
            let then_val = walk(then, builder, context, function);
            builder.build_unconditional_branch(&cont_block);
            let then_exit = builder.get_insert_block().unwrap();

            builder.position_at_end(&else_block);
            builder.build_int_add(
                context.i64_type().const_zero(),
                context.i64_type().const_zero(),
                "",
            ); // noop
            let else_val = walk(alt, builder, context, function);
            builder.build_unconditional_branch(&cont_block);
            let else_exit = builder.get_insert_block().unwrap();

            builder.position_at_end(&cont_block);
            let phi = builder.build_phi(context.i64_type(), "phi");
            phi.add_incoming(&[(&then_val, &then_exit), (&else_val, &else_exit)]);

            println!("{:?}", cond_block.get_terminator());

            phi.as_basic_value().into_int_value()
        }
        Expression::Id(_) => panic!(),
    }
}

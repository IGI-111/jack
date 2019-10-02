use crate::ast::*;
use crate::types::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::{FunctionValue, IntValue};
use inkwell::IntPredicate;

pub fn generate(root: &TypedNode) {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let i64_type = context.i64_type();
    let fn_type = i64_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);
    let main_block = context.append_basic_block(&function, "");
    builder.position_at_end(&main_block);
    let ret_val = eval_int(root, &builder, &context, &function);
    builder.build_return(Some(&ret_val));

    module.print_to_file("out.bc").unwrap();
}

// fn eval_ptr(
//     expr: &Expression<RawNode,
//     builder: &Builder,
//     context: &Context,
//     function: &FunctionValue,
// ) -> PointerValue {
//     match expr {
//         // Expression::Array(vals) => {
//         //     let elems = vals
//         //         .into_iter()
//         //         .map(|val| eval_int(val, builder, context, function))
//         //         .collect::<Vec<IntValue>>();
//         //     let array_type = context.i64_type().array_type(elems.len() as u32);

//         //     let ptr_val = builder.build_array_alloca(
//         //         array_type,
//         //         context.i64_type().const_int(elems.len() as u64, false),
//         //         "",
//         //     );
//         // }
//         val => panic!(format!("Unhandled expression: {:?}", val)),
//     }
// }

pub fn eval_int(
    root: &TypedNode,
    builder: &Builder,
    context: &Context,
    function: &FunctionValue,
) -> IntValue {
    match root.expr() {
        Expression::Bool(val) => context
            .bool_type()
            .const_int(if *val { 1 } else { 0 }, false),
        Expression::Int(val) => context.i64_type().const_int(*val, false),
        Expression::UnaryOp(op, a) => match op {
            UnaryOp::Minus => builder.build_int_sub(
                context.i64_type().const_zero(),
                eval_int(a, builder, context, function),
                "",
            ),
            UnaryOp::Not => builder.build_xor(
                context.bool_type().const_int(1, false),
                eval_int(a, builder, context, function),
                "",
            ),
        },
        Expression::BinaryOp(op, a, b) => match op {
            BinaryOp::Add => builder.build_int_add(
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::Sub => builder.build_int_sub(
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::Multiply => builder.build_int_mul(
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::Divide => builder.build_int_signed_div(
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::LessThan => builder.build_int_compare(
                IntPredicate::SLT,
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::LessThanOrEqual => builder.build_int_compare(
                IntPredicate::SLE,
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::GreaterThan => builder.build_int_compare(
                IntPredicate::SGT,
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::GreaterThanOrEqual => builder.build_int_compare(
                IntPredicate::SGE,
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::Equal => builder.build_int_compare(
                IntPredicate::EQ,
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::NotEqual => builder.build_int_compare(
                IntPredicate::NE,
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            // using binary AND and OR because since bools are i1, it's the same as logical
            // operations
            BinaryOp::And => builder.build_and(
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            BinaryOp::Or => builder.build_or(
                eval_int(a, builder, context, function),
                eval_int(b, builder, context, function),
                "",
            ),
            val => panic!(format!("Unhandled expression: {:?}", val)),
        },
        Expression::Conditional(cond, then, alt) => {
            let cond_block = builder
                .get_insert_block()
                .unwrap_or_else(|| context.append_basic_block(&function, "if"));
            let then_block = context.insert_basic_block_after(&cond_block, "then");
            let else_block = context.insert_basic_block_after(&then_block, "else");
            let cont_block = context.insert_basic_block_after(&else_block, "fi");

            builder.position_at_end(&cond_block);
            let cond_val = eval_int(cond, builder, context, function);
            builder.build_conditional_branch(cond_val, &then_block, &else_block);

            builder.position_at_end(&then_block);
            builder.build_int_add(
                context.i64_type().const_zero(),
                context.i64_type().const_zero(),
                "",
            ); // noop
            let then_val = eval_int(then, builder, context, function);
            builder.build_unconditional_branch(&cont_block);
            let then_exit = builder.get_insert_block().unwrap();

            builder.position_at_end(&else_block);
            builder.build_int_add(
                context.i64_type().const_zero(),
                context.i64_type().const_zero(),
                "",
            ); // noop
            let else_val = eval_int(alt, builder, context, function);
            builder.build_unconditional_branch(&cont_block);
            let else_exit = builder.get_insert_block().unwrap();

            builder.position_at_end(&cont_block);
            let phi = builder.build_phi(context.i64_type(), "phi");
            phi.add_incoming(&[(&then_val, &then_exit), (&else_val, &else_exit)]);

            phi.as_basic_value().into_int_value()
        }
        val => panic!(format!("Unhandled expression: {:?}", val)),
    }
}

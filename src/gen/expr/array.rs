use super::super::{GenerationContext, Realizable};
use super::gen_expr;
use crate::ir::sem::*;
use crate::ir::*;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{ArrayValue, BasicValueEnum, FloatValue, IntValue};

pub(super) fn gen_array(
    vals: &Vec<Box<SemNode>>,
    ty: &Type,
    ctx: &GenerationContext,
) -> BasicValueEnum {
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
        BasicTypeEnum::FloatType(t) => t.const_array(
            &elems
                .into_iter()
                .map(|e| e.into_float_value())
                .collect::<Vec<FloatValue>>(),
        ),
        BasicTypeEnum::ArrayType(t) => t.const_array(
            &elems
                .into_iter()
                .map(|e| e.into_array_value())
                .collect::<Vec<ArrayValue>>(),
        ),
        BasicTypeEnum::PointerType(_)
        | BasicTypeEnum::VectorType(_)
        | BasicTypeEnum::StructType(_) => panic!(),
    };
    BasicValueEnum::ArrayValue(array_val)
}

pub(super) fn gen_array_deref(
    array: &SemNode,
    index: &SemNode,
    ctx: &GenerationContext,
) -> BasicValueEnum {
    ctx.builder
        .build_extract_value(
            gen_expr(array, ctx).into_array_value(),
            match index.expr() {
                SemExpression::Int(val) => *val as u32,
                _ => unreachable!(),
            },
            "",
        )
        .expect("Invalid index")
}

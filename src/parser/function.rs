use super::expression::expression;
use super::types::type_literal;
use super::{identifier, sp};
use crate::ir::raw::RawFunction;
use crate::ir::Type;
use nom::bytes::complete::tag;
use nom::multi::separated_list;
use nom::sequence::tuple;
use nom::IResult;

fn argument(i: &str) -> IResult<&str, (String, Type)> {
    let (i, (name, _, _, _, arg_type)) = tuple((identifier, sp, tag(":"), sp, type_literal))(i)?;
    Ok((i, (name.to_string(), arg_type)))
}

pub fn function(i: &str) -> IResult<&str, RawFunction> {
    let (i, (_, _, name, _, _, args, _, _, _, _, return_type, _, _, _, root)) = tuple((
        tag("fun"),
        sp,
        identifier,
        sp,
        tag("("),
        separated_list(tuple((sp, tag(","), sp)), argument),
        tag(")"),
        sp,
        tag(":"),
        sp,
        type_literal,
        sp,
        tag("="),
        sp,
        expression,
    ))(i)?;
    let args_types = args.iter().cloned().map(Box::new).collect::<Vec<_>>();
    Ok((
        i,
        RawFunction {
            name: name.to_string(),
            root,
            ty: Type::Function(Box::new(return_type), args_types),
            args,
        },
    ))
}

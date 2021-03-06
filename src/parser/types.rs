use super::sp;
use crate::ir::Type;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::error::VerboseError;
use nom::sequence::tuple;
use nom::IResult;

fn array_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, (_, _, elem, _, _, _, len, _, _)) = tuple((
        tag("["),
        sp,
        type_literal,
        sp,
        tag(";"),
        sp,
        take_while1(move |c: char| c.is_numeric()),
        sp,
        tag("]"),
    ))(i)?;
    Ok((i, Type::Array(len.parse().unwrap(), Box::new(elem))))
}

fn bool_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("bool")(i)?;
    Ok((i, Type::Bool))
}

fn int_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("int")(i)?;
    Ok((i, Type::Int))
}

fn float_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("float")(i)?;
    Ok((i, Type::Float))
}

pub fn type_literal(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    alt((float_type, int_type, bool_type, array_type))(i)
}

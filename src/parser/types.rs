use super::sp;
use crate::types::Type;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::sequence::tuple;
use nom::IResult;

fn array_type(i: &str) -> IResult<&str, Type> {
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

fn bool_type(i: &str) -> IResult<&str, Type> {
    let (i, _) = tag("bool")(i)?;
    Ok((i, Type::Bool))
}

fn int_type(i: &str) -> IResult<&str, Type> {
    let (i, _) = tag("int")(i)?;
    Ok((i, Type::Int))
}

pub fn type_literal(i: &str) -> IResult<&str, Type> {
    alt((int_type, bool_type, array_type))(i)
}

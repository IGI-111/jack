use crate::ast::RawFunction;
use crate::types::Type;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::multi::separated_list;
use nom::sequence::tuple;
use nom::IResult;

mod expression;
mod types;

use expression::expression;
use types::type_literal;

fn sp(i: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn identifier(i: &str) -> IResult<&str, &str> {
    take_while1(move |c: char| c.is_alphanumeric())(i)
}

fn function(i: &str) -> IResult<&str, RawFunction> {
    let (i, (_, _, name, _, _, _, _, _, _, _, return_type, _, root, _, _)) = tuple((
        tag("fn"),
        sp,
        identifier,
        sp,
        tag("("),
        sp,
        tag(")"),
        sp,
        tag(":"),
        sp,
        type_literal,
        sp,
        expression,
        sp,
        tag("end"),
    ))(i)?;
    Ok((
        i,
        RawFunction {
            name: name.to_string(),
            root,
            ty: Type::Function(Box::new(return_type)),
        },
    ))
}

pub fn program(i: &str) -> IResult<&str, Vec<RawFunction>> {
    let (i, (_, exprs, _)) = tuple((sp, separated_list(sp, function), sp))(i)?;
    Ok((i, exprs))
}

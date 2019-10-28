use crate::ast::RawFunction;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::sequence::tuple;
use nom::IResult;

mod expression;

use expression::expression;

fn sp(i: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn identifier(i: &str) -> IResult<&str, &str> {
    take_while1(move |c: char| c.is_alphanumeric())(i)
}

pub fn function(i: &str) -> IResult<&str, RawFunction> {
    let (i, (_, _, name, _, _, _, _, _, root, _, _)) = tuple((
        tag("fn"),
        sp,
        identifier,
        sp,
        tag("("),
        sp,
        tag(")"),
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
        },
    ))
}

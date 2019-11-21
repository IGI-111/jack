use crate::ir::raw::RawFunction;
use nom::bytes::complete::{take_while, take_while1};
use nom::combinator::all_consuming;
use nom::error::VerboseError;
use nom::multi::separated_list;
use nom::sequence::tuple;
use nom::IResult;

mod expression;
mod function;
mod types;

use function::function;

fn sp(i: &str) -> IResult<&str, &str, VerboseError<&str>> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn identifier(i: &str) -> IResult<&str, &str, VerboseError<&str>> {
    take_while1(move |c: char| c.is_alphanumeric())(i)
}

pub fn program(i: &str) -> IResult<&str, Vec<RawFunction>, VerboseError<&str>> {
    let (i, (_, funs, _)) = all_consuming(tuple((sp, separated_list(sp, function), sp)))(i)?;
    Ok((i, funs))
}

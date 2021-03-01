use crate::ir::raw::RawFunction;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while, take_while1};
use nom::combinator::all_consuming;
use nom::error::VerboseError;
use nom::multi::{many0, separated_list};
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
    take_while1(move |c: char| c.is_alphanumeric() || "_'".contains(c))(i)
}

fn comment(i: &str) -> IResult<&str, &str, VerboseError<&str>> {
    let (i, (_, text, _)) = tuple((tag("(*"), take_until("*)"), tag("*)")))(i)?;
    Ok((i, text))
}

fn non_comment(i: &str) -> IResult<&str, &str, VerboseError<&str>> {
    alt((take_until("(*"), take_while(|_| true)))(i)
}

pub fn comments_ommited(i: &str) -> IResult<&str, String, VerboseError<&str>> {
    let (i, (_, r, _)) = all_consuming(tuple((
        many0(comment),
        separated_list(comment, non_comment),
        many0(comment),
    )))(i)?;
    Ok((i, r.join("")))
}

pub fn program(i: &str) -> IResult<&str, Vec<RawFunction>, VerboseError<&str>> {
    let (i, (_, funs, _)) = all_consuming(tuple((sp, separated_list(sp, function), sp)))(&i)?;
    Ok((i, funs))
}

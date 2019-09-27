use nom::bytes::complete::take_while;
use nom::bytes::complete::take_while1;
use nom::IResult;

mod expression;

pub use expression::expression;

fn sp(i: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn identifier(i: &str) -> IResult<&str, &str> {
    take_while1(move |c: char| c.is_alphanumeric())(i)
}

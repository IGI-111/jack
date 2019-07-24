use super::sp;
use crate::ast::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while1;
use nom::combinator::opt;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;

fn int_literal(i: &str) -> IResult<&str, Expression> {
    let (i, num) = take_while1(move |c: char| c.is_numeric())(i)?;
    Ok((i, Expression::Int(num.parse().unwrap())))
}

fn bool_literal(i: &str) -> IResult<&str, Expression> {
    let (i, val) = alt((tag("true"), tag("false")))(i)?;
    Ok((i, Expression::Bool(val == "true")))
}
fn literal(i: &str) -> IResult<&str, Expression> {
    alt((int_literal, bool_literal))(i)
}

fn parens(i: &str) -> IResult<&str, Expression> {
    let (i, (_, _, expr, _, _)) = tuple((tag("("), sp, expression, sp, tag(")")))(i)?;
    Ok((i, expr))
}

fn identifier(i: &str) -> IResult<&str, Expression> {
    let (i, id) = super::identifier(i)?;
    Ok((i, Expression::Id(id.to_string())))
}

fn terminal(i: &str) -> IResult<&str, Expression> {
    alt((parens, literal, identifier))(i)
}

fn factor(i: &str) -> IResult<&str, Expression> {
    let (i, (ops, expr)) = tuple((many0(tuple((alt((tag("-"), tag("!"))), sp))), terminal))(i)?;
    Ok((
        i,
        ops.into_iter().rev().fold(expr, |prev, (op, _)| match op {
            "-" => Expression::UnaryOp(UnaryOp::Minus, Box::new(prev)),
            "!" => Expression::UnaryOp(UnaryOp::Not, Box::new(prev)),
            _ => unreachable!(),
        }),
    ))
}

fn term(i: &str) -> IResult<&str, Expression> {
    let (i, (first, remainder)) = tuple((
        factor,
        many0(tuple((sp, alt((tag("*"), tag("/"))), sp, factor))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, _, next)| match op {
                "*" => Expression::BinaryOp(BinaryOp::Multiply, Box::new(prev), Box::new(next)),
                "/" => Expression::BinaryOp(BinaryOp::Divide, Box::new(prev), Box::new(next)),
                _ => unreachable!(),
            }),
    ))
}

fn additive_expr(i: &str) -> IResult<&str, Expression> {
    let (i, (first, remainder)) = tuple((
        term,
        many0(tuple((sp, alt((tag("+"), tag("-"))), sp, term))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, _, next)| match op {
                "+" => Expression::BinaryOp(BinaryOp::Add, Box::new(prev), Box::new(next)),
                "-" => Expression::BinaryOp(BinaryOp::Sub, Box::new(prev), Box::new(next)),
                _ => unreachable!(),
            }),
    ))
}

fn relational_expr(i: &str) -> IResult<&str, Expression> {
    let (i, (first, remainder)) = tuple((
        additive_expr,
        many0(tuple((
            sp,
            alt((tag("<"), tag(">"))),
            opt(tag("=")),
            sp,
            term,
        ))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, eq, _, next)| match (op, eq) {
                ("<", None) => {
                    Expression::BinaryOp(BinaryOp::LessThan, Box::new(prev), Box::new(next))
                }
                ("<", Some("=")) => {
                    Expression::BinaryOp(BinaryOp::LessThanOrEqual, Box::new(prev), Box::new(next))
                }
                (">", None) => {
                    Expression::BinaryOp(BinaryOp::GreaterThan, Box::new(prev), Box::new(next))
                }
                (">", Some("=")) => Expression::BinaryOp(
                    BinaryOp::GreaterThanOrEqual,
                    Box::new(prev),
                    Box::new(next),
                ),
                _ => unreachable!(),
            }),
    ))
}

fn equality_expr(i: &str) -> IResult<&str, Expression> {
    let (i, (first, remainder)) = tuple((
        relational_expr,
        many0(tuple((
            sp,
            alt((tag("=="), tag("!="))),
            sp,
            relational_expr,
        ))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, _, next)| match op {
                "==" => Expression::BinaryOp(BinaryOp::Equal, Box::new(prev), Box::new(next)),
                "!=" => Expression::BinaryOp(BinaryOp::NotEqual, Box::new(prev), Box::new(next)),
                _ => unreachable!(),
            }),
    ))
}

fn logical_and_expr(i: &str) -> IResult<&str, Expression> {
    let (i, (first, remainder)) = tuple((
        equality_expr,
        many0(tuple((sp, tag("&&"), sp, equality_expr))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, _op, _, next)| {
                Expression::BinaryOp(BinaryOp::And, Box::new(prev), Box::new(next))
            }),
    ))
}

fn logical_or_expr(i: &str) -> IResult<&str, Expression> {
    let (i, (first, remainder)) = tuple((
        logical_and_expr,
        many0(tuple((sp, tag("||"), sp, logical_and_expr))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, _op, _, next)| {
                Expression::BinaryOp(BinaryOp::Or, Box::new(prev), Box::new(next))
            }),
    ))
}

fn conditional_expr(i: &str) -> IResult<&str, Expression> {
    let (i, (_, _, cond, _, _, _, exp, _, _, _, _, _, _, _, alt, _, _)) = tuple((
        tag("if"),
        sp,
        expression,
        sp,
        tag("{"),
        sp,
        expression,
        sp,
        tag("}"),
        sp,
        tag("else"),
        sp,
        tag("{"),
        sp,
        expression,
        sp,
        tag("}"),
    ))(i)?;

    Ok((
        i,
        Expression::Conditional(Box::new(cond), Box::new(exp), Box::new(alt)),
    ))
}

pub fn expression(i: &str) -> IResult<&str, Expression> {
    alt((conditional_expr, logical_or_expr))(i)
}

#[test]
fn expression_test() {
    assert_eq!(
        relational_expr("1 < 2"),
        Ok((
            "",
            Expression::BinaryOp(
                BinaryOp::LessThan,
                Box::new(Expression::Int(1)),
                Box::new(Expression::Int(2))
            )
        ))
    );
    assert_eq!(
        relational_expr("1 <= 2"),
        Ok((
            "",
            Expression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(Expression::Int(1)),
                Box::new(Expression::Int(2))
            )
        ))
    );
    assert_eq!(
        relational_expr("1 + 1 <= 2"),
        Ok((
            "",
            Expression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(Expression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(Expression::Int(1)),
                    Box::new(Expression::Int(1))
                )),
                Box::new(Expression::Int(2))
            )
        ))
    );
    assert_eq!(
        relational_expr("1 + -1 <= 2"),
        Ok((
            "",
            Expression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(Expression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(Expression::Int(1)),
                    Box::new(Expression::UnaryOp(
                        UnaryOp::Minus,
                        Box::new(Expression::Int(1))
                    ))
                )),
                Box::new(Expression::Int(2))
            )
        ))
    );
}

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

fn terminal(i: &str) -> IResult<&str, Expression> {
    alt((parens, literal))(i)
}

fn factor(i: &str) -> IResult<&str, Expression> {
    let (i, (ops, expr)) = tuple((many0(tuple((alt((tag("-"), tag("!"))), sp))), terminal))(i)?;
    Ok((
        i,
        ops.into_iter().rev().fold(expr, |prev, (op, _)| match op {
            "-" => Expression::Minus(Box::new(prev)),
            "!" => Expression::Not(Box::new(prev)),
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
                "*" => Expression::Multiply(Box::new(prev), Box::new(next)),
                "/" => Expression::Divide(Box::new(prev), Box::new(next)),
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
                "+" => Expression::Add(Box::new(prev), Box::new(next)),
                "-" => Expression::Sub(Box::new(prev), Box::new(next)),
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
                ("<", None) => Expression::LessThan(Box::new(prev), Box::new(next)),
                ("<", Some("=")) => Expression::LessThanOrEqual(Box::new(prev), Box::new(next)),
                (">", None) => Expression::GreaterThan(Box::new(prev), Box::new(next)),
                (">", Some("=")) => Expression::GreaterThanOrEqual(Box::new(prev), Box::new(next)),
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
                "==" => Expression::Equal(Box::new(prev), Box::new(next)),
                "!=" => Expression::NotEqual(Box::new(prev), Box::new(next)),
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
                Expression::And(Box::new(prev), Box::new(next))
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
                Expression::Or(Box::new(prev), Box::new(next))
            }),
    ))
}

fn conditional_expr(i: &str) -> IResult<&str, Expression> {
    let (i, (_, _, cond, _, _, _, exp, _, _, alt)) = tuple((
        tag("if"),
        sp,
        expression,
        sp,
        tag("{"),
        sp,
        expression,
        sp,
        tag("}"),
        opt(tuple((
            sp,
            tag("else"),
            sp,
            tag("{"),
            sp,
            expression,
            sp,
            tag("}"),
        ))),
    ))(i)?;

    Ok((
        i,
        Expression::Conditional(
            Box::new(cond),
            Box::new(exp),
            Box::new(alt.map(|(_, _, _, _, _, alt, _, _)| alt)),
        ),
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
            Expression::LessThan(Box::new(Expression::Int(1)), Box::new(Expression::Int(2)))
        ))
    );
    assert_eq!(
        relational_expr("1 <= 2"),
        Ok((
            "",
            Expression::LessThanOrEqual(Box::new(Expression::Int(1)), Box::new(Expression::Int(2)))
        ))
    );
    assert_eq!(
        relational_expr("1 + 1 <= 2"),
        Ok((
            "",
            Expression::LessThanOrEqual(
                Box::new(Expression::Add(
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
            Expression::LessThanOrEqual(
                Box::new(Expression::Add(
                    Box::new(Expression::Int(1)),
                    Box::new(Expression::Minus(Box::new(Expression::Int(1))))
                )),
                Box::new(Expression::Int(2))
            )
        ))
    );
}

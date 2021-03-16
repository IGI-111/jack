use super::{identifier, sp};
use crate::ir::raw::*;
use crate::ir::*;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::combinator::opt;
use nom::error::VerboseError;
use nom::multi::{many0, separated_list0};
use nom::sequence::tuple;
use nom::IResult;

fn int_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, num) = take_while1(move |c: char| c.is_numeric())(i)?;
    Ok((i, RawNode::new(RawExpression::Int(num.parse().unwrap()))))
}

fn float_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (u, _, f)) = tuple((
        take_while1(move |c: char| c.is_numeric()),
        tag("."),
        take_while1(move |c: char| c.is_numeric()),
    ))(i)?;
    Ok((
        i,
        RawNode::new(RawExpression::Float(
            format!("{}.{}", u, f).parse().unwrap(),
        )),
    ))
}

fn bool_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, val) = alt((tag("true"), tag("false")))(i)?;
    Ok((i, RawNode::new(RawExpression::Bool(val == "true"))))
}

fn array_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, exprs, _, _)) = tuple((
        tag("["),
        sp,
        separated_list0(tuple((sp, tag(","), sp)), expression),
        sp,
        tag("]"),
    ))(i)?;
    Ok((i, RawNode::new(RawExpression::Array(exprs.to_vec()))))
}

fn literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((float_literal, int_literal, bool_literal, array_literal))(i)
}

fn parens(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, expr, _, _)) = tuple((tag("("), sp, expression, sp, tag(")")))(i)?;
    Ok((i, expr))
}

fn fun_call(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (id, _, _, _, args, _, _)) = tuple((
        identifier,
        sp,
        tag("("),
        sp,
        separated_list0(tuple((sp, tag(","), sp)), expression),
        sp,
        tag(")"),
    ))(i)?;
    Ok((
        i,
        RawNode::new(RawExpression::FunCall(id.to_string(), args.to_vec())),
    ))
}

fn identifier_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, id) = identifier(i)?;
    Ok((i, RawNode::new(RawExpression::Id(id.to_string()))))
}

fn terminal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((parens, literal, fun_call, identifier_expr))(i)
}

fn deref_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (ops, expr)) = tuple((many0(tuple((alt((tag("-"), tag("!"))), sp))), terminal))(i)?;
    Ok((
        i,
        ops.into_iter().rev().fold(expr, |prev, (op, _)| match op {
            "-" => RawNode::new(RawExpression::UnaryOp(UnaryOp::Minus, Box::new(prev))),
            "!" => RawNode::new(RawExpression::UnaryOp(UnaryOp::Not, Box::new(prev))),
            _ => unreachable!(),
        }),
    ))
}

fn factor(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        deref_expr,
        many0(tuple((sp, tag("["), sp, int_literal, sp, tag("]")))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, _, _, next, _, _)| {
                RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::ArrayDeref,
                    Box::new(prev),
                    Box::new(next),
                ))
            }),
    ))
}

fn term(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        factor,
        many0(tuple((sp, alt((tag("*"), tag("/"))), sp, factor))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, _, next)| match op {
                "*" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Multiply,
                    Box::new(prev),
                    Box::new(next),
                )),
                "/" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Divide,
                    Box::new(prev),
                    Box::new(next),
                )),
                _ => unreachable!(),
            }),
    ))
}

fn additive_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        term,
        many0(tuple((sp, alt((tag("+"), tag("-"))), sp, term))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, _, next)| match op {
                "+" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(prev),
                    Box::new(next),
                )),
                "-" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Sub,
                    Box::new(prev),
                    Box::new(next),
                )),
                _ => unreachable!(),
            }),
    ))
}

fn relational_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
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
                ("<", None) => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::LessThan,
                    Box::new(prev),
                    Box::new(next),
                )),
                ("<", Some("=")) => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::LessThanOrEqual,
                    Box::new(prev),
                    Box::new(next),
                )),
                (">", None) => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::GreaterThan,
                    Box::new(prev),
                    Box::new(next),
                )),
                (">", Some("=")) => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::GreaterThanOrEqual,
                    Box::new(prev),
                    Box::new(next),
                )),
                _ => unreachable!(),
            }),
    ))
}

fn equality_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
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
                "==" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Equal,
                    Box::new(prev),
                    Box::new(next),
                )),
                "!=" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::NotEqual,
                    Box::new(prev),
                    Box::new(next),
                )),
                _ => unreachable!(),
            }),
    ))
}

fn logical_and_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        equality_expr,
        many0(tuple((sp, tag("&&"), sp, equality_expr))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, _op, _, next)| {
                RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::And,
                    Box::new(prev),
                    Box::new(next),
                ))
            }),
    ))
}

fn logical_or_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        logical_and_expr,
        many0(tuple((sp, tag("||"), sp, logical_and_expr))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, _op, _, next)| {
                RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Or,
                    Box::new(prev),
                    Box::new(next),
                ))
            }),
    ))
}

fn conditional_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, top_cond, _, _, _, top_exp, _, elifs, _, _, top_alt)) = tuple((
        tag("if"),
        sp,
        expression,
        sp,
        tag("then"),
        sp,
        expression,
        sp,
        many0(tuple((
            tag("else"),
            sp,
            tag("if"),
            sp,
            expression,
            sp,
            tag("then"),
            sp,
            expression,
            sp,
        ))),
        tag("else"),
        sp,
        expression,
    ))(i)?;

    let mut node = top_alt;
    for (_, _, _, _, cond, _, _, _, expr, _) in elifs.into_iter().rev() {
        node = RawNode::new(RawExpression::Conditional(
            Box::new(cond),
            Box::new(expr),
            Box::new(node),
        ));
    }
    node = RawNode::new(RawExpression::Conditional(
        Box::new(top_cond),
        Box::new(top_exp),
        Box::new(node),
    ));

    Ok((i, node))
}

fn let_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, id, _, _, _, val, _, _, _, expr)) = tuple((
        tag("let"),
        sp,
        identifier,
        sp,
        tag("="),
        sp,
        expression,
        sp,
        tag("in"),
        sp,
        expression,
    ))(i)?;
    Ok((
        i,
        RawNode::new(RawExpression::Let(
            id.to_string(),
            Box::new(val),
            Box::new(expr),
        )),
    ))
}

pub fn expression(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((let_expr, conditional_expr, logical_or_expr))(i)
}

#[test]
fn expression_test() {
    assert_eq!(
        relational_expr("1 < 2"),
        Ok((
            "",
            RawNode::new(RawExpression::BinaryOp(
                BinaryOp::LessThan,
                Box::new(RawNode::new(RawExpression::Int(1))),
                Box::new(RawNode::new(RawExpression::Int(2)))
            ))
        ))
    );
    assert_eq!(
        relational_expr("1 <= 2"),
        Ok((
            "",
            RawNode::new(RawExpression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(RawNode::new(RawExpression::Int(1))),
                Box::new(RawNode::new(RawExpression::Int(2)))
            ))
        ))
    );
    assert_eq!(
        relational_expr("1 + 1 <= 2"),
        Ok((
            "",
            RawNode::new(RawExpression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(RawNode::new(RawExpression::Int(1))),
                    Box::new(RawNode::new(RawExpression::Int(1)))
                ))),
                Box::new(RawNode::new(RawExpression::Int(2)))
            ))
        ))
    );
    assert_eq!(
        relational_expr("1 + -1 <= 2"),
        Ok((
            "",
            RawNode::new(RawExpression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(RawNode::new(RawExpression::Int(1))),
                    Box::new(RawNode::new(RawExpression::UnaryOp(
                        UnaryOp::Minus,
                        Box::new(RawNode::new(RawExpression::Int(1)))
                    )))
                ))),
                Box::new(RawNode::new(RawExpression::Int(2)))
            ))
        ))
    );
}

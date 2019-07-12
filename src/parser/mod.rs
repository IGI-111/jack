use crate::ast::*;
use expression::expression;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while;
use nom::bytes::complete::take_while1;
use nom::multi::separated_list;
use nom::sequence::tuple;
use nom::IResult;

mod expression;

pub fn parser(i: &str) -> IResult<&str, Module> {
    let (i, (_, imports, _, export, _)) = tuple((sp, import_list, sp, export, sp))(i)?;
    Ok((i, Module { imports, export }))
}

fn sp(i: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn identifier(i: &str) -> IResult<&str, &str> {
    take_while1(move |c: char| c.is_alphanumeric())(i)
}

fn import(i: &str) -> IResult<&str, Import> {
    let (i, (_, _, id)) = tuple((tag("import"), sp, identifier))(i)?;
    Ok((
        i,
        Import {
            module: id.to_string(),
        },
    ))
}

fn export(i: &str) -> IResult<&str, Export> {
    let (i, (_, _, expr)) = tuple((tag("export"), sp, expression))(i)?;
    Ok((i, Export { expr }))
}

fn import_list(i: &str) -> IResult<&str, Vec<Import>> {
    separated_list(sp, import)(i)
}

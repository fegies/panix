use std::{borrow::Cow, collections::HashMap};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::{
        complete::{multispace0, multispace1},
        is_alphanumeric,
    },
    combinator::{complete, cut, map, opt},
    multi::fold_many1,
    sequence::{delimited, terminated},
    IResult,
};

type NixString = String;
type ParseResult<'a, R> = IResult<&'a [u8], R>;

#[derive(Debug)]
enum BasicValue<'a> {
    String(Cow<'a, str>),
    Bool(bool),
    Null,
    Int(i64),
    Float(f64),
    Path(NixString),
}

#[derive(Debug)]
struct Attrset<'a> {
    is_recursive: bool,
    attrs: HashMap<&'a str, NixExpr<'a>>,
}
#[derive(Debug)]
struct List {}

#[derive(Debug)]
enum CompoundValue<'a> {
    Attrset(Attrset<'a>),
    List(List),
}

#[derive(Debug)]
struct LetInExpr<'a> {
    bindings: HashMap<&'a str, NixExpr<'a>>,
    body: Box<NixExpr<'a>>,
}

#[derive(Debug)]
struct WithExpr<'a> {
    binding: Box<NixExpr<'a>>,
    body: Box<NixExpr<'a>>,
}

#[derive(Debug)]
struct Lambda<'a> {
    args: LambdaArgs<'a>,
    body: Box<NixExpr<'a>>,
}

#[derive(Debug)]
enum Code<'a> {
    LetInExpr(LetInExpr<'a>),
    RecAttrset,
    ValueReference,
    WithExpr(WithExpr<'a>),
    Lambda(Lambda<'a>),
}

#[derive(Debug)]
enum NixExpr<'a> {
    BasicValue,
    CompoundValue(CompoundValue<'a>),
    Code(Code<'a>),
}

#[derive(Debug)]
enum LambdaArgs<'a> {
    SimpleBinding(&'a str),
    AttrsetBinding {
        includes_rest_pattern: bool,
        total_name: Option<&'a str>,
        args: HashMap<&'a str, Option<Box<NixExpr<'a>>>>,
    },
}

fn parse_attrset_contents(input: &[u8]) -> ParseResult<Attrset> {
    todo!()
}

fn parse_list_contents(input: &[u8]) -> ParseResult<List> {
    todo!()
}

fn parse_binding(input: &[u8]) -> ParseResult<(&str, NixExpr)> {
    todo!()
}

fn parse_let(input: &[u8]) -> ParseResult<Code> {
    fn inner(input: &[u8]) -> ParseResult<LetInExpr> {
        let mut bindings = HashMap::new();
        let (input, _) = fold_many1(
            parse_binding,
            || (),
            |_, (name, value)| {
                bindings.insert(name, value);
            },
        )(input)?;

        let (input, _) = tag("in")(input)?;
        let (input, _) = multispace1(input)?;

        let (input, body) = parse_expr(input)?;
        Ok((
            input,
            LetInExpr {
                bindings,
                body: Box::new(body),
            },
        ))
    }

    let (input, _) = tag("let")(input)?;
    let (input, _) = multispace1(input)?;
    cut(map(inner, Code::LetInExpr))(input)
}

fn parse_with(input: &[u8]) -> ParseResult<Code> {
    fn inner(input: &[u8]) -> ParseResult<WithExpr> {
        let (input, binding) = parse_expr(input)?;
        let (input, _) = tag(";")(input)?;
        let (input, body) = parse_expr(input)?;

        Ok((
            input,
            WithExpr {
                binding: Box::new(binding),
                body: Box::new(body),
            },
        ))
    }

    let (input, _) = tag("with")(input)?;
    let (input, _) = multispace1(input)?;

    cut(map(inner, Code::WithExpr))(input)
}

fn parse_ident(input: &[u8]) -> ParseResult<&str> {
    let (input, ident) = take_while1(is_alphanumeric)(input)?;

    // SAFETY: This is always safe because the parser above guaranteed it is only composed of ascii chars
    // and ascii is always valid unicode
    let ident = unsafe { std::str::from_utf8_unchecked(ident) };

    Ok((input, ident))
}

fn parse_function(input: &[u8]) -> ParseResult<Code> {
    fn parse_args(input: &[u8]) -> ParseResult<LambdaArgs> {
        let (input, ident) = opt(parse_ident)(input)?;
        // we got an ident. It may either be the total binding for attrset args or an ident by itself
        if let Some(ident) = ident {
        } else {
            parse_attrset_bindings()
        }
        todo!()
    }

    let (input, args) = parse_args(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, body) = parse_expr(input)?;

    Ok((
        input,
        Code::Lambda(Lambda {
            args,
            body: Box::new(body),
        }),
    ))
}

fn parse_expr(input: &[u8]) -> ParseResult<NixExpr> {
    let (input, _) = multispace0(input)?;

    alt((
        map(
            delimited(tag("["), cut(parse_list_contents), tag("]")),
            |list| NixExpr::CompoundValue(CompoundValue::List(list)),
        ),
        map(parse_let, NixExpr::Code),
        map(parse_with, NixExpr::Code),
        map(parse_function, NixExpr::Code),
        map(
            delimited(tag("{"), cut(parse_attrset_contents), tag("}")),
            |set| NixExpr::CompoundValue(CompoundValue::Attrset(set)),
        ),
    ))(input)
}

pub fn parse_nix(input: &[u8]) {
    let mut parser = complete(terminated(parse_expr, multispace0));

    match parser(input) {
        Ok(value) => println!("{value:?}"),
        Err(e) => {
            println!("failed to parse")
        }
    }
}

use std::{collections::HashMap, fmt::Write};

pub enum NixString<'a> {
    Literal(&'a str),
    Composite(Vec<&'a str>),
    Interpolated(Vec<InterpolationEntry<'a>>),
    Empty,
}

#[derive(Debug)]
pub enum InterpolationEntry<'a> {
    LiteralPiece(&'a str),
    Expression(NixExpr<'a>),
}

impl core::fmt::Debug for NixString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(arg0) => arg0.fmt(f),
            Self::Composite(arg0) => {
                f.write_char('"')?;
                for s in arg0 {
                    f.write_str(s)?;
                }
                f.write_char('"')?;
                Ok(())
            }
            Self::Interpolated(vals) => f
                .debug_struct("Interpolation")
                .field("entries", vals)
                .finish(),
            Self::Empty => "".fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum BasicValue<'a> {
    String(NixString<'a>),
    Bool(bool),
    Null,
    Int(i64),
    Float(f64),
    Path(NixString<'a>),
}

#[derive(Debug)]
pub struct Attrset<'a> {
    pub is_recursive: bool,
    pub attrs: HashMap<&'a str, NixExpr<'a>>,
}
#[derive(Debug)]
pub struct List<'a> {
    pub entries: Vec<NixExpr<'a>>,
}

#[derive(Debug)]
pub enum CompoundValue<'a> {
    Attrset(Attrset<'a>),
    List(List<'a>),
}

#[derive(Debug)]
pub struct LetInExpr<'a> {
    pub bindings: HashMap<&'a str, NixExpr<'a>>,
    pub body: Box<NixExpr<'a>>,
}

#[derive(Debug)]
pub struct IfExpr<'a> {
    pub condition: Box<NixExpr<'a>>,
    pub truthy_case: Box<NixExpr<'a>>,
    pub falsy_case: Box<NixExpr<'a>>,
}

#[derive(Debug)]
pub struct WithExpr<'a> {
    pub binding: Box<NixExpr<'a>>,
    pub body: Box<NixExpr<'a>>,
}

#[derive(Debug)]
pub struct Lambda<'a> {
    pub args: LambdaArgs<'a>,
    pub body: Box<NixExpr<'a>>,
}

#[derive(Debug)]
pub enum Op<'a> {
    AttrRef {
        left: Box<NixExpr<'a>>,
        name: &'a str,
    },
    Call {
        function: Box<NixExpr<'a>>,
        arg: Box<NixExpr<'a>>,
    },
    Add {
        left: Box<NixExpr<'a>>,
        right: Box<NixExpr<'a>>,
    },
}

#[derive(Debug)]
pub enum Code<'a> {
    LetInExpr(LetInExpr<'a>),
    RecAttrset,
    ValueReference { ident: &'a str },
    WithExpr(WithExpr<'a>),
    Lambda(Lambda<'a>),
    Op(Op<'a>),
    IfExpr(IfExpr<'a>),
}

#[derive(Debug)]
pub enum NixExpr<'a> {
    BasicValue(BasicValue<'a>),
    CompoundValue(CompoundValue<'a>),
    Code(Code<'a>),
}

#[derive(Debug)]
pub struct LambdaAttrsetArgs<'a> {
    pub bindings: HashMap<&'a str, Option<NixExpr<'a>>>,
    pub includes_rest_pattern: bool,
}

#[derive(Debug)]
pub enum LambdaArgs<'a> {
    SimpleBinding(&'a str),
    AttrsetBinding {
        total_name: Option<&'a str>,
        args: LambdaAttrsetArgs<'a>,
    },
}

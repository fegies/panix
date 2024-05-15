use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

pub mod impls;

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

#[derive(Debug)]
pub enum BasicValue<'a> {
    String(NixString<'a>),
    Bool(bool),
    Null,
    Int(i64),
    Float(f64),
    Path(NixString<'a>),
}

/// This is the key to an attribute set.
#[derive(Debug)]
pub enum AttrsetKey<'a> {
    /// just a single-level item
    Single(NixString<'a>),
    /// a multi-level entry.
    /// the parsed representation of the nested attrset shorthand.
    Multi(Vec<NixString<'a>>),
}

#[derive(Debug)]
pub struct Attrset<'a> {
    pub is_recursive: bool,
    pub inherit_keys: HashSet<&'a str>,
    pub attrs: Vec<(AttrsetKey<'a>, NixExpr<'a>)>,
}
impl<'t> Attrset<'t> {
    pub(crate) fn empty() -> Attrset<'t> {
        Self {
            is_recursive: false,
            inherit_keys: HashSet::new(),
            attrs: Vec::new(),
        }
    }
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
pub enum BinopOpcode {
    Add,
    ListConcat,
    AttrsetMerge,
    Equals,
    NotEqual,
    Subtract,
    Multiply,
    Divide,
    LogicalOr,
    LogicalAnd,
    LessThanOrEqual,
    LessThanStrict,
    GreaterOrRequal,
    GreaterThanStrict,
    LogicalImplication,
}

#[derive(Debug)]
pub enum Op<'a> {
    AttrRef {
        left: Box<NixExpr<'a>>,
        name: NixString<'a>,
    },
    Call {
        function: Box<NixExpr<'a>>,
        arg: Box<NixExpr<'a>>,
    },
    Binop {
        left: Box<NixExpr<'a>>,
        right: Box<NixExpr<'a>>,
        opcode: BinopOpcode,
    },
    HasAttr {
        left: Box<NixExpr<'a>>,
        path: AttrsetKey<'a>,
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

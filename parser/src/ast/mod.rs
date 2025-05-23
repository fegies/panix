use std::collections::BTreeMap;

pub use lexer::SourcePosition;

pub mod impls;

#[derive(PartialEq, Clone)]
pub struct NixString<'a> {
    pub position: SourcePosition,
    pub content: NixStringContent<'a>,
}

#[derive(PartialEq, Clone)]
pub enum NixStringContent<'a> {
    Known(KnownNixStringContent<'a>),
    Interpolated(Vec<InterpolationEntry<'a>>),
}

#[derive(Clone)]
pub enum KnownNixStringContent<'a> {
    Literal(&'a str),
    Composite(Vec<&'a str>),
    Empty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InterpolationEntry<'a> {
    LiteralPiece(&'a str),
    Expression(NixExpr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BasicValue<'a> {
    String(NixString<'a>),
    Bool(bool),
    Null,
    Int(i64),
    Float(f64),
    Path(NixString<'a>),
    SearchPath(&'a str),
}

/// This is the key to an attribute set.
#[derive(Debug, PartialEq, Clone)]
pub enum AttrsetKey<'a> {
    /// just a single-level item
    Single(NixString<'a>),
    /// a multi-level entry.
    /// the parsed representation of the nested attrset shorthand.
    Multi(Vec<NixString<'a>>),
}
impl<'a> IntoIterator for AttrsetKey<'a> {
    type Item = NixString<'a>;

    type IntoIter = AttrsetKeyIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            AttrsetKey::Single(nix_string) => AttrsetKeyIter::Single(Some(nix_string)),
            AttrsetKey::Multi(vec) => AttrsetKeyIter::Multi(vec.into_iter()),
        }
    }
}

pub enum AttrsetKeyIter<'a> {
    Single(Option<NixString<'a>>),
    Multi(std::vec::IntoIter<NixString<'a>>),
}

impl<'a> ExactSizeIterator for AttrsetKeyIter<'a> {
    fn len(&self) -> usize {
        match self {
            AttrsetKeyIter::Single(nix_string) => {
                if nix_string.is_some() {
                    1
                } else {
                    0
                }
            }
            AttrsetKeyIter::Multi(i) => i.len(),
        }
    }
}
impl<'a> Iterator for AttrsetKeyIter<'a> {
    type Item = NixString<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            AttrsetKeyIter::Single(nix_string) => nix_string.take(),
            AttrsetKeyIter::Multi(iter) => iter.next(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InheritEntry<'a> {
    /// the source of the attribute.
    /// If it is not set to anything, it will be resolved from the variables in scope
    pub source: Option<Box<NixExpr<'a>>>,
    pub entries: Vec<&'a str>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Attrset<'a> {
    pub is_recursive: bool,
    pub inherit_keys: Vec<InheritEntry<'a>>,
    pub attrs: Vec<(AttrsetKey<'a>, NixExpr<'a>)>,
}
impl<'t> Attrset<'t> {
    pub(crate) fn empty() -> Attrset<'t> {
        Self {
            is_recursive: false,
            inherit_keys: Vec::new(),
            attrs: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct List<'a> {
    pub entries: Vec<NixExpr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompoundValue<'a> {
    Attrset(Attrset<'a>),
    List(List<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LetExpr<'a> {
    LetIn(LetInExpr<'a>),
    AttrsetLet(Attrset<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetInExpr<'a> {
    pub bindings: BTreeMap<&'a str, NixExpr<'a>>,
    pub inherit_entries: Vec<InheritEntry<'a>>,
    pub body: Box<NixExpr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpr<'a> {
    pub condition: Box<NixExpr<'a>>,
    pub truthy_case: Box<NixExpr<'a>>,
    pub falsy_case: Box<NixExpr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WithExpr<'a> {
    pub binding: Box<NixExpr<'a>>,
    pub body: Box<NixExpr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lambda<'a> {
    pub args: LambdaArgs<'a>,
    pub body: Box<NixExpr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    GreaterOrEqual,
    GreaterThanStrict,
    LogicalImplication,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MonopOpcode {
    NumericMinus,
    BinaryNot,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op<'a> {
    AttrRef {
        left: Box<NixExpr<'a>>,
        path: AttrsetKey<'a>,
        default: Option<Box<NixExpr<'a>>>,
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
    Monop {
        opcode: MonopOpcode,
        body: Box<NixExpr<'a>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Code<'a> {
    LetExpr(LetExpr<'a>),
    ValueReference { ident: &'a str },
    WithExpr(WithExpr<'a>),
    Lambda(Lambda<'a>),
    Op(Op<'a>),
    IfExpr(IfExpr<'a>),
    AssertExpr(AssertExpr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssertExpr<'a> {
    pub assertion: Box<NixExpr<'a>>,
    pub value: Box<NixExpr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum NixExprContent<'a> {
    BasicValue(BasicValue<'a>),
    CompoundValue(CompoundValue<'a>),
    Code(Code<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct NixExpr<'a> {
    pub position: SourcePosition,
    pub content: NixExprContent<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LambdaAttrsetArgs<'a> {
    pub bindings: BTreeMap<&'a str, Option<NixExpr<'a>>>,
    pub includes_rest_pattern: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LambdaArgs<'a> {
    SimpleBinding(&'a str),
    AttrsetBinding {
        total_name: Option<&'a str>,
        args: LambdaAttrsetArgs<'a>,
    },
}

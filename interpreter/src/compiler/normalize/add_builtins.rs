use std::{collections::BTreeMap, ops::Deref, sync::LazyLock};

use parser::{
    ast::{
        Attrset, AttrsetKey, Code, InheritEntry, Lambda, LetInExpr, NixExpr, NixExprContent,
        SourcePosition,
    },
    parse_nix,
};

use crate::{builtins::get_builtins_expr, compiler::get_null_expr};

use super::Pass;

pub fn add_builtins_def(ast: &mut NixExpr<'_>) {
    let position = ast.position;
    let body = core::mem::replace(ast, get_null_expr());

    let mut template = get_builtins_expr();
    *template.body = body;

    ast.content = NixExprContent::Code(Code::LetInExpr(template));
}

use std::{collections::BTreeMap, ops::Deref, sync::LazyLock};

use parser::{
    ast::{
        Attrset, AttrsetKey, Code, InheritEntry, Lambda, LetInExpr, NixExpr, NixExprContent,
        SourcePosition,
    },
    parse_nix,
};

use crate::compiler::get_null_expr;

use super::Pass;

static BUILTINS_EXPR: LazyLock<LetInExpr<'static>> = LazyLock::new(build_builtins_expr);

pub fn add_builtins_def(ast: &mut NixExpr<'_>) {
    let position = ast.position;
    let body = core::mem::replace(ast, get_null_expr());

    let mut template = BUILTINS_EXPR.clone();
    *template.body = body;

    ast.content = NixExprContent::Code(Code::LetInExpr(template));
}

fn build_builtins_expr() -> LetInExpr<'static> {
    let mut builtins = parse_nix(
        r#"
        let builtins = {
            inherit builtins;
        };
        in __body__
     "#
        .as_bytes(),
    )
    .expect("to be able to parse the static builtin def");

    MarkBuiltinsPass {}.inspect_expr(&mut builtins);

    if let NixExprContent::Code(Code::LetInExpr(letexpr)) = builtins.content {
        letexpr
    } else {
        unreachable!("we built a let in expr.");
    }
}

struct MarkBuiltinsPass {}

impl Pass<'_> for MarkBuiltinsPass {
    fn inspect_value_ref(&mut self, ident: &mut &str) {
        macro_rules! match_builtins {
            ($($builtin: expr), +) => {
                match *ident {
                    $(
                        $builtin => *ident = concat!("<builtin", $builtin, ">"),
                        )+
                    _ => {}
                }
            };
        }

        match_builtins!("_hasAttr", "_abort")
    }
}

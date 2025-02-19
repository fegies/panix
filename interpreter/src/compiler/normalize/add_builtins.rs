use parser::ast::{Code, NixExpr, NixExprContent};

use crate::{builtins::get_builtins_expr, compiler::get_null_expr};

pub fn add_builtins_def(ast: &mut NixExpr<'_>) {
    let body = core::mem::replace(ast, get_null_expr());

    let mut template = get_builtins_expr();
    *template.body = body;

    ast.content = NixExprContent::Code(Code::LetExpr(parser::ast::LetExpr::LetIn(template)));
}

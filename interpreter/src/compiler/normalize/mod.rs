use parser::ast::{
    Attrset, BasicValue, Code, CompoundValue, IfExpr, Lambda, LetInExpr, List, NixExpr, NixString,
    Op, WithExpr,
};

use self::remove_multipath_attrset::RemoveMultipathPass;

pub mod remove_multipath_attrset;

pub fn normalize_ast(ast: &mut NixExpr) {
    RemoveMultipathPass {}.inspect_expr(ast);
}

trait Pass {
    fn inspect_expr(&mut self, expr: &mut NixExpr) {
        match &mut expr.content {
            parser::ast::NixExprContent::BasicValue(val) => self.inspect_basic_value(val),
            parser::ast::NixExprContent::CompoundValue(val) => self.inspect_compount_value(val),
            parser::ast::NixExprContent::Code(code) => self.inspect_code(code),
        }
    }

    fn inspect_basic_value(&mut self, val: &mut BasicValue) {
        match val {
            BasicValue::String(str) => self.inspect_nix_string(str),
            BasicValue::Bool(_) => {}
            BasicValue::Null => {}
            BasicValue::Int(_) => {}
            BasicValue::Float(_) => {}
            BasicValue::Path(path) => self.inspect_nix_string(path),
        }
    }

    fn inspect_compount_value(&mut self, value: &mut CompoundValue) {
        match value {
            CompoundValue::Attrset(attrset) => self.inspect_attrset(attrset),
            CompoundValue::List(list) => self.inspect_list(list),
        }
    }

    fn inspect_code(&mut self, code: &mut Code) {
        match code {
            Code::LetInExpr(letexpr) => self.inspect_let_expr(letexpr),
            Code::ValueReference { mut ident } => self.inspect_value_ref(&mut ident),
            Code::WithExpr(expr) => self.inspect_with_expr(expr),
            Code::Lambda(lambda) => self.inspect_lambda(lambda),
            Code::Op(expr) => self.inspect_op(expr),
            Code::IfExpr(expr) => self.inspect_if_expr(expr),
        }
    }

    fn inspect_nix_string(&mut self, string: &mut NixString<'_>) {
        match string.content {
            parser::ast::NixStringContent::Literal(_) => todo!(),
            parser::ast::NixStringContent::Composite(_) => todo!(),
            parser::ast::NixStringContent::Interpolated(_) => todo!(),
            parser::ast::NixStringContent::Empty => todo!(),
        }
    }

    fn inspect_attrset(&mut self, attrset: &mut Attrset) {}

    fn inspect_list(&mut self, list: &mut List) {}

    fn inspect_let_expr(&mut self, expr: &mut LetInExpr) {}

    fn inspect_value_ref(&mut self, ident: &mut &str) {}

    fn inspect_with_expr(&mut self, expr: &mut WithExpr) {}

    fn inspect_lambda(&mut self, lambda: &mut Lambda) {}

    fn inspect_op(&mut self, expr: &mut Op) {}

    fn inspect_if_expr(&mut self, expr: &mut IfExpr) {}
}

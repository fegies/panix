use parser::ast::{
    Attrset, BasicValue, BinopOpcode, Code, CompoundValue, IfExpr, Lambda, LetInExpr, List,
    MonopOpcode, NixExpr, NixString, Op, WithExpr,
};

use self::remove_multipath_attrset::RemoveMultipathPass;

pub mod remove_multipath_attrset;

pub fn normalize_ast(ast: &mut NixExpr) {
    RemoveMultipathPass::new().inspect_expr(ast);
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
        match &mut string.content {
            parser::ast::NixStringContent::Known(_) => {}
            parser::ast::NixStringContent::Interpolated(exprs) => {
                for expr in exprs {
                    match expr {
                        parser::ast::InterpolationEntry::LiteralPiece(_) => {}
                        parser::ast::InterpolationEntry::Expression(expr) => {
                            self.inspect_expr(expr)
                        }
                    }
                }
            }
        }
    }

    fn inspect_attrset(&mut self, attrset: &mut Attrset) {
        self.descend_attrset(attrset)
    }
    fn descend_attrset(&mut self, attrset: &mut Attrset) {
        for (key, expr) in &mut attrset.attrs {
            match key {
                parser::ast::AttrsetKey::Single(s) => self.inspect_nix_string(s),
                parser::ast::AttrsetKey::Multi(m) => {
                    for part in m {
                        self.inspect_nix_string(part);
                    }
                }
            }
            self.inspect_expr(expr)
        }
    }

    fn inspect_list(&mut self, list: &mut List) {
        for expr in &mut list.entries {
            self.inspect_expr(expr)
        }
    }

    fn inspect_let_expr(&mut self, expr: &mut LetInExpr) {
        for expr in expr.bindings.values_mut() {
            self.inspect_expr(expr)
        }
        self.inspect_expr(&mut expr.body)
    }

    fn inspect_value_ref(&mut self, _ident: &mut &str) {}

    fn inspect_with_expr(&mut self, expr: &mut WithExpr) {
        self.inspect_expr(&mut expr.binding);
        self.inspect_expr(&mut expr.body);
    }

    fn inspect_lambda(&mut self, lambda: &mut Lambda) {
        match &mut lambda.args {
            parser::ast::LambdaArgs::SimpleBinding(_name) => {}
            parser::ast::LambdaArgs::AttrsetBinding {
                total_name: _,
                args,
            } => {
                for expr in args.bindings.values_mut().filter_map(|a| a.as_mut()) {
                    self.inspect_expr(expr);
                }
            }
        }

        self.inspect_expr(&mut lambda.body);
    }

    fn inspect_op(&mut self, expr: &mut Op) {
        match expr {
            Op::AttrRef {
                left,
                name,
                default,
            } => {
                self.inspect_expr(left);
                self.inspect_nix_string(name);
                if let Some(default) = default {
                    self.inspect_expr(default);
                }
            }
            Op::Call { function, arg } => {
                self.inspect_expr(function);
                self.inspect_expr(arg);
            }
            Op::Binop {
                left,
                right,
                opcode,
            } => self.inspect_binop(left, right, opcode),
            Op::HasAttr { left, path } => {
                self.inspect_expr(left);
                match path {
                    parser::ast::AttrsetKey::Single(s) => self.inspect_nix_string(s),
                    parser::ast::AttrsetKey::Multi(parts) => {
                        for part in parts {
                            self.inspect_nix_string(part)
                        }
                    }
                }
            }
            Op::Monop { opcode, body } => self.inspect_monop(body, opcode),
        }
    }

    fn inspect_binop(
        &mut self,
        left: &mut NixExpr,
        right: &mut NixExpr,
        _opcode: &mut BinopOpcode,
    ) {
        self.inspect_expr(left);
        self.inspect_expr(right);
    }

    fn inspect_monop(&mut self, expr: &mut NixExpr, _opcode: &mut MonopOpcode) {
        self.inspect_expr(expr)
    }

    fn inspect_if_expr(&mut self, expr: &mut IfExpr) {
        self.inspect_expr(&mut expr.condition);
        self.inspect_expr(&mut expr.truthy_case);
        self.inspect_expr(&mut expr.falsy_case);
    }
}

use add_builtins::add_builtins_def;
use bumpalo::Bump;
use misc::MiscPass;
use parser::ast::{
    AssertExpr, Attrset, AttrsetKey, BasicValue, BinopOpcode, Code, CompoundValue, IfExpr, Lambda,
    LetExpr, LetInExpr, List, MonopOpcode, NixExpr, NixString, Op, SourcePosition, WithExpr,
};
use remove_attrset_rec::RemoveAttrsetRecPass;
use remove_multipath_attrset::RemoveMultipathPass;
use remove_with_expr::RemoveWithExprPass;

mod add_builtins;
mod misc;
mod remove_attrset_rec;
mod remove_multipath_attrset;
mod remove_with_expr;

pub fn normalize_ast<'src>(ast: &mut NixExpr<'src>, bump: &'src Bump) {
    add_builtins_def(ast);
    MiscPass {}.inspect_expr(ast);
    RemoveMultipathPass::new().inspect_expr(ast);
    RemoveAttrsetRecPass::new().inspect_expr(ast);
    RemoveWithExprPass::new(bump).inspect_expr(ast);
}

trait Pass<'src> {
    fn inspect_expr(&mut self, expr: &mut NixExpr<'src>) {
        self.descend_expr(expr)
    }

    fn descend_expr(&mut self, expr: &mut NixExpr<'src>) {
        match &mut expr.content {
            parser::ast::NixExprContent::BasicValue(val) => self.inspect_basic_value(val),
            parser::ast::NixExprContent::CompoundValue(val) => {
                self.inspect_compount_value(val, expr.position)
            }
            parser::ast::NixExprContent::Code(code) => self.inspect_code(code, expr.position),
        }
    }

    fn inspect_basic_value(&mut self, val: &mut BasicValue<'src>) {
        match val {
            BasicValue::String(str) => self.inspect_nix_string(str),
            BasicValue::Bool(_) => {}
            BasicValue::Null => {}
            BasicValue::Int(_) => {}
            BasicValue::Float(_) => {}
            BasicValue::Path(path) => self.inspect_nix_string(path),
            BasicValue::SearchPath(_) => {}
        }
    }

    fn inspect_compount_value(&mut self, value: &mut CompoundValue<'src>, pos: SourcePosition) {
        match value {
            CompoundValue::Attrset(attrset) => self.inspect_attrset(attrset, pos),
            CompoundValue::List(list) => self.inspect_list(list),
        }
    }

    fn inspect_code(&mut self, code: &mut Code<'src>, pos: SourcePosition) {
        self.descend_code(code, pos)
    }

    fn descend_code(&mut self, code: &mut Code<'src>, pos: SourcePosition) {
        match code {
            Code::LetExpr(letexpr) => self.inspect_let_expr(letexpr, pos),
            Code::ValueReference { ident } => self.inspect_value_ref(ident),
            Code::WithExpr(expr) => self.inspect_with_expr(expr),
            Code::Lambda(lambda) => self.inspect_lambda(lambda),
            Code::Op(expr) => self.inspect_op(expr),
            Code::IfExpr(expr) => self.inspect_if_expr(expr),
            Code::AssertExpr(assert) => self.inspect_assert(assert),
        }
    }

    fn inspect_let_expr(&mut self, letexpr: &mut LetExpr<'src>, pos: SourcePosition) {
        match letexpr {
            LetExpr::LetIn(let_in_expr) => self.inspect_let_in_expr(let_in_expr),
            LetExpr::AttrsetLet(attrset) => self.inspect_attrset(attrset, pos),
        }
    }

    fn inspect_assert(&mut self, assert: &mut AssertExpr<'src>) {
        self.inspect_expr(&mut assert.assertion);
        self.inspect_expr(&mut assert.value);
    }

    fn inspect_nix_string(&mut self, string: &mut NixString<'src>) {
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

    fn inspect_attrset(&mut self, attrset: &mut Attrset<'src>, _pos: SourcePosition) {
        self.descend_attrset(attrset)
    }
    fn descend_attrset(&mut self, attrset: &mut Attrset<'src>) {
        for entry in &mut attrset.inherit_keys {
            if let Some(source) = entry.source.as_mut() {
                self.inspect_expr(source);
            }
        }
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

    fn inspect_list(&mut self, list: &mut List<'src>) {
        for expr in &mut list.entries {
            self.inspect_expr(expr)
        }
    }

    fn inspect_let_in_expr(&mut self, expr: &mut LetInExpr<'src>) {
        self.descend_let_expr(expr)
    }

    fn descend_let_expr(&mut self, expr: &mut LetInExpr<'src>) {
        for inherit in &mut expr.inherit_entries {
            if let Some(expr) = inherit.source.as_mut() {
                self.inspect_expr(expr)
            }
        }
        for expr in expr.bindings.values_mut() {
            self.inspect_expr(expr)
        }
        self.inspect_expr(&mut expr.body)
    }

    fn inspect_value_ref(&mut self, _ident: &mut &str) {}

    fn inspect_with_expr(&mut self, expr: &mut WithExpr<'src>) {
        self.inspect_expr(&mut expr.binding);
        self.inspect_expr(&mut expr.body);
    }

    fn inspect_lambda(&mut self, lambda: &mut Lambda<'src>) {
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

    fn inspect_op(&mut self, expr: &mut Op<'src>) {
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
            Op::HasAttr { left, path } => self.inspect_hasattr(left, path),
            Op::Monop { opcode, body } => self.inspect_monop(body, opcode),
        }
    }

    fn inspect_hasattr(&mut self, attrset: &mut NixExpr<'src>, path: &mut AttrsetKey<'src>) {
        self.descend_hasattr(attrset, path)
    }

    fn descend_hasattr(&mut self, attrset: &mut NixExpr<'src>, path: &mut AttrsetKey<'src>) {
        self.inspect_expr(attrset);
        match path {
            parser::ast::AttrsetKey::Single(s) => self.inspect_nix_string(s),
            parser::ast::AttrsetKey::Multi(parts) => {
                for part in parts {
                    self.inspect_nix_string(part)
                }
            }
        }
    }

    fn inspect_binop(
        &mut self,
        left: &mut NixExpr<'src>,
        right: &mut NixExpr<'src>,
        _opcode: &mut BinopOpcode,
    ) {
        self.inspect_expr(left);
        self.inspect_expr(right);
    }

    fn inspect_monop(&mut self, expr: &mut NixExpr<'src>, _opcode: &mut MonopOpcode) {
        self.inspect_expr(expr)
    }

    fn inspect_if_expr(&mut self, expr: &mut IfExpr<'src>) {
        self.inspect_expr(&mut expr.condition);
        self.inspect_expr(&mut expr.truthy_case);
        self.inspect_expr(&mut expr.falsy_case);
    }
}

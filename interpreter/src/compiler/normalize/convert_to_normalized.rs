use std::collections::BTreeMap;

use crate::compiler::normalize::normalized_ast::{self, *};

pub fn convert_to_normalized(expr: parser::ast::NixExpr<'_>) -> normalized_ast::NixExpr<'_> {
    let content = match expr.content {
        parser::ast::NixExprContent::BasicValue(basic_value) => {
            NixExprContent::BasicValue(convert_basic_value(basic_value))
        }
        parser::ast::NixExprContent::CompoundValue(compound_value) => {
            NixExprContent::CompoundValue(convert_compound_value(compound_value))
        }
        parser::ast::NixExprContent::Code(code) => NixExprContent::Code(convert_code(code)),
    };

    NixExpr {
        position: expr.position,
        content,
    }
}

fn convert_basic_value(val: parser::ast::BasicValue<'_>) -> BasicValue<'_> {
    match val {
        parser::ast::BasicValue::String(nix_string) => {
            BasicValue::String(convert_string(nix_string))
        }
        parser::ast::BasicValue::Bool(b) => BasicValue::Bool(b),
        parser::ast::BasicValue::Null => BasicValue::Null,
        parser::ast::BasicValue::Int(i) => BasicValue::Int(i),
        parser::ast::BasicValue::Float(f) => BasicValue::Float(f),
        parser::ast::BasicValue::Path(p) => BasicValue::Path(convert_string(p)),
        parser::ast::BasicValue::SearchPath(_) => {
            unreachable!("Search paths should have been removed by an ast pass.")
        }
    }
}

fn convert_string(nix_string: parser::ast::NixString<'_>) -> NixString<'_> {
    let content = match nix_string.content {
        parser::ast::NixStringContent::Known(known_nix_string_content) => {
            NixStringContent::Known(match known_nix_string_content {
                parser::ast::KnownNixStringContent::Literal(l) => KnownNixStringContent::Literal(l),
                parser::ast::KnownNixStringContent::Composite(items) => {
                    KnownNixStringContent::Composite(items)
                }
                parser::ast::KnownNixStringContent::Empty => KnownNixStringContent::Empty,
            })
        }
        parser::ast::NixStringContent::Interpolated(items) => {
            let mut result_items = Vec::with_capacity(items.len());
            for item in items {
                result_items.push(match item {
                    parser::ast::InterpolationEntry::LiteralPiece(l) => {
                        InterpolationEntry::LiteralPiece(l)
                    }
                    parser::ast::InterpolationEntry::Expression(nix_expr) => {
                        InterpolationEntry::Expression(convert_to_normalized(nix_expr))
                    }
                });
            }
            NixStringContent::Interpolated(result_items)
        }
    };

    NixString {
        position: nix_string.position,
        content,
    }
}

fn convert_compound_value(compound_value: parser::ast::CompoundValue<'_>) -> CompoundValue<'_> {
    match compound_value {
        parser::ast::CompoundValue::Attrset(attrset) => {
            CompoundValue::Attrset(convert_attrset(attrset))
        }
        parser::ast::CompoundValue::List(list) => CompoundValue::List(List {
            entries: list
                .entries
                .into_iter()
                .map(convert_to_normalized)
                .collect(),
        }),
    }
}

fn convert_attrset(attrset: parser::ast::Attrset<'_>) -> Attrset<'_> {
    if attrset.is_recursive {
        unreachable!("Recursive attrsets should have been removed by an ast pass.");
    }

    let mut attrs = Vec::with_capacity(attrset.attrs.len());
    for (key, value) in attrset.attrs {
        let key = match key {
            parser::ast::AttrsetKey::Single(nix_string) => nix_string,
            parser::ast::AttrsetKey::Multi(_nix_strings) => {
                unreachable!("Multipart attrset keys should have been removed by an ast pass.")
            }
        };
        attrs.push((convert_string(key), convert_to_normalized(value)));
    }

    Attrset {
        inherit_keys: attrset
            .inherit_keys
            .into_iter()
            .map(convert_inherit_key)
            .collect(),
        attrs,
    }
}

fn convert_inherit_key(entry: parser::ast::InheritEntry<'_>) -> InheritEntry<'_> {
    let source = entry.source.map(|s| Box::new(convert_to_normalized(*s)));
    InheritEntry {
        source,
        entries: entry.entries,
    }
}

fn convert_code(code: parser::ast::Code<'_>) -> Code<'_> {
    match code {
        parser::ast::Code::LetExpr(let_expr) => Code::LetExpr(convert_let(let_expr)),
        parser::ast::Code::ValueReference { ident } => Code::ValueReference { ident },
        parser::ast::Code::WithExpr(_) => {
            unreachable!("With expressions should have been removed by an ast pass.")
        }
        parser::ast::Code::Lambda(lambda) => Code::Lambda(convert_lambda(lambda)),
        parser::ast::Code::Op(op) => Code::Op(convert_op(op)),
        parser::ast::Code::IfExpr(if_expr) => Code::IfExpr(IfExpr {
            condition: Box::new(convert_to_normalized(*if_expr.condition)),
            truthy_case: Box::new(convert_to_normalized(*if_expr.truthy_case)),
            falsy_case: Box::new(convert_to_normalized(*if_expr.falsy_case)),
        }),
        parser::ast::Code::AssertExpr(_assert_expr) => {
            unreachable!("Asserts should have been removed by an ast pass.")
        }
    }
}

fn convert_op(op: parser::ast::Op<'_>) -> Op<'_> {
    match op {
        parser::ast::Op::AttrRef {
            left,
            path,
            default,
        } => Op::AttrRef {
            left: Box::new(convert_to_normalized(*left)),
            path: convert_attrsetkey(path),
            default: default.map(|v| Box::new(convert_to_normalized(*v))),
        },
        parser::ast::Op::Call { function, arg } => Op::Call {
            function: Box::new(convert_to_normalized(*function)),
            arg: Box::new(convert_to_normalized(*arg)),
        },
        parser::ast::Op::Binop {
            left,
            right,
            opcode,
        } => Op::Binop {
            left: Box::new(convert_to_normalized(*left)),
            right: Box::new(convert_to_normalized(*right)),
            opcode,
        },
        parser::ast::Op::HasAttr { left, path } => Op::HasAttr {
            left: Box::new(convert_to_normalized(*left)),
            path: match path {
                parser::ast::AttrsetKey::Single(nix_string) => convert_string(nix_string),
                parser::ast::AttrsetKey::Multi(_nix_strings) => {
                    unreachable!("multipath hasattr should have been removed by an ast pass.")
                }
            },
        },
        parser::ast::Op::Monop { opcode, body } => Op::Monop {
            opcode,
            body: Box::new(convert_to_normalized(*body)),
        },
    }
}

fn convert_attrsetkey(path: parser::ast::AttrsetKey<'_>) -> AttrsetKey<'_> {
    match path {
        parser::ast::AttrsetKey::Single(nix_string) => {
            AttrsetKey::Single(convert_string(nix_string))
        }
        parser::ast::AttrsetKey::Multi(nix_strings) => {
            AttrsetKey::Multi(nix_strings.into_iter().map(convert_string).collect())
        }
    }
}

fn convert_lambda(lambda: parser::ast::Lambda<'_>) -> Lambda<'_> {
    let args = match lambda.args {
        parser::ast::LambdaArgs::SimpleBinding(s) => LambdaArgs::SimpleBinding(s),
        parser::ast::LambdaArgs::AttrsetBinding { total_name, args } => {
            let mut bindings = BTreeMap::new();
            for (k, v) in args.bindings {
                bindings.insert(k, v.map(convert_to_normalized));
            }
            LambdaArgs::AttrsetBinding {
                total_name,
                args: LambdaAttrsetArgs {
                    bindings,
                    includes_rest_pattern: args.includes_rest_pattern,
                },
            }
        }
    };

    Lambda {
        args,
        body: Box::new(convert_to_normalized(*lambda.body)),
    }
}

fn convert_let(let_expr: parser::ast::LetExpr<'_>) -> LetInExpr<'_> {
    match let_expr {
        parser::ast::LetExpr::LetIn(let_in_expr) => {
            let mut bindings = BTreeMap::new();
            for (k, v) in let_in_expr.bindings {
                bindings.insert(k, convert_to_normalized(v));
            }

            LetInExpr {
                bindings,
                inherit_entries: let_in_expr
                    .inherit_entries
                    .into_iter()
                    .map(convert_inherit_key)
                    .collect(),
                body: Box::new(convert_to_normalized(*let_in_expr.body)),
            }
        }
        parser::ast::LetExpr::AttrsetLet(_attrset) => {
            unreachable!("Attrset let should have been removed by an ast pass.")
        }
    }
}

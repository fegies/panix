use gc::{GcError, GcHandle, GcPointer};
use parser::ast::{Attrset, BasicValue, List, NixExpr, NixString};

use crate::{
    util::BufferPool,
    vm::value::{self, NixValue, Thunk},
};

mod normalize;
#[cfg(test)]
mod tests;

#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("error when allocating")]
    Gc(#[from] GcError),
}

pub fn translate_expression(
    gc_handle: &mut GcHandle,
    mut expr: NixExpr<'_>,
) -> Result<Thunk, CompileError> {
    normalize::normalize_ast(&mut expr);
    let key_buffer = Default::default();
    let value_buffer = Default::default();
    let mut compiler = Compiler {
        gc_handle,
        key_buffer: &key_buffer,
        value_buffer: &value_buffer,
    };
    let mut root_scope = CompilationScope {
        compiler: &mut compiler,
        parent: None,
    };
    root_scope.translate_expression(expr)
}

struct Compiler<'gc, 'bufferpool> {
    gc_handle: &'gc mut GcHandle,
    key_buffer: &'bufferpool BufferPool<value::NixString>,
    value_buffer: &'bufferpool BufferPool<Thunk>,
}

struct CompilationScope<'compiler, 'bufferpool, 'gc> {
    compiler: &'compiler mut Compiler<'bufferpool, 'gc>,
    parent: Option<&'compiler mut CompilationScope<'compiler, 'bufferpool, 'gc>>,
}

impl<'gc> Compiler<'gc, '_> {
    fn alloc_string(&mut self, str: NixString<'_>) -> Result<value::NixString, CompileError> {
        let res = match str.content {
            parser::ast::NixStringContent::Interpolated(_) => todo!(),
            parser::ast::NixStringContent::Known(known_entry) => match known_entry {
                parser::ast::KnownNixStringContent::Literal(literal) => {
                    value::NixString::Simple(self.gc_handle.alloc_string(literal)?)
                }
                parser::ast::KnownNixStringContent::Composite(_) => todo!(),
                parser::ast::KnownNixStringContent::Empty => {
                    value::NixString::Simple(self.gc_handle.alloc_string("")?)
                }
            },
        };
        Ok(res)
    }
}

impl<'compiler, 'gc, 'bufferpool> CompilationScope<'compiler, 'bufferpool, 'gc> {
    fn translate_expression(&mut self, expr: NixExpr<'_>) -> Result<Thunk, CompileError> {
        let res = match expr.content {
            parser::ast::NixExprContent::BasicValue(v) => {
                Thunk::Value(self.translate_basic_value(v)?)
            }
            parser::ast::NixExprContent::CompoundValue(v) => {
                Thunk::Value(self.translate_compound_value(v)?)
            }
            parser::ast::NixExprContent::Code(_) => todo!(),
        };
        Ok(res)
    }

    fn translate_basic_value(&mut self, value: BasicValue<'_>) -> Result<NixValue, CompileError> {
        let res = match value {
            BasicValue::Bool(bool) => NixValue::Bool(bool),
            BasicValue::Null => NixValue::Null,
            BasicValue::Int(i) => NixValue::Int(i),
            BasicValue::Float(f) => NixValue::Float(f),
            BasicValue::String(str) => NixValue::String(self.compiler.alloc_string(str)?),
            BasicValue::Path(p) => NixValue::Path(self.compiler.alloc_string(p)?),
        };

        Ok(res)
    }

    fn translate_compound_value(
        &mut self,
        value: parser::ast::CompoundValue<'_>,
    ) -> Result<NixValue, CompileError> {
        Ok(match value {
            parser::ast::CompoundValue::Attrset(attrset) => {
                NixValue::Attrset(self.translate_attrset(attrset)?)
            }
            parser::ast::CompoundValue::List(list) => NixValue::List(self.translate_list(list)?),
        })
    }

    fn resolve_reference(&mut self, reference: &str) -> Result<Thunk, CompileError> {
        todo!()
    }

    fn translate_attrset(&mut self, attrset: Attrset<'_>) -> Result<value::Attrset, CompileError> {
        let mut key_buffer = self.compiler.key_buffer.get();
        let key_buffer = key_buffer.as_mut();
        let mut value_buffer = self.compiler.value_buffer.get();
        let value_buffer = value_buffer.as_mut();

        // first fill the inherited keys
        for key in attrset.inherit_keys {
            let value = self.resolve_reference(key)?;
            let key = value::NixString::Simple(self.compiler.gc_handle.alloc_string(key)?);
            key_buffer.push(key);
            value_buffer.push(value);
        }

        // and now all of the normal keys
        if attrset.is_recursive {
            todo!()
        } else {
            // since this attrset is _not_ recursive, there is no need to
            // create a new scope, no matter how the keys look like.
            for (k, v) in attrset.attrs {
                let key = match k {
                    parser::ast::AttrsetKey::Single(s) => self.compiler.alloc_string(s)?,
                    parser::ast::AttrsetKey::Multi(_) => {
                        unreachable!(
                            "Multipath attrsets should have been removed by a normalize pass"
                        )
                    }
                };
                key_buffer.push(key);
                let value = self.translate_expression(v)?;
                value_buffer.push(value);
            }
        }

        let keys = self.compiler.gc_handle.alloc_vec(key_buffer)?;
        let values = self.compiler.gc_handle.alloc_vec(value_buffer)?;

        Ok(value::Attrset { keys, values })
    }

    fn translate_list(&mut self, list: List<'_>) -> Result<value::List, CompileError> {
        let mut value_buffer = self.compiler.value_buffer.get();
        let value_buffer = value_buffer.as_mut();
        for entry in list.entries {
            value_buffer.push(self.translate_expression(entry)?);
        }

        let entries = self.compiler.gc_handle.alloc_vec(value_buffer)?;
        Ok(value::List { entries })
    }
}

use std::fmt::Write;

use gc::GcPointer;

use crate::{
    EvaluateError, Evaluator,
    vm::value::{NixString, NixValue, Thunk},
};

#[derive(thiserror::Error, Debug)]
enum ToJsonError {
    #[error("tried to serialize self referential datastructure")]
    SelfRecursiveSerialized,
    #[error("cannot convert a function to JSON")]
    FunctionSerialized,
    #[error("generic eval err")]
    EvaluateError(#[from] EvaluateError),
}

type JsonSerResult<T> = Result<T, ToJsonError>;

pub struct JsonWriter<'eval, 'gc> {
    output_buffer: String,
    seen_stack: Vec<GcPointer<Thunk>>,
    evaluator: &'eval mut Evaluator<'gc>,
}

impl<'eval, 'gc> JsonWriter<'eval, 'gc> {
    pub fn new(evaluator: &'eval mut Evaluator<'gc>) -> Self {
        JsonWriter {
            output_buffer: String::new(),
            seen_stack: Vec::with_capacity(8),
            evaluator,
        }
    }

    pub fn serialize(mut self, value: GcPointer<Thunk>) -> Result<NixString, EvaluateError> {
        self.write_thunk(value).map_err(|e| match e {
            ToJsonError::EvaluateError(e) => e,
            _ => EvaluateError::Misc(Box::new(e)),
        })?;

        let nixstr = self.evaluator.gc_handle.alloc_string(&self.output_buffer)?;

        Ok(nixstr.into())
    }

    fn write_thunk(&mut self, thunk: GcPointer<Thunk>) -> JsonSerResult<()> {
        for parent in &self.seen_stack {
            if self.evaluator.gc_handle.reference_equals(parent, &thunk) {
                return Err(ToJsonError::SelfRecursiveSerialized);
            }
        }

        self.seen_stack.push(thunk.clone());
        let value = self.evaluator.force_thunk(thunk.clone())?;
        self.write_value(&value, thunk)?;
        self.seen_stack.pop();
        Ok(())
    }

    fn write_value(&mut self, value: &NixValue, thunk: GcPointer<Thunk>) -> JsonSerResult<()> {
        match value {
            NixValue::String(nix_string) => self.write_string(nix_string),
            NixValue::Bool(b) => self.write_bool(*b),
            NixValue::Null => self.write_null(),
            NixValue::Int(i) => self.write_int(*i),
            NixValue::Float(f) => self.write_float(*f),
            NixValue::Path(path_value) => self.write_path(path_value),
            NixValue::Attrset(attrset) => self.write_object(attrset, thunk),
            NixValue::Function(_function) => Err(ToJsonError::FunctionSerialized),
            NixValue::List(list) => self.write_list(list),
            NixValue::Builtin(_builtin_type_token) => Err(ToJsonError::FunctionSerialized),
        }
    }

    fn write_bool(&mut self, b: bool) -> Result<(), ToJsonError> {
        if b {
            self.output_buffer.push_str("true");
        } else {
            self.output_buffer.push_str("false");
        }

        Ok(())
    }

    fn write_null(&mut self) -> Result<(), ToJsonError> {
        self.output_buffer.push_str("null");
        Ok(())
    }

    fn write_int(&mut self, i: i64) -> Result<(), ToJsonError> {
        self.output_buffer.write_fmt(format_args!("{}", i)).unwrap();
        Ok(())
    }

    fn write_float(&mut self, f: f64) -> Result<(), ToJsonError> {
        self.output_buffer.write_fmt(format_args!("{}", f)).unwrap();
        Ok(())
    }

    fn write_object(
        &mut self,
        attrset: &crate::vm::value::Attrset,
        thunk: GcPointer<Thunk>,
    ) -> Result<(), ToJsonError> {
        if let Some(tostring_func) = attrset.get_entry_str(&self.evaluator.gc_handle, "__toString")
        {
            let tostring_func = self
                .evaluator
                .force_thunk(tostring_func)?
                .expect_function()?;
            let string_repr = self
                .evaluator
                .evaluate_call(tostring_func, thunk)?
                .expect_string()?;
            return self.write_string(&string_repr);
        }

        let items = self
            .evaluator
            .gc_handle
            .load(&attrset.entries)
            .as_ref()
            .to_owned();

        self.output_buffer.push('{');
        let mut first = true;
        for (name, value) in items {
            if first {
                first = false;
            } else {
                self.output_buffer.push(',');
            }

            self.write_string(&name)?;
            self.output_buffer.push(':');
            self.write_thunk(value)?;
        }
        self.output_buffer.push('}');

        Ok(())
    }

    fn write_list(&mut self, list: &crate::vm::value::List) -> Result<(), ToJsonError> {
        let items = self
            .evaluator
            .gc_handle
            .load(&list.entries)
            .as_ref()
            .to_owned();

        self.output_buffer.push('[');
        let mut first = true;
        for item in items {
            if first {
                first = false;
            } else {
                self.output_buffer.push(',');
            }

            self.write_thunk(item)?;
        }
        self.output_buffer.push(']');

        Ok(())
    }

    fn write_string(&mut self, string: &NixString) -> JsonSerResult<()> {
        let mut remaining = string.load(&self.evaluator.gc_handle);

        self.output_buffer.push('"');
        while let Some(escape_point) = find_first_escape_point(remaining) {
            self.output_buffer.push_str(&remaining[..escape_point]);
            self.output_buffer.push('\\');
            self.output_buffer
                .push(get_escape_char(remaining.as_bytes()[escape_point]));
            remaining = &remaining[(escape_point + 1)..];
        }
        self.output_buffer.push_str(remaining);
        self.output_buffer.push('"');

        return Ok(());

        fn find_first_escape_point(input: &str) -> Option<usize> {
            input.as_bytes().iter().enumerate().find_map(|(idx, b)| {
                if matches!(*b, b'\\' | b'"' | b'\x08' | b'\x0c' | b'\n' | b'\r' | b'\t') {
                    Some(idx)
                } else {
                    None
                }
            })
        }
        fn get_escape_char(input: u8) -> char {
            match input {
                b'\\' => '\\',
                b'"' => '"',
                b'\x08' => 'b',
                b'\x0c' => 'f',
                b'\n' => 'n',
                b'\r' => 'r',
                b'\t' => 't',
                _ => unreachable!(),
            }
        }
    }

    fn write_path(&self, _path_value: &crate::vm::value::PathValue) -> Result<(), ToJsonError> {
        todo!()
    }
}

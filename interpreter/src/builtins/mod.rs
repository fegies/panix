use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    io::{self, Read},
    path::{Path, PathBuf},
    sync::LazyLock,
};

use gc::{GcHandle, GcPointer};
use gc_derive::Trace;
use json_parser::JsonParser;
use parser::{
    ast::{Code, LetInExpr, NixExprContent},
    parse_nix,
};
use regex::Regex;

mod json_parser;

use crate::{
    compile_source_with_nix_filename,
    vm::{
        opcodes::{ExecutionContext, ValueSource, VmOp},
        value::{self, Attrset, List, NixString, NixValue, Thunk},
    },
    EvaluateError, Evaluator, InterpreterError,
};

pub trait Builtins {
    /// an opaque token that identifies the specific builtin being called.
    type TypeToken;
    type ExecuteError;

    /// try to resolve the given ident as a built in fuction.
    /// the provided token is guaranteed to be able to be used in the execute_builtin call.
    fn get_builtin(&self, ident: &str) -> Option<Self::TypeToken>;

    fn execute_builtin(
        &self,
        builtin: Self::TypeToken,
        argument: GcPointer<Thunk>,
        evaluator: &mut Evaluator,
    ) -> Result<NixValue, Self::ExecuteError>;
}

static BUILTINS_EXPR: LazyLock<LetInExpr<'static>> = LazyLock::new(build_builtins_expr);

pub fn get_builtins() -> NixBuiltins {
    NixBuiltins {
        import_cache: RefCell::new(HashMap::new()),
    }
}

pub fn get_builtins_expr() -> LetInExpr<'static> {
    BUILTINS_EXPR.clone()
}

fn build_builtins_expr() -> LetInExpr<'static> {
    let builtins = parse_nix(include_bytes!("./prelude.nix"))
        .expect("to be able to parse the static builtin def");

    if let NixExprContent::Code(Code::LetInExpr(letexpr)) = builtins.content {
        letexpr
    } else {
        unreachable!("we built a let in expr.");
    }
}

pub struct NixBuiltins {
    import_cache: RefCell<HashMap<PathBuf, NixValue>>,
}

#[derive(Debug, Clone, Trace, Copy)]
pub struct BuiltinTypeToken {
    inner: BuiltinType,
}
#[derive(Debug, Clone, Trace, Copy)]
enum BuiltinType {
    Throw,
    Abort,
    TryEval,
    TypeOf,
    ToString,
    Map,
    Import,
    Split,
    FilterPick,
    ListLength,
    ElemAt,
    ConcatLists,
    FromJson,
    RemoveAttrs,
    DeepSeq,
    Seq,
    Match,
    MapAttrs,
    ConcatStrings,
    StringLength,
    Substring,
}

impl Builtins for NixBuiltins {
    type TypeToken = BuiltinTypeToken;
    type ExecuteError = EvaluateError;

    fn get_builtin(&self, ident: &str) -> Option<Self::TypeToken> {
        let t = match ident {
            "___builtin_throw" => BuiltinType::Throw,
            "___builtin_tryeval" => BuiltinType::TryEval,
            "___builtin_typeof" => BuiltinType::TypeOf,
            "___builtin_tostring" => BuiltinType::ToString,
            "___builtin_map" => BuiltinType::Map,
            "___builtin_import" => BuiltinType::Import,
            "___builtin_abort" => BuiltinType::Abort,
            "___builtin_split" => BuiltinType::Split,
            "___builtin_filter_pick" => BuiltinType::FilterPick,
            "___builtin_length" => BuiltinType::ListLength,
            "___builtin_elemat" => BuiltinType::ElemAt,
            "___builtin_concatLists" => BuiltinType::ConcatLists,
            "___builtin_fromJSON" => BuiltinType::FromJson,
            "___builtin_removeAttrs" => BuiltinType::RemoveAttrs,
            "___builtin_deepSeq" => BuiltinType::DeepSeq,
            "___builtin_seq" => BuiltinType::Seq,
            "___builtin_match" => BuiltinType::Match,
            "___builtin_mapAttrs" => BuiltinType::MapAttrs,
            "___builtin_concatStrings" => BuiltinType::ConcatStrings,
            "___builtin_stringLength" => BuiltinType::StringLength,
            "___builtin_substring" => BuiltinType::Substring,
            _ => return None,
        };

        Some(BuiltinTypeToken { inner: t })
    }

    fn execute_builtin(
        &self,
        builtin: Self::TypeToken,
        argument: GcPointer<Thunk>,
        evaluator: &mut Evaluator,
    ) -> Result<NixValue, Self::ExecuteError> {
        match builtin.inner {
            BuiltinType::Throw => Err(EvaluateError::Throw {
                value: evaluator
                    .force_thunk(argument)?
                    .expect_string()?
                    .load(&evaluator.gc_handle)
                    .to_owned(),
            }),
            BuiltinType::Abort => Err(EvaluateError::Abort {
                value: evaluator
                    .force_thunk(argument)?
                    .expect_string()?
                    .load(&evaluator.gc_handle)
                    .to_owned(),
            }),
            BuiltinType::TryEval => {
                let value = match evaluator.force_thunk(argument) {
                    Ok(val) => Some(val),
                    e @ Err(EvaluateError::Abort { value: _ }) => {
                        // we cannot catch aborts.
                        return e;
                    }
                    Err(_) => None,
                };
                let success_string = evaluator.gc_handle.alloc_string("success")?.into();
                let value_string = evaluator.gc_handle.alloc_string("value")?.into();

                let bool_value = NixValue::Bool(value.is_some());

                let value = evaluator
                    .gc_handle
                    .alloc(Thunk::Value(value.unwrap_or_else(|| bool_value.clone())))?;
                let bool_value = evaluator.gc_handle.alloc(Thunk::Value(bool_value))?;

                let entries = evaluator
                    .gc_handle
                    .alloc_slice(&[(success_string, bool_value), (value_string, value)])?;

                let attrset = Attrset { entries };

                Ok(NixValue::Attrset(attrset))
            }
            BuiltinType::TypeOf => {
                let typename = match evaluator.force_thunk(argument)? {
                    NixValue::String(_) => "string",
                    NixValue::Bool(_) => "bool",
                    NixValue::Null => "null",
                    NixValue::Int(_) => "int",
                    NixValue::Float(_) => "float",
                    NixValue::Path(_) => "path",
                    NixValue::Attrset(_) => "set",
                    NixValue::Function(_) => "lambda",
                    NixValue::List(_) => "list",
                    NixValue::Builtin(_) => "lambda",
                };

                let value = evaluator.gc_handle.alloc_string(typename)?;

                Ok(NixValue::String(value.into()))
            }
            BuiltinType::ToString => Ok(NixValue::String(execute_to_string(argument, evaluator)?)),
            BuiltinType::Map => execute_map(evaluator, argument),
            BuiltinType::Import => self.execute_import(evaluator, argument),
            BuiltinType::Split => execute_split(evaluator, argument),
            BuiltinType::FilterPick => execute_filter_pick(evaluator, argument),
            BuiltinType::ListLength => {
                let lst = evaluator.force_thunk(argument)?.expect_list()?;
                let len = evaluator.gc_handle.load(&lst.entries).as_ref().len();
                Ok(NixValue::Int(len as i64))
            }
            BuiltinType::ElemAt => {
                let arg = evaluator.force_thunk(argument)?.expect_attrset()?;
                let list = arg
                    .get_and_force_entry_str(evaluator, "list")?
                    .expect_list()?;
                let idx: usize = arg
                    .get_and_force_entry_str(evaluator, "idx")?
                    .expect_int()?
                    .try_into()
                    .map_err(|_| EvaluateError::AccessOutOfRange)?;

                let ptr = evaluator
                    .gc_handle
                    .load(&list.entries)
                    .as_ref()
                    .get(idx)
                    .ok_or(EvaluateError::AccessOutOfRange)?
                    .clone();

                evaluator.force_thunk(ptr)
            }
            BuiltinType::ConcatLists => execute_concat_lists(evaluator, argument),
            BuiltinType::FromJson => execute_fromjson(evaluator, argument),
            BuiltinType::RemoveAttrs => execute_remove_attrs(evaluator, argument),
            BuiltinType::DeepSeq => execute_deepseq(evaluator, argument),
            BuiltinType::Seq => {
                let [e1, e2] = evaluator
                    .force_thunk(argument)?
                    .expect_list()?
                    .expect_entries(&evaluator.gc_handle)?;

                evaluator.force_thunk(e1)?;
                evaluator.force_thunk(e2)
            }
            BuiltinType::Match => execute_match(evaluator, argument),
            BuiltinType::MapAttrs => execute_map_attrs(evaluator, argument),
            BuiltinType::ConcatStrings => execute_concat_strings(evaluator, argument),
            BuiltinType::StringLength => {
                let len = evaluator
                    .force_thunk(argument)?
                    .expect_string()?
                    .load(&evaluator.gc_handle)
                    .len();
                Ok(NixValue::Int(len as i64))
            }
            BuiltinType::Substring => execute_substring(evaluator, argument),
        }
    }
}

fn execute_substring(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let [start, len, string] = evaluator
        .force_thunk(argument)?
        .expect_list()?
        .expect_entries(&evaluator.gc_handle)?;

    let start = evaluator.force_thunk(start)?.expect_int()? as usize;
    let len = evaluator.force_thunk(len)?.expect_int()? as usize;

    let range = start..(start + len);

    let result = evaluator
        .force_thunk(string)?
        .expect_string()?
        .get_substring(&mut evaluator.gc_handle, range)?;

    Ok(NixValue::String(result))
}

fn execute_concat_strings(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let strings = evaluator.force_thunk(argument)?.expect_list()?;
    let strings = evaluator
        .gc_handle
        .load(&strings.entries)
        .as_ref()
        .to_owned()
        .into_iter()
        .map(|ptr| evaluator.force_thunk(ptr)?.expect_string())
        .collect::<Vec<_>>();

    let result = NixString::concat_many(strings.into_iter(), &mut evaluator.gc_handle)?;

    Ok(NixValue::String(result))
}

fn execute_map_attrs(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let [func, attrset] = evaluator
        .force_thunk(argument)?
        .expect_list()?
        .expect_entries(&evaluator.gc_handle)?;

    let attrset = evaluator.force_thunk(attrset)?.expect_attrset()?;
    let mut attrset_entries = evaluator
        .gc_handle
        .load(&attrset.entries)
        .as_ref()
        .to_owned();

    if attrset_entries.is_empty() {
        // for an empty attrset we do not need to map anything
        return Ok(NixValue::Attrset(attrset));
    }

    let call_code = evaluator.gc_handle.alloc_slice(&[
        VmOp::LoadThunk(ValueSource::ContextReference(0)), // function to be called
        VmOp::DuplicateThunk(ValueSource::ContextReference(1)), // entry name
        VmOp::Call,
        VmOp::DuplicateThunk(ValueSource::ContextReference(2)), // entry value
        VmOp::TailCall,
    ])?;

    for (attr_name, attr_value) in &mut attrset_entries {
        let name_thunk = evaluator
            .gc_handle
            .alloc(Thunk::Value(NixValue::String(attr_name.clone())))?;
        let context =
            evaluator
                .gc_handle
                .alloc_slice(&[func.clone(), name_thunk, attr_value.clone()])?;
        *attr_value = evaluator.gc_handle.alloc(Thunk::Deferred {
            context: ExecutionContext { entries: context },
            code: call_code.clone(),
        })?;
    }

    let attrset_entries = evaluator.gc_handle.alloc_vec(&mut attrset_entries)?;
    Ok(NixValue::Attrset(Attrset {
        entries: attrset_entries,
    }))
}

fn execute_match(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let [regex, string] = evaluator
        .force_thunk(argument)?
        .expect_list()?
        .expect_entries(&evaluator.gc_handle)?;

    let regex = {
        let regex = evaluator.force_thunk(regex)?.expect_string()?;
        let regex = regex.load(&evaluator.gc_handle);
        if regex.starts_with('^') && regex.ends_with('$') {
            Regex::new(regex)
        } else {
            let mut re_buffer = String::with_capacity(regex.len() + 2);
            if !regex.starts_with('^') {
                re_buffer.push('^');
            }
            re_buffer.push_str(regex);
            if !regex.ends_with('$') {
                re_buffer.push('$');
            }
            Regex::new(&re_buffer)
        }
        .map_err(|e| EvaluateError::Misc(Box::new(e)))?
    };

    let string = evaluator.force_thunk(string)?.expect_string()?;
    if regex.captures_len() == 0 {
        // no capture groups
        if regex.is_match(string.load(&evaluator.gc_handle)) {
            Ok(NixValue::List(List {
                entries: evaluator.gc_handle.alloc_slice(&[])?,
            }))
        } else {
            Ok(NixValue::Null)
        }
    } else {
        // capture groups present.
        if let Some(re_match) = regex.captures(string.load(&evaluator.gc_handle)) {
            let ranges = re_match
                .iter()
                .skip(1) // skip the initial full match
                .map(|capture| capture.map(|m| m.range()))
                .collect::<Vec<_>>();
            core::mem::drop(re_match);
            let mut substrings = ranges
                .into_iter()
                .map(|range| {
                    let value = if let Some(range) = range {
                        NixValue::String(string.get_substring(&mut evaluator.gc_handle, range)?)
                    } else {
                        NixValue::Null
                    };
                    evaluator.gc_handle.alloc(Thunk::Value(value))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let entries = evaluator.gc_handle.alloc_vec(&mut substrings)?;
            Ok(NixValue::List(List { entries }))
        } else {
            // not a match
            Ok(NixValue::Null)
        }
    }
}

fn execute_deepseq(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let [e1, e2] = evaluator
        .force_thunk(argument)?
        .expect_list()?
        .expect_entries(&evaluator.gc_handle)?;

    // deep force the value of e1
    deep_force(evaluator, e1)?;

    // ... and return the value of arg 2
    evaluator.force_thunk(e2)
}

fn deep_force(evaluator: &mut Evaluator, value: GcPointer<Thunk>) -> Result<(), EvaluateError> {
    match evaluator.force_thunk(value)? {
        NixValue::List(list) => {
            let entries = evaluator.gc_handle.load(&list.entries).as_ref().to_owned();
            for entry in entries {
                deep_force(evaluator, entry)?;
            }
        }
        NixValue::Attrset(set) => {
            let entries = evaluator.gc_handle.load(&set.entries).as_ref().to_owned();
            for (_name, entry) in entries {
                println!("forcing: {:?}", _name.debug(&evaluator.gc_handle));
                deep_force(evaluator, entry)?;
            }
        }
        _ => {
            // all other values need not be forced further
        }
    }

    Ok(())
}

fn execute_remove_attrs(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let [set, list] = evaluator
        .force_thunk(argument)?
        .expect_list()?
        .expect_entries(&evaluator.gc_handle)?;
    let set = evaluator.force_thunk(set)?.expect_attrset()?;
    let mut set_entries = evaluator.gc_handle.load(&set.entries).as_ref().to_owned();

    let name_list = evaluator.force_thunk(list)?.expect_list()?;

    let name_list = evaluator
        .gc_handle
        .load(&name_list.entries)
        .as_ref()
        .to_owned()
        .into_iter()
        .map(|ptr| evaluator.force_thunk(ptr)?.expect_string())
        .collect::<Result<Vec<_>, _>>()?;
    let name_set: HashSet<_> = name_list
        .iter()
        .map(|ptr| ptr.load(&evaluator.gc_handle))
        .collect();

    set_entries.retain(|(name, _)| {
        let name = name.load(&evaluator.gc_handle);
        !name_set.contains(name)
    });

    let set_entries = evaluator.gc_handle.alloc_vec(&mut set_entries)?;

    Ok(NixValue::Attrset(Attrset {
        entries: set_entries,
    }))
}

fn execute_fromjson(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let source = evaluator
        .force_thunk(argument)?
        .expect_string()?
        .load(&evaluator.gc_handle)
        .to_owned();

    JsonParser::new(source.as_bytes(), &mut evaluator.gc_handle)
        .parse_json()
        .map_err(|e| EvaluateError::Misc(Box::new(e)))
}

fn execute_concat_lists(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let nested_list = evaluator.force_thunk(argument)?.expect_list()?;
    let nested_list = evaluator
        .gc_handle
        .load(&nested_list.entries)
        .as_ref()
        .to_owned()
        .into_iter()
        .map(|ptr| evaluator.force_thunk(ptr)?.expect_list())
        .collect::<Result<Vec<_>, _>>()?;

    let result = List::concat_lists(&nested_list, &mut evaluator.gc_handle)?;
    Ok(NixValue::List(result))
}

fn execute_filter_pick(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let argument = evaluator.force_thunk(argument)?.expect_attrset()?;
    let list = argument
        .get_and_force_entry_str(evaluator, "list")?
        .expect_list()?;
    let pickset = argument
        .get_and_force_entry_str(evaluator, "pickSet")?
        .expect_list()?;
    let pickset = evaluator
        .gc_handle
        .load(&pickset.entries)
        .as_ref()
        .to_owned()
        .into_iter()
        .map(|ptr| Ok(matches!(evaluator.force_thunk(ptr)?, NixValue::Bool(true))))
        .collect::<Result<Vec<_>, EvaluateError>>()?;

    let result_count = pickset.iter().filter(|b| **b).count();
    let mut result_buf = Vec::with_capacity(result_count);
    let result_iter = evaluator
        .gc_handle
        .load(&list.entries)
        .as_ref()
        .into_iter()
        .zip(pickset.into_iter())
        .filter_map(|(ptr, keep)| if keep { Some(ptr.clone()) } else { None });
    result_buf.extend(result_iter);

    let result_entries = evaluator.gc_handle.alloc_vec(&mut result_buf)?;

    Ok(NixValue::List(List {
        entries: result_entries,
    }))
}

fn execute_split(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let argument = evaluator.force_thunk(argument)?.expect_attrset()?;
    let regex = argument
        .get_and_force_entry_str(evaluator, "regex")?
        .expect_string()?;
    let search_str = argument
        .get_and_force_entry_str(evaluator, "str")?
        .expect_string()?;

    let regex = Regex::new(regex.load(&evaluator.gc_handle))
        .map_err(|e| EvaluateError::Misc(Box::new(e)))?;

    let mut entries = if regex.captures_len() == 1 {
        let mut prev_idx = 0;

        // first, determine the substring locations we have to cut
        let loaded_str = search_str.load(&evaluator.gc_handle);
        let string_len = loaded_str.len();
        let mut ranges = Vec::new();
        for location in regex.find_iter(loaded_str) {
            ranges.push(prev_idx..location.start());
            prev_idx = location.end();
        }

        // and construct our output.
        // since there is no output, we can just reuse the same empty list thunk
        let empty_list = Thunk::Value(NixValue::List(List {
            entries: evaluator.gc_handle.alloc_slice(&[])?,
        }));
        let empty_list = evaluator.gc_handle.alloc(empty_list)?;
        let mut result = Vec::with_capacity(2 * ranges.len() + 1);
        for location in ranges {
            let substring = search_str.get_substring(&mut evaluator.gc_handle, location)?;
            let substring_thunk = evaluator
                .gc_handle
                .alloc(Thunk::Value(NixValue::String(substring)))?;
            result.push(substring_thunk);
            result.push(empty_list.clone());
        }
        // and the final string piece
        let substring = search_str.get_substring(&mut evaluator.gc_handle, prev_idx..string_len)?;
        result.push(
            evaluator
                .gc_handle
                .alloc(Thunk::Value(NixValue::String(substring)))?,
        );

        result
    } else {
        let mut prev_idx = 0;
        let loaded_str = search_str.load(&evaluator.gc_handle);
        let string_len = loaded_str.len();

        // again, find the indexes that we need to alloc
        let mut ranges = Vec::new();
        for location in regex.captures_iter(loaded_str) {
            let mut captures = location.iter();
            let full_match = captures
                .next()
                .flatten()
                .expect("there should always be the full match group");

            let captures: Vec<_> = captures.map(|o| o.map(|m| m.range())).collect();

            ranges.push((prev_idx..full_match.start(), captures));
            prev_idx = full_match.end();
        }

        // and construct our result
        let mut result = Vec::with_capacity(2 * ranges.len() + 1);
        let mut subgroup_buf = Vec::with_capacity(regex.captures_len() - 1);
        for (location_before, matchgroups) in ranges {
            let substring = search_str.get_substring(&mut evaluator.gc_handle, location_before)?;
            result.push(
                evaluator
                    .gc_handle
                    .alloc(Thunk::Value(NixValue::String(substring)))?,
            );
            // and the capture groups
            let mut cached_null_thunk = None;
            for matchgroup in matchgroups {
                if let Some(range) = matchgroup {
                    let substring = search_str.get_substring(&mut evaluator.gc_handle, range)?;
                    subgroup_buf.push(
                        evaluator
                            .gc_handle
                            .alloc(Thunk::Value(NixValue::String(substring)))?,
                    );
                } else {
                    let null_thunk = cached_null_thunk
                        .get_or_insert_with(|| {
                            evaluator.gc_handle.alloc(Thunk::Value(NixValue::Null))
                        })
                        .clone()?;
                    subgroup_buf.push(null_thunk);
                };
            }
            let group_entries = evaluator.gc_handle.alloc_vec(&mut subgroup_buf)?;
            result.push(
                evaluator
                    .gc_handle
                    .alloc(Thunk::Value(NixValue::List(List {
                        entries: group_entries,
                    })))?,
            );
        }
        // and the final string piece
        let substring = search_str.get_substring(&mut evaluator.gc_handle, prev_idx..string_len)?;
        result.push(
            evaluator
                .gc_handle
                .alloc(Thunk::Value(NixValue::String(substring)))?,
        );

        result
    };

    let result_list = List {
        entries: evaluator.gc_handle.alloc_vec(&mut entries)?,
    };
    Ok(NixValue::List(result_list))
}

impl NixBuiltins {
    fn execute_import(
        &self,
        evaluator: &mut Evaluator<'_>,
        argument: GcPointer<Thunk>,
    ) -> Result<NixValue, EvaluateError> {
        let mut path_to_import = match evaluator.force_thunk(argument)? {
            NixValue::String(s) => s,
            NixValue::Path(p) => p.resolved,
            _ => return Err(EvaluateError::TypeError),
        };

        fn resolve_path(input: &Path) -> Result<PathBuf, io::Error> {
            if input == Path::new("<<<___builtins>>>") {
                return Ok(Path::new(input).to_owned());
            }
            let mut path = Path::new(input).canonicalize()?;
            if path.is_dir() {
                path.push("default.nix");
            }

            Ok(path)
        }

        let path_to_import_borrowed = Path::new(path_to_import.load(&evaluator.gc_handle));
        let path_to_import_owned = resolve_path(path_to_import_borrowed)?;
        if path_to_import_borrowed != &path_to_import_owned {
            let string = path_to_import_owned
                .as_os_str()
                .to_str()
                .ok_or(EvaluateError::TypeError)?;
            path_to_import = evaluator.gc_handle.alloc_string(string)?.into();
        }

        if let Some(cached) = self.import_cache.borrow().get(&path_to_import_owned) {
            // we have imported file file previously. Now we can just reuse the cached value.
            println!("using cached value for {path_to_import_owned:?}");
            return Ok(cached.clone());
        }

        println!("reading and compiling {path_to_import_owned:?}");

        let thunk = self
            .import_and_compile(
                &mut evaluator.gc_handle,
                path_to_import,
                &path_to_import_owned,
            )
            .map_err(|e| EvaluateError::ImportError(Box::new(e)))?;

        println!("evaluating top-level thunk");

        let result = evaluator.eval_expression(thunk)?;

        // cache the evaluated file content.
        // most likely it will be a lambda, but all other values are possible too.
        self.import_cache
            .borrow_mut()
            .insert(path_to_import_owned, result.clone());

        Ok(result)
    }

    fn import_and_compile(
        &self,
        gc_handle: &mut GcHandle,
        source_filename: NixString,
        source_path: &Path,
    ) -> Result<Thunk, InterpreterError> {
        if source_path == Path::new("<<<___builtins>>>") {
            compile_source_with_nix_filename(
                gc_handle,
                include_bytes!("./builtins.nix"),
                source_filename,
            )
        } else {
            let mut file_content = Vec::new();
            std::fs::File::open(source_path)?.read_to_end(&mut file_content)?;
            compile_source_with_nix_filename(gc_handle, &file_content, source_filename)
        }
    }
}

fn execute_map(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    let argument = evaluator.force_thunk(argument)?.expect_attrset()?;
    let list = argument
        .get_and_force_entry_str(evaluator, "list")?
        .expect_list()?;

    let mut list_entries = evaluator.gc_handle.load(&list.entries).as_ref().to_owned();

    if list_entries.is_empty() {
        // all empty lists are in principle identical.
        // no need to allocate anything more here.
        return Ok(NixValue::List(list));
    }

    let func = argument
        .get_entry_str(&evaluator.gc_handle, "func")
        .ok_or_else(|| EvaluateError::AttrsetKeyNotFound {
            attr_name: "func".to_owned(),
        })?;

    // since the call op will pop the argument off the context stack,
    // we only need to ensure that the argument is on top of the stack.
    let call_code = evaluator.gc_handle.alloc_slice(&[
        VmOp::LoadThunk(ValueSource::ContextReference(0)),
        VmOp::DuplicateThunk(ValueSource::ContextReference(1)),
        VmOp::TailCall,
    ])?;

    // we implement the map by replacing each source thunk pointer by
    // a newly allocated deferred thunk that models the call.
    for entry in list_entries.iter_mut() {
        let context = evaluator
            .gc_handle
            .alloc_slice(&[func.clone(), entry.clone()])?;
        let thunk = evaluator.gc_handle.alloc(Thunk::Deferred {
            context: ExecutionContext { entries: context },
            code: call_code.clone(),
        })?;
        *entry = thunk;
    }

    let entries = evaluator.gc_handle.alloc_vec(&mut list_entries)?;
    Ok(NixValue::List(List { entries }))
}

fn execute_to_string(
    argument: GcPointer<Thunk>,
    evaluator: &mut Evaluator,
) -> Result<value::NixString, EvaluateError> {
    let result = match evaluator.force_thunk(argument)? {
        NixValue::String(s) => s,
        NixValue::Bool(b) => {
            let value = if b { "1" } else { "" };
            evaluator.gc_handle.alloc_string(value)?.into()
        }
        NixValue::Null => evaluator.gc_handle.alloc_string("")?.into(),
        NixValue::Int(int) => {
            let val = format!("{int}");
            evaluator.gc_handle.alloc_string(&val)?.into()
        }
        NixValue::Float(float) => {
            let val = format!("{float}");
            evaluator.gc_handle.alloc_string(&val)?.into()
        }
        NixValue::Path(path) => path.resolved,
        NixValue::Attrset(_attrset) => {
            // the cases where the attrset can be stringified is handled by the
            // nix code in the prelude
            return Err(EvaluateError::TypeErrorWithMessage {
                msg: format!("cannot coerce attrset to string"),
            });
        }
        NixValue::List(list) => {
            let entries = evaluator.gc_handle.load(&list.entries).as_ref().to_owned();
            if entries.len() == 0 {
                return Ok(evaluator.gc_handle.alloc_string("")?.into());
            }
            if entries.len() == 1 {
                return execute_to_string(entries.into_iter().next().unwrap(), evaluator);
            }

            let mut strings = Vec::with_capacity(entries.len() * 2);
            let separator: NixString = evaluator.gc_handle.alloc_string(" ")?.into();
            for entry in entries {
                strings.push(execute_to_string(entry, evaluator)?);
                strings.push(separator.clone());
            }

            // we pushed one final separator too much.
            strings.pop();

            NixString::concat_many(
                strings.into_iter().map(Ok::<_, EvaluateError>),
                &mut evaluator.gc_handle,
            )?
        }
        NixValue::Function(_) => {
            return Err(EvaluateError::TypeErrorWithMessage {
                msg: format!("connot coerce function to string"),
            })
        }
        NixValue::Builtin(_) => {
            return Err(EvaluateError::TypeErrorWithMessage {
                msg: format!("cannot coerce function to string"),
            })
        }
    };

    Ok(result)
}

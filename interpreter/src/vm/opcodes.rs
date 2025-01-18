use super::value::{self, NixString, NixValue, Thunk};
use gc::{specialized_types::array::Array, GcPointer};
use gc_derive::Trace;

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Trace, Debug)]
pub enum ValueSource {
    ContextReference(u32),
    ThunkStackRef(u32),
}

#[derive(Trace, Clone, Debug, PartialEq, Eq)]
pub struct SourcePosition {
    pub line: u32,
    pub column: u32,
}

impl core::fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line, self.column))
    }
}

impl From<parser::ast::SourcePosition> for SourcePosition {
    fn from(value: parser::ast::SourcePosition) -> Self {
        Self {
            line: value.line,
            column: value.column,
        }
    }
}

#[derive(Debug, Trace, Clone)]
pub struct ExecutionContext {
    pub entries: GcPointer<Array<GcPointer<Thunk>>>,
    pub source_filename: NixString,
    pub source_positions: Option<GcPointer<Array<SourcePosition>>>,
}

#[derive(Debug, Trace)]
pub struct ThunkAllocArgs {
    pub code: GcPointer<Array<VmOp>>,
    /// an array for the source position for each vm op
    pub source_positions: GcPointer<Array<SourcePosition>>,
    pub context_id: u32,
    pub context_build_instructions: GcPointer<Array<ValueSource>>,
    pub source_file: value::NixString,
}

#[derive(Debug, Trace)]
pub struct LambdaAllocArgs {
    pub code: GcPointer<Array<VmOp>>,
    /// an array for the source position for each vm op
    pub source_locations: GcPointer<Array<SourcePosition>>,
    pub context_build_instructions: GcPointer<Array<ValueSource>>,
    pub call_requirements: LambdaCallType,
    pub source_file: value::NixString,
}

#[derive(Debug, Trace, Clone)]
pub enum LambdaCallType {
    Simple,
    Attrset {
        /// the keys for this function, given as pairs of the name
        /// and a bool set to true if the arg is required
        keys: GcPointer<Array<(value::NixString, bool)>>,
        includes_rest_pattern: bool,
    },
}

#[derive(Debug, Trace, Clone)]
pub enum VmOp {
    /// Allocates a list.
    /// pops count items from the stack and moves them into the list
    /// before pushing the list on the stack.
    AllocList(u32),

    /// pops the specified number of keys from the value stack and
    /// the specified number of chunks from the thunk stack
    /// builds an attrest from the pairs and pushes the result.
    BuildAttrset(u32),

    /// loads a value, either from the context or the local thunk stack
    /// and pushes it onto the value stack.
    LoadThunk(ValueSource),

    /// duplicates the thunk from the provided source and pushes it on the stack.
    DuplicateThunk(ValueSource),

    /// pops a thunk from the local thunk stack and overwrites the specified thunk
    /// with it.
    OverwriteThunk { stackref: u32 },

    /// pushes the provided immediate value on the stack.
    PushImmediate(GcPointer<NixValue>),

    /// Pushes the provided number of blackhole thunks onto the thunk context
    PushBlackholes(u32),

    /// drops the requested number of thunks from the local thunk context
    DropThunks(u32),

    /// assembles a thunk by following the provided instructions.
    /// It is then pushed to the thunk stack.
    AllocateThunk(GcPointer<ThunkAllocArgs>),

    /// allocates a lambda using the provided code
    /// and call arguments, as well as the provided context instructions
    AllocLambda(GcPointer<LambdaAllocArgs>),

    /// skips the provided number of instructions.
    /// You can think of it as a forward-only jump.
    Skip(u32),

    /// pops the top value from the stack and evaluates it as a boolean.
    /// if it evaluates truthy, the next instruction is executed as normal
    /// if it evaluates falsy, the supplied number of instructions are skipped
    SkipUnless(u32),

    /// pops n lists from the stack, concatenates them and pushes the result
    ConcatLists(u32),

    /// pops n strings from the stack, concatenates them and pushes the result
    ConcatStrings(u32),

    /// pops 2 values from the stack, concatenates them and pushes the result
    Add,

    /// pops 2 values from the stack, multiplies them and pushes the result
    Mul,

    /// pops 2 values from the stack, divides the lower by the upper and pushes the result
    Div,

    /// pops 2 values from the stack, subtracts the upper from the lower and pushes the result
    Sub,

    /// pops a value from the stack, multiplies it with -1 and pushes the result
    NumericNegate,
    /// pops a value from the stack, performs binary not and pushes the result
    BinaryNot,

    /// pops a value from the stack. this is the function to be called.
    /// then pops a thunk from the thunk stack and uses it as the argument.
    /// the function is evaluated with the thunk as argument and the result pushed to the stack
    Call,

    /// pops a value from the stack. this is the function to be called.
    /// then pops a thunk from the stack and uses it as the argument.
    ///
    /// then the current stack frame and context are cleared, all remaining instructions ignored
    /// and reinitialized as if the current function was the function to be tail called.
    TailCall,

    /// pops a string from the stack, converts it to a apth and pushes the result
    CastToPath { source_location: value::NixString },

    /// pops two values from the stack, performs a comparison and pushes the result
    Compare(CompareMode),

    /// pops two values from the stack, assets they are attrsets and merges the attributes
    /// from the second into the first, creating a new attrset.
    /// The resulting attrset is then pushed to the stack
    MergeAttrsets,

    /// first pops an attrset, then a name value from the stack.
    /// Then the attribute with that name is retrieved from the attrset
    /// and pushed to the stack.
    ///
    /// the push_error field controls the error reporting behaviour.
    /// if it is set to _false_ and the requested attribute is not present, a runtime error is
    /// raised.
    /// if it is set to _true_ and the attribute was present, an additional boolean value of
    /// _false_ is pushed to the stack after the attribute value was pushed.
    /// if it is set to _true_ and the attribute was not present, a value of _true_ is pushed to
    /// the stack. Since no attribute value could be retrieved in this case, the result boolen will
    /// be the only pushed value.
    GetAttribute { push_error: bool },

    /// first pops an attrset, then a name value from the stack.
    /// test if the key is present in the attrset, and push the result as a bool
    /// to the stack.
    HasAttribute,
}

#[derive(Clone, Copy, Debug, Trace)]
pub enum CompareMode {
    Equal,
    NotEqual,
    LessThanStrict,
    LessThanOrEqual,
    GreaterThanStrict,
    GreaterOrEqual,
}

use gc::{
    specialized_types::{array::Array, string::SimpleGcString},
    GcPointer, Trace,
};
use gc_derive::Trace;

#[derive(Debug)]
pub enum NixString {
    Simple(GcPointer<SimpleGcString>),
}

#[derive(Debug)]
pub enum NixValue {
    String(NixString),
    Bool(bool),
    Null,
    Int(i64),
    Float(f64),
    Path(NixString),
    Attrset(Attrset),
    Function(Function),
}

#[derive(Debug)]
pub struct Attrset {
    pub keys: GcPointer<Array<NixString>>,
    pub values: GcPointer<Array<Thunk>>,
}

#[derive(Debug)]
pub struct Function {
    pub args: FunctionArgs,
    pub body: Thunk,
}

#[derive(Debug)]
pub struct AttrsetFunctionArg {
    pub name: GcPointer<NixString>,
    pub default: Option<Thunk>,
}
#[derive(Debug)]
pub enum FunctionArgs {
    Single,
    AttrsetArgs {
        entries: GcPointer<Array<AttrsetFunctionArg>>,
        others_allowed: bool,
    },
}

#[derive(Debug, Trace)]
struct ContextReference(u32);

#[derive(Debug, Trace)]
pub enum VmOp {
    /// Duplicate the value at the specified position on the stack and pushes
    /// it to the stack top.
    Dup(u32),

    /// Allocates a list.
    /// pops count items from the stack and moves them into the list
    /// before pushing the list on the stack.
    AllocList(u32),

    /// pops an array of keys and an array of values from the stack and combines
    /// them into an attribute set.
    BuildAttrset,

    /// Loads the provided context item and pushes it on the stack.
    LoadContext(ContextReference),

    /// pushes the provided immediate value on the stack.
    PushImmediate(GcPointer<Thunk>),

    /// pops two values from the stack, adds them and pushes the result
    Add,

    /// pops two values from the stack, subs the top from the bottom and pushes the result
    Sub,

    /// pops n values from the stack and assembles them into an execution context.
    /// then combines it with the provided code instructions to generate a thunk.
    /// the allocated thunk is pushed on the stack.
    AllocateThunk {
        context_length: u16,
        code: GcPointer<Array<VmOp>>,
    },

    /// skips the provided number of instructions.
    /// You can think of it as a forward-only jump.
    Skip(u32),

    /// pops the top value from the stack and evaluates it as a boolean.
    /// if it evaluates truthy, the provided number of instructions is skipped,
    /// if it evaluates falsy, the next instruction is executed.
    SkipConditional(u32),

    //// pops n lists from the stack, concatenates them and pushes the result
    ConcatLists(u32),

    //// pops a value from the stack, multiplies it with -1 and pushes the result
    NumericNegate,
    //// pops a value from the stack, performs binary not and pushes the result
    BinaryNot,
}

#[derive(Debug, Trace)]
pub struct ExecutionContext {
    pub entries: GcPointer<Array<Thunk>>,
}

pub struct List {
    pub entries: GcPointer<Array<Thunk>>,
}

#[derive(Debug, Trace)]
pub enum Thunk {
    Blackhole,
    Value(GcPointer<NixValue>),
    Deferred {
        context: ExecutionContext,
        code: GcPointer<Array<VmOp>>,
    },
}

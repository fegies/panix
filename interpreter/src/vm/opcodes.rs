use super::value::{NixValue, Thunk};
use gc::{specialized_types::array::Array, GcPointer};
use gc_derive::Trace;

#[derive(Debug, Trace, Clone, Copy)]
pub struct ContextReference(u32);

#[derive(Debug, Trace, Clone)]
pub struct ExecutionContext {
    pub entries: GcPointer<Array<Thunk>>,
}

#[derive(Debug, Trace, Clone)]
pub enum VmOp {
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
    PushImmediate(GcPointer<NixValue>),

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
    /// if it evaluates truthy, the next instruction is executed as normal
    /// if it evaluates falsy, the supplied number of instructions are skipped
    SkipUnless(u32),

    /// pops n lists from the stack, concatenates them and pushes the result
    ConcatLists(u32),

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
    /// pops two values from the stack, applies the top to the bottom and pushes the result
    Call,

    /// pops a string from the stack, converts it to a apth and pushes the result
    CastToPath,

    /// pops two values from the stack, performs a comparison and pushes the result
    CompareEqual,
    /// pops two values from the stack, performs a comparison and pushes the result
    CompareNotEqual,
}

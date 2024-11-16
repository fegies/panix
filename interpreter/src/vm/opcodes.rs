use gc::{specialized_types::array::Array, GcPointer};
use gc_derive::Trace;
use parser::ast::BinopOpcode;

use super::value::{NixValue, Thunk};

#[derive(Debug, Trace, Clone, Copy)]
pub struct ContextReference(u32);

#[derive(Debug, Trace)]
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

    /// pops two values from the stack, executes the specified operation and pushes the result
    Binop(BinopOpcode),

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

    /// pops n lists from the stack, concatenates them and pushes the result
    ConcatLists(u32),

    /// pops a value from the stack, multiplies it with -1 and pushes the result
    NumericNegate,
    /// pops a value from the stack, performs binary not and pushes the result
    BinaryNot,
    /// pops two values from the stack, applies the top to the bottom and pushes the result
    Call,

    /// pops a string from the stack, converts it to a apth and pushes the result
    CastToPath,
}

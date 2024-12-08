use super::value::{NixValue, Thunk};
use gc::{specialized_types::array::Array, GcPointer};
use gc_derive::Trace;

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Trace)]
pub enum ValueSource {
    ContextReference(u32),
    ThunkStackRef(u32),
}

#[derive(Debug, Trace, Clone, Copy)]
pub struct ContextReference(pub u32);

#[derive(Debug, Trace, Clone)]
pub struct ExecutionContext {
    pub entries: GcPointer<Array<Thunk>>,
}

#[derive(Debug, Trace)]
pub struct ThunkAllocArgs {
    pub code: GcPointer<Array<VmOp>>,
    pub context_id: u32,
    pub context_build_instructions: GcPointer<Array<ValueSource>>,
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

    /// loads the provided value from the local Thunk stack
    LoadLocalThunk(u32),

    /// pushes the provided immediate value on the stack.
    PushImmediate(GcPointer<NixValue>),

    /// Pushes the provided number of blackhole thunks onto the thunk context
    PushBlackholes(u32),

    /// drops the requested number of thunks from the local thunk context
    DropThunks(u32),

    /// assembles a thunk by following the provided instructions.
    /// it is then written to the context at the specified slot.
    /// the slot is measured from the top with 0 being the top element of the stack
    AllocateThunk {
        slot: u16,
        args: GcPointer<ThunkAllocArgs>,
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
    /// pops two values from the stack, applies the top to the bottom and pushes the result
    Call,

    /// pops a string from the stack, converts it to a apth and pushes the result
    CastToPath,

    /// pops two values from the stack, performs a comparison and pushes the result
    CompareEqual,
    /// pops two values from the stack, performs a comparison and pushes the result
    CompareNotEqual,

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
}

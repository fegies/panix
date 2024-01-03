use std::sync::{Arc, Mutex};

use bumpalo::Bump;

/// This is a resolved Identifier.
pub struct Ident {
    id: u32,
}

pub struct IdentResolver {
    inner: Mutex<Inner>,
}

struct Inner {
    bump: Bump,
}

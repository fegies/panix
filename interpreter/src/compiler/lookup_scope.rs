pub struct LocalThunkRef(u32);

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ValueSource {
    ContextReference(u32),
    ThunkStackRef(u32),
}

struct ScopeLevel<'src> {
    local_entries: Vec<(&'src str, LocalThunkRef)>,

    inherited_keys: Vec<&'src str>,
    /// this vec represents what values are inherited from the parent
    /// and the order.
    /// They are always to be interpreted relative to the parent scope only
    inherited_values: Vec<ValueSource>,
}

pub struct ScopeBacking<'src> {
    levels: Vec<ScopeLevel<'src>>,

    /// an internal pool storing currently unused levels to reduce allocations
    unused_level_pool: Vec<ScopeLevel<'src>>,
}

impl<'src> ScopeBacking<'src> {
    pub fn new() -> Self {
        Self {
            levels: Vec::new(),
            unused_level_pool: Vec::new(),
        }
    }

    pub fn build_scope(&mut self) -> LookupScope<'src, '_> {
        let level = self.unused_level_pool.pop().unwrap_or_else(|| ScopeLevel {
            local_entries: Vec::new(),
            inherited_keys: Vec::new(),
            inherited_values: Vec::new(),
        });
        self.levels.push(level);
        LookupScope { backing: self }
    }

    fn push_thunkref(&mut self, name: &'src str, thunkref: LocalThunkRef) {
        self.levels
            .last_mut()
            .expect("at least 1 level should always exist because it is tied to the lifetime of the Scope")
            .local_entries
            .push((name, thunkref));
    }

    fn deref_ident(&mut self, ident: &'src str) -> Option<ValueSource> {
        self.deref_ident_at_height(ident, self.levels.len() - 1)
    }

    fn deref_ident_at_height(&mut self, ident: &'src str, level: usize) -> Option<ValueSource> {
        let level_entries = &mut self.levels[level];

        // first case is if our key comes from a thunk-local definition.
        if let Some((_, src)) = level_entries
            .local_entries
            .iter()
            .rev()
            .find(|(local_ident, _)| *local_ident == ident)
        {
            return Some(ValueSource::ThunkStackRef(src.0));
        }

        // second case is if we already have it included in our thunk context
        if let Some((idx, _)) = level_entries
            .inherited_keys
            .iter()
            .enumerate()
            .find(|(_, iident)| ident == **iident)
        {
            return Some(ValueSource::ContextReference(idx as u32));
        }

        // we have really not seen this entry before. We need to check our parent scopes (if any)

        if level > 0 {
            // we do still have a parent scope that we can check
            if let Some(parent_res) = self.deref_ident_at_height(ident, level - 1) {
                // and we got a hit from our parent.
                // we do need to add it to the context, so that future refs do not try to include
                // it twice.
                let level_entries = &mut self.levels[level];
                let res_idx = level_entries.inherited_keys.len() as u32;
                level_entries.inherited_keys.push(ident);
                level_entries.inherited_values.push(parent_res);

                // but what we return to the caller is actually just a reference to our local
                // context
                return Some(ValueSource::ContextReference(res_idx));
            }
        }

        // we searched to the root and did not find anything :(
        None
    }
}

pub struct LookupScope<'src, 'backing> {
    backing: &'backing mut ScopeBacking<'src>,
}

impl<'src, 'backing> LookupScope<'src, 'backing> {
    pub fn subscope<'s>(&'s mut self) -> LookupScope<'src, 's> {
        self.backing.build_scope()
    }

    pub fn push_local_thunkref(&mut self, name: &'src str, thunkref: LocalThunkRef) {
        self.backing.push_thunkref(name, thunkref);
    }

    pub fn deref_ident(&mut self, ident: &'src str) -> Option<ValueSource> {
        self.backing.deref_ident(ident)
    }

    pub fn into_inherit_context(self) -> Vec<ValueSource> {
        let level    = self.backing.levels.last_mut().expect("there should always be at least 1 level because it is tied to the lifetime of the LookupScope");
        core::mem::take(&mut level.inherited_values)
    }
}

impl Drop for LookupScope<'_, '_> {
    fn drop(&mut self) {
        if let Some(mut lvl) = self.backing.levels.pop() {
            lvl.local_entries.clear();
            lvl.inherited_keys.clear();
            lvl.inherited_values.clear();
            self.backing.unused_level_pool.push(lvl);
        }
    }
}

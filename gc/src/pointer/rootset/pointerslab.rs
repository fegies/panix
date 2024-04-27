pub struct Pointerslab {
    storage: Vec<u32>,
    free_top: Option<Slabkey>,
}

pub struct Slabkey(pub u32);

impl Pointerslab {
    pub const fn new() -> Self {
        Self {
            storage: Vec::new(),
            free_top: None,
        }
    }

    pub fn get(&self, key: &Slabkey) -> u32 {
        self.storage[key.0 as usize]
    }

    fn get_mut(&mut self, key: &Slabkey) -> &mut u32 {
        unsafe { self.storage.get_unchecked_mut(key.0 as usize) }
    }

    pub fn insert(&mut self, value: u32) -> Slabkey {
        if let Some(free_spot) = self.free_top.take() {
            let slot = self.get_mut(&free_spot);
            let next_free = core::mem::replace(slot, value);
            if next_free != u32::MAX {
                self.free_top = Some(Slabkey(next_free));
            }
            // the previously filled free spot is now our key
            free_spot
        } else {
            let spot = self.storage.len() as u32;
            self.storage.push(value);
            Slabkey(spot)
        }
    }

    /// remove the key, return the previously stored value
    pub fn remove(&mut self, key: Slabkey) -> u32 {
        let next_free = self.free_top.take().map_or(u32::MAX, |k| k.0);
        let res = core::mem::replace(self.get_mut(&key), next_free);
        self.free_top = Some(key);
        res
    }

    pub fn iter(&self) -> impl Iterator<Item = u32> + '_ {
        self.storage.iter().copied().filter(|e| *e < u32::MAX)
    }
}

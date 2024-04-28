pub struct Pointerslab {
    storage: Vec<u32>,
    free_slots: Vec<Slabkey>,
}

pub struct Slabkey(pub u32);

impl Pointerslab {
    pub const fn new() -> Self {
        Self {
            storage: Vec::new(),
            free_slots: Vec::new(),
        }
    }

    pub fn get(&self, key: &Slabkey) -> u32 {
        self.storage[key.0 as usize]
    }

    fn get_mut(&mut self, key: &Slabkey) -> &mut u32 {
        unsafe { self.storage.get_unchecked_mut(key.0 as usize) }
    }

    pub fn insert(&mut self, value: u32) -> Slabkey {
        if let Some(free_slot) = self.free_slots.pop() {
            let slot = self.get_mut(&free_slot);
            *slot = value;
            // the previously filled free spot is now our key
            free_slot
        } else {
            let spot = self.storage.len() as u32;
            self.storage.push(value);
            Slabkey(spot)
        }
    }

    /// remove the key, return the previously stored value
    pub fn remove(&mut self, key: Slabkey) -> u32 {
        let result = core::mem::replace(self.get_mut(&key), u32::MAX);
        self.free_slots.push(key);
        result
    }

    pub fn iter(&self) -> impl Iterator<Item = u32> + '_ {
        self.storage.iter().copied().filter(|e| *e < u32::MAX)
    }
}

use crate::pointer::RawHeapGcPointer;

pub struct Pointerslab {
    storage: Vec<RawHeapGcPointer>,
    free_slots: Vec<Slabkey>,
}

const INVALID_SLOT: RawHeapGcPointer = unsafe { RawHeapGcPointer::from_bits(u32::MAX) };

pub struct Slabkey(pub u32);

impl Pointerslab {
    pub const fn new() -> Self {
        Self {
            storage: Vec::new(),
            free_slots: Vec::new(),
        }
    }

    #[inline]
    pub fn get(&self, key: &Slabkey) -> RawHeapGcPointer {
        self.storage[key.0 as usize].clone()
    }

    #[inline]
    fn get_mut(&mut self, key: &Slabkey) -> &mut RawHeapGcPointer {
        unsafe { self.storage.get_unchecked_mut(key.0 as usize) }
    }

    pub fn insert(&mut self, value: RawHeapGcPointer) -> Slabkey {
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

    #[inline]
    /// remove the key, return the previously stored value
    pub fn remove(&mut self, key: Slabkey) -> RawHeapGcPointer {
        let result = core::mem::replace(self.get_mut(&key), INVALID_SLOT);
        self.free_slots.push(key);
        result
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut RawHeapGcPointer> {
        self.storage.iter_mut().filter(|e| e.content < u32::MAX)
    }
}

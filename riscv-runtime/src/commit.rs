pub static mut PUBLICS: CommittedPublics = CommittedPublics::new();

pub fn commit(n: u32) {
    unsafe { PUBLICS.commit(n) }
}

pub(crate) fn finalize() -> [u64; 4] {
    unsafe { PUBLICS.finalize() }
}

pub struct CommittedPublics {
    state: [u64; 12],
    buffer_size: u8,
}

impl CommittedPublics {
    pub const fn new() -> Self {
        Self {
            state: [0; 12],
            buffer_size: 0,
        }
    }

    pub fn commit(&mut self, n: u32) {
        self.state[self.buffer_size as usize + 4] = n as u64;

        self.buffer_size += 1;

        if self.buffer_size == 4 {
            self.buffer_size = 0;
            self.update_state();
        }
    }

    fn update_state(&mut self) {
        crate::hash::native_hash(&mut self.state);
    }

    pub fn finalize(&mut self) -> [u64; 4] {
        // Prevents hash of empty.
        self.commit(1);

        if self.buffer_size != 0 {
            for n in self.state[self.buffer_size as usize + 4..8].iter_mut() {
                *n = 0;
            }
            self.update_state();
        }
        let hash = self.state[0..4].try_into().unwrap();

        *self = Self::new();

        hash
    }
}

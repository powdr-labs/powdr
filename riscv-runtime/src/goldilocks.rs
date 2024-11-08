pub const PRIME: u64 = 0xffffffff00000001;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Goldilocks(
    // TODO: maybe represent this as a u32, which ends up as a full word in Powdr.
    // (the conversion to and from u64 would require ecalls)
    u64,
);

impl Goldilocks {
    /// Panics if the value is not in the field (i.e. panics if value >= PRIME).
    pub fn new(value: u64) -> Self {
        assert!(value < PRIME);
        Self(value)
    }

    /// Precondition for safety: value must be in the field (i.e. value < PRIME).
    pub unsafe fn new_unchecked(value: u64) -> Self {
        Self(value)
    }

    // TODO: maybe add other operations we can accelerate with ecalls:
    // - add
    // - mul
    // - neg
    // - inv
}

impl From<Goldilocks> for u64 {
    /// Return the canonical u64 representation of the field element.
    fn from(value: Goldilocks) -> u64 {
        value.0
    }
}

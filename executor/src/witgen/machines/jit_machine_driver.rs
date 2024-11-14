use powdr_number::FieldElement;

use crate::witgen::FixedData;

use super::MachineParts;

pub struct JitMachineDriver<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    parts: MachineParts<'a, T>,
    block_size: usize,
    latch_row: usize,
}

impl<'a, T: FieldElement> JitMachineDriver<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        parts: MachineParts<'a, T>,
        block_size: usize,
        latch_row: usize,
    ) -> Self {
        JitMachineDriver {
            fixed_data,
            parts,
            block_size,
            latch_row,
        }
    }
}

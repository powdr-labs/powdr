use bit_vec::BitVec;
use powdr_number::FieldElement;

use crate::witgen::{EvalError, FixedData, MutableState, QueryCallback};

use super::{LookupCell, MachineParts};

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

    pub fn can_answer_lookup(&self, identity_id: u64, known_inputs: &BitVec) -> bool {
        // TODO
        return false;
    }

    pub fn process_lookup_direct<'b, 'c, Q: QueryCallback<T>>(
        &self,
        _mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        identity_id: u64,
        values: Vec<LookupCell<'c, T>>,
    ) -> Result<bool, EvalError<T>> {
        todo!();
    }
}

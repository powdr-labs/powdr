use bit_vec::BitVec;
use powdr_number::FieldElement;

use crate::witgen::{
    data_structures::finalizable_data::CompactDataRef,
    machines::{LookupCell, MachineParts},
    util::try_to_simple_poly,
    EvalError, FixedData, MutableState, QueryCallback,
};

pub struct JitProcessor<'a, T: FieldElement> {
    _fixed_data: &'a FixedData<'a, T>,
    parts: MachineParts<'a, T>,
    _block_size: usize,
    latch_row: usize,
}

impl<'a, T: FieldElement> JitProcessor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        parts: MachineParts<'a, T>,
        block_size: usize,
        latch_row: usize,
    ) -> Self {
        JitProcessor {
            _fixed_data: fixed_data,
            parts,
            _block_size: block_size,
            latch_row,
        }
    }

    pub fn can_answer_lookup(&self, _identity_id: u64, _known_inputs: &BitVec) -> bool {
        // TODO call the JIT compiler here.
        false
    }

    pub fn process_lookup_direct<'b, 'c, 'd, Q: QueryCallback<T>>(
        &self,
        _mutable_state: &'b mut MutableState<'a, 'b, T, Q>,
        connection_id: u64,
        values: Vec<LookupCell<'c, T>>,
        mut data: CompactDataRef<'d, T>,
    ) -> Result<bool, EvalError<T>> {
        // Transfer inputs.
        let right = self.parts.connections[&connection_id].right;
        for (e, v) in right.expressions.iter().zip(&values) {
            match v {
                LookupCell::Input(&v) => {
                    let col = try_to_simple_poly(e).unwrap();
                    data.set(self.latch_row as i32, col.poly_id.id as u32, v);
                }
                LookupCell::Output(_) => {}
            }
        }

        // Just some code here to avoid "unused" warnings.
        // This code will not be called as long as `can_answer_lookup` returns false.
        data.get(self.latch_row as i32, 0);

        unimplemented!();
    }
}

use std::collections::BTreeMap;

use bit_vec::BitVec;
use powdr_number::FieldElement;

use crate::witgen::{
    data_structures::finalizable_data::{ColumnLayout, CompactDataRef},
    machines::{LookupCell, MachineParts},
    EvalError, FixedData, MutableState, QueryCallback,
};

use super::{
    block_machine_processor::BlockMachineProcessor,
    compiler::{compile_effects, WitgenFunction},
    variable::Variable,
};

pub struct JitProcessor<'a, T: FieldElement> {
    processor: BlockMachineProcessor<'a, T>,
    column_layout: ColumnLayout,
    witgen_functions: BTreeMap<(u64, BitVec), Option<WitgenFunction<T>>>,
}

impl<'a, T: FieldElement> JitProcessor<'a, T> {
    pub fn new(
        fixed_data: &'a FixedData<'a, T>,
        parts: MachineParts<'a, T>,
        block_size: usize,
        latch_row: usize,
        metadata: ColumnLayout,
    ) -> Self {
        let processor =
            BlockMachineProcessor::new(fixed_data, parts.clone(), block_size, latch_row);

        JitProcessor {
            processor,
            column_layout: metadata,
            witgen_functions: BTreeMap::new(),
        }
    }

    pub fn can_answer_lookup(&mut self, identity_id: u64, known_args: &BitVec) -> bool {
        self.ensure_cache(identity_id, known_args);
        self.witgen_functions
            .get(&(identity_id, known_args.clone()))
            .unwrap()
            .is_some()
    }

    fn ensure_cache(&mut self, identity_id: u64, known_args: &BitVec) {
        let cache_key = (identity_id, known_args.clone());
        if self.witgen_functions.contains_key(&cache_key) {
            return;
        }

        let f = self
            .processor
            .generate_code(identity_id, known_args)
            .ok()
            .and_then(|code| {
                let known_inputs = known_args
                    .iter()
                    .enumerate()
                    .filter_map(|(i, b)| if b { Some(Variable::Param(i)) } else { None })
                    .collect::<Vec<_>>();
                compile_effects(
                    self.column_layout.first_column_id,
                    self.column_layout.column_count,
                    &known_inputs,
                    &code,
                )
                // TODO: This should never fail, but right now it does!
                .ok()
            });
        assert!(self.witgen_functions.insert(cache_key, f).is_none())
    }

    pub fn process_lookup_direct<'c, 'd, Q: QueryCallback<T>>(
        &self,
        mutable_state: &MutableState<'a, T, Q>,
        connection_id: u64,
        mut values: Vec<LookupCell<'c, T>>,
        data: CompactDataRef<'d, T>,
    ) -> Result<bool, EvalError<T>> {
        let known_args = values
            .iter()
            .map(|cell| cell.is_input())
            .collect::<BitVec>();

        let f = self
            .witgen_functions
            .get(&(connection_id, known_args))
            .expect("Cache miss")
            .as_ref()
            .expect("cannot answer call");
        f.call(mutable_state, &mut values, data);

        Ok(true)
    }
}

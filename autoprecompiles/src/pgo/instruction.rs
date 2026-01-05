use std::{
    cmp::Reverse,
    collections::BTreeMap,
};

use itertools::Itertools;

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterBasicBlock, AdapterVmConfig, PgoAdapter},
    execution_profile::ExecutionProfile,
    pgo::create_apcs_for_all_blocks,
    EmpiricalConstraints, PowdrConfig,
};

pub struct InstructionPgo<A> {
    _marker: std::marker::PhantomData<A>,
    data: ExecutionProfile,
}

impl<A> InstructionPgo<A> {
    pub fn with_pgo_data(data: ExecutionProfile) -> Self {
        Self {
            _marker: std::marker::PhantomData,
            data,
        }
    }
}

impl<A: Adapter> PgoAdapter for InstructionPgo<A> {
    type Adapter = A;

    fn create_apcs_with_pgo(
        &self,
        blocks: Vec<AdapterBasicBlock<Self::Adapter>>,
        // execution count of blocks (indexes into the `blocks` vec)
        block_exec_count: Option<Vec<u32>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        _labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        tracing::info!(
            "Generating autoprecompiles with instruction PGO for {} blocks",
            blocks.len()
        );

        if config.autoprecompiles == 0 {
            return vec![];
        }

        // ensure blocks are valid for APC
        let block_exec_count = block_exec_count.unwrap();
        blocks
            .iter()
            .enumerate()
            .for_each(|(idx, b)| assert!(block_exec_count[idx] > 0 && b.statements.len() > 1));

        tracing::debug!(
            "Retained {} basic blocks after filtering by pc_idx_count",
            blocks.len()
        );

        // sort blocks by execution count * number of instructions
        let blocks = blocks.into_iter()
            .enumerate()
            .map(|(idx, block)| {
                let count = block_exec_count[idx];
                (count, block)
            })
            .sorted_by_key(|(count, b)| Reverse(count * b.statements.len() as u32))
            .inspect(|(count, b)| {
                let number_of_instructions = b.statements.len();
                let value = count * number_of_instructions as u32;
                tracing::debug!(
                    "Basic block start_pc: {start_pc}, other_pcs: {:?}, value: {value}, frequency: {count}, number_of_instructions: {number_of_instructions}",
                    b.other_pcs,
                    start_pc = b.start_pc,
                );
            })
            .map(|(_, block)| block).collect::<Vec<_>>();

        create_apcs_for_all_blocks::<Self::Adapter>(
            blocks,
            config,
            vm_config,
            empirical_constraints,
        )
    }

    fn profiling_data(&self) -> Option<&ExecutionProfile> {
        Some(&self.data)
    }
}

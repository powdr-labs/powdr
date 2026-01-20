use std::{cmp::Reverse, collections::BTreeMap};

use itertools::Itertools;

use crate::{
    EmpiricalConstraints, PowdrConfig, adapter::{Adapter, AdapterApcWithStats, AdapterPGOBlocks, AdapterVmConfig, PgoAdapter}, blocks::PGOBlocks, execution_profile::ExecutionProfile, pgo::create_apcs_for_all_blocks
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
        pgo_blocks: AdapterPGOBlocks<Self::Adapter>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        _labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        if config.autoprecompiles == 0 {
            return vec![];
        }

        let PGOBlocks {
            blocks,
            counts,
            execution_bb_runs: _,
        } = pgo_blocks;

        tracing::info!(
            "Generating autoprecompiles with instruction PGO for {} blocks",
            blocks.len()
        );

        // ensure blocks are valid for APC
        let counts = counts.unwrap();
        blocks
            .iter()
            .enumerate()
            .for_each(|(idx, b)| assert!(counts[idx] > 0 && b.statements().count() > 1));

        tracing::debug!(
            "Retained {} basic blocks after filtering by pc_idx_count",
            blocks.len()
        );

        // TODO: similar to the cell PGO case, we should take into account that selecting some superblock might lower the execution count of conflicting blocks.

        // sort blocks by execution count * number of instructions
        let blocks = blocks.into_iter()
            .enumerate()
            .map(|(idx, block)| {
                let count = counts[idx];
                (count, block)
            })
            .sorted_by_key(|(count, b)| Reverse(count * b.statements().count() as u32))
            .inspect(|(count, b)| {
                let number_of_instructions = b.statements().count();
                let value = count * number_of_instructions as u32;
                tracing::debug!(
                    "Basic block pcs: {:?}, value: {value}, frequency: {count}, number_of_instructions: {number_of_instructions}",
                    b.original_pcs(),
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

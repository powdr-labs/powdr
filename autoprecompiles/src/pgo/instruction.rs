use std::{cmp::Reverse, collections::BTreeMap};

use itertools::Itertools;

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterExecutionBlocks, AdapterVmConfig, PgoAdapter},
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
        exec_blocks: AdapterExecutionBlocks<Self::Adapter>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        _labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        tracing::info!(
            "Generating autoprecompiles with instruction PGO for {} blocks",
            exec_blocks.blocks.len()
        );

        if config.autoprecompiles == 0 {
            return vec![];
        }

        let blocks = exec_blocks
            .blocks
            .into_iter()
            // sort by frequency * number of instructions in the block, descending
            .sorted_by_key(|block_and_stats| {
                Reverse(block_and_stats.count.unwrap() * block_and_stats.block.statements().count() as u32)
            })
            .map(|block_and_stats| {
                let block = block_and_stats.block;
                assert!(block.is_basic_block(), "Instruction PGO does not support superblocks");
                let frequency = block_and_stats.count.unwrap();
                let number_of_instructions = block.statements().count();
                let value = frequency * number_of_instructions as u32;

                tracing::debug!(
                    "Basic block start_pc: {}, value: {}, frequency: {}, number_of_instructions: {}",
                    block.pcs().next().unwrap(),
                    value,
                    frequency,
                    number_of_instructions,
                );

                block
            })
            .collect();

        create_apcs_for_all_blocks::<Self::Adapter>(
            blocks,
            config,
            vm_config,
            empirical_constraints,
        )
    }

    fn execution_profile(&self) -> Option<&ExecutionProfile> {
        Some(&self.data)
    }
}

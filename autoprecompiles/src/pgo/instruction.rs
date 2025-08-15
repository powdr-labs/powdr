use std::collections::HashMap;

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterVmConfig, PgoAdapter},
    blocks::BasicBlock,
    pgo::create_apcs_for_all_blocks,
    PowdrConfig,
};

pub struct InstructionPgo<A> {
    _marker: std::marker::PhantomData<A>,
    data: HashMap<u64, u32>,
}

impl<A> InstructionPgo<A> {
    pub fn with_pgo_data(data: HashMap<u64, u32>) -> Self {
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
        blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        // Filter out blocks that should be skipped according to the adapter.
        let mut blocks: Vec<_> = blocks
            .into_iter()
            .filter(|block| !Self::Adapter::should_skip_block(block))
            .collect();

        tracing::info!(
            "Generating autoprecompiles with instruction PGO for {} blocks",
            blocks.len()
        );

        if config.autoprecompiles == 0 {
            return vec![];
        }

        let pgo_program_pc_count: &HashMap<u64, u32> = &self.data;
        // drop any block whose start index cannot be found in pc_idx_count,
        // because a basic block might not be executed at all.
        // Also only keep basic blocks with more than one original instruction.
        blocks.retain(|b| pgo_program_pc_count.contains_key(&b.start_pc) && b.statements.len() > 1);

        tracing::debug!(
            "Retained {} basic blocks after filtering by pc_idx_count",
            blocks.len()
        );

        // cost = cells_saved_per_row
        blocks.sort_by(|a, b| {
            let a_cnt = pgo_program_pc_count[&a.start_pc];
            let b_cnt = pgo_program_pc_count[&b.start_pc];
            (b_cnt * (b.statements.len() as u32)).cmp(&(a_cnt * (a.statements.len() as u32)))
        });

        // Debug print blocks by descending cost
        for block in &blocks {
            let frequency = pgo_program_pc_count[&block.start_pc];
            let number_of_instructions = block.statements.len();
            let value = frequency * number_of_instructions as u32;

            tracing::debug!(
                    "Basic block start_pc: {start_pc}, value: {value}, frequency: {frequency}, number_of_instructions: {number_of_instructions}",
                    start_pc = block.start_pc,
                );
        }

        create_apcs_for_all_blocks::<Self::Adapter>(blocks, config, vm_config)
    }

    fn pc_execution_count(&self, pc: u64) -> Option<u32> {
        self.data.get(&pc).cloned()
    }
}

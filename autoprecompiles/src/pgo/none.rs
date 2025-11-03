use std::collections::BTreeMap;

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterVmConfig, PgoAdapter, ApcArithmetization},
    blocks::BasicBlock,
    pgo::create_apcs_for_all_blocks,
    PowdrConfig,
};

pub struct NonePgo<A, Air> {
    _marker: std::marker::PhantomData<(A, Air)>,
}

// TODO: derive with explicit bounds
impl<A, Air> Default for NonePgo<A, Air> {
    fn default() -> Self {
        Self {
            _marker: std::marker::PhantomData,
        }
    }
}

impl<A: Adapter, Air: ApcArithmetization<A>> PgoAdapter for NonePgo<A, Air> {
    type Adapter = A;
    type Air = Air;

    fn create_apcs_with_pgo(
        &self,
        mut blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        _labels: BTreeMap<u64, Vec<String>>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        // cost = number_of_original_instructions
        blocks.sort_by(|a, b| b.statements.len().cmp(&a.statements.len()));

        // Debug print blocks by descending cost
        for block in &blocks {
            tracing::debug!(
                "Basic block start_pc: {}, number_of_instructions: {}",
                block.start_pc,
                block.statements.len(),
            );
        }

        create_apcs_for_all_blocks::<Self::Adapter, Self::Air>(blocks, config, vm_config)
    }
}

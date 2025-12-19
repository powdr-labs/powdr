use std::collections::BTreeMap;

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterBasicBlock, AdapterVmConfig, PgoAdapter},
    pgo::create_apcs_for_all_blocks,
    EmpiricalConstraints, PowdrConfig,
};

pub struct NonePgo<A> {
    _marker: std::marker::PhantomData<A>,
}

// TODO: derive with explicit bounds
impl<A> Default for NonePgo<A> {
    fn default() -> Self {
        Self {
            _marker: std::marker::PhantomData,
        }
    }
}

impl<A: Adapter> PgoAdapter for NonePgo<A> {
    type Adapter = A;

    fn create_apcs_with_pgo(
        &self,
        mut blocks: Vec<AdapterBasicBlock<Self::Adapter>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        _labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
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

        create_apcs_for_all_blocks::<Self::Adapter>(
            blocks,
            config,
            vm_config,
            empirical_constraints,
        )
    }
}

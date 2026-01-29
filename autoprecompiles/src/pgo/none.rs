use std::collections::BTreeMap;

use derivative::Derivative;

use crate::{
    EmpiricalConstraints, PowdrConfig, adapter::{Adapter, AdapterApcWithStats, AdapterProgramBlocks, AdapterVmConfig, PgoAdapter}, pgo::create_apcs_for_all_blocks
};

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
pub struct NonePgo<A> {
    _marker: std::marker::PhantomData<A>,
}

impl<A: Adapter> PgoAdapter for NonePgo<A> {
    type Adapter = A;

    fn create_apcs_with_pgo(
        &self,
        blocks: AdapterProgramBlocks<Self::Adapter>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        _labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        // cost = number_of_original_instructions
        let mut blocks = blocks.blocks;
        blocks.sort_by_key(|b| std::cmp::Reverse(b.statements().count()));

        // Debug print blocks by descending cost
        for block in &blocks {
            tracing::debug!(
                "Basic block pcs: {:?}, number_of_instructions: {}",
                block.original_bb_pcs(),
                block.statements().count(),
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

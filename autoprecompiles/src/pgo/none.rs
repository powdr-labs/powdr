use std::{cmp::Reverse, collections::BTreeMap};

use derivative::Derivative;
use itertools::Itertools;

use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterExecutionBlocks, AdapterVmConfig, PgoAdapter},
    pgo::create_apcs_for_all_blocks,
    EmpiricalConstraints, PowdrConfig,
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
        exec_blocks: AdapterExecutionBlocks<Self::Adapter>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        _labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        let blocks = exec_blocks
            .blocks
            .into_iter()
            // sort by number of instructions in the block, descending
            .sorted_by_key(|block_and_stats| {
                Reverse(block_and_stats.block.statements().count() as u32)
            })
            .map(|block_and_stats| {
                let block = block_and_stats.block;
                assert!(
                    block.is_basic_block(),
                    "None PGO does not support superblocks"
                );
                tracing::debug!(
                    "Basic block start_pc: {}, number_of_instructions: {}",
                    block.pcs().next().unwrap(),
                    block.statements().count(),
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
}

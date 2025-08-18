use crate::{
    adapter::{Adapter, AdapterApcWithStats, AdapterVmConfig, PgoAdapter},
    blocks::BasicBlock,
    pgo::create_apcs_for_all_blocks,
    PowdrConfig,
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
        blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        tracing::info!(
            "Generating autoprecompiles with no PGO for {} blocks",
            blocks.len()
        );

        create_apcs_for_all_blocks::<Self::Adapter>(blocks, config, vm_config)
    }
}

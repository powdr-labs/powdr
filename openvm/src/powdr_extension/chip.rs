// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/auipc/core.rs#L1)

#[cfg(feature = "cuda")]
mod cuda {
    use std::{cell::RefCell, rc::Rc};

    use openvm_circuit::arch::DenseRecordArena;
    use openvm_stark_sdk::p3_baby_bear::BabyBear;

    use crate::{
        extraction_utils::{OriginalAirs, OriginalVmConfig},
        powdr_extension::{
            executor::OriginalArenas,
            trace_generator::cuda::{PowdrPeripheryInstancesGpu, PowdrTraceGeneratorGpu},
            PowdrPrecompile,
        },
    };

    pub struct PowdrChipGpu {
        pub name: String,
        pub record_arena_by_air_name: Rc<RefCell<OriginalArenas<DenseRecordArena>>>,
        pub trace_generator: PowdrTraceGeneratorGpu,
    }

    impl PowdrChipGpu {
        pub(crate) fn new(
            precompile: PowdrPrecompile<BabyBear>,
            original_airs: OriginalAirs<BabyBear>,
            base_config: OriginalVmConfig,
            periphery: PowdrPeripheryInstancesGpu,
        ) -> Self {
            let PowdrPrecompile {
                name,
                apc,
                apc_record_arena_gpu: apc_record_arena,
                ..
            } = precompile;
            let trace_generator =
                PowdrTraceGeneratorGpu::new(apc, original_airs, base_config, periphery);

            Self {
                name,
                record_arena_by_air_name: apc_record_arena,
                trace_generator,
            }
        }
    }
}
#[cfg(feature = "cuda")]
pub use cuda::*;

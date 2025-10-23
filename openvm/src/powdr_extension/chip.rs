// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/auipc/core.rs#L1)

use std::{cell::RefCell, collections::BTreeMap, rc::Rc, sync::Arc};

use crate::{
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    powdr_extension::{
        executor::OriginalArenas,
        trace_generator::cpu::{PowdrPeripheryInstancesCpu, PowdrTraceGeneratorCpu},
        PowdrPrecompile,
    },
    Instr,
};

use itertools::Itertools;
use openvm_circuit::arch::MatrixRecordArena;
use openvm_stark_backend::{
    p3_air::{Air, BaseAir},
    rap::ColumnsAir,
};

use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, PartitionedBaseAir},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicEvaluator, AlgebraicReference, WitnessEvaluator},
    Apc,
};

pub struct PowdrChipCpu {
    pub name: String,
    pub record_arena_by_air_name: Rc<RefCell<OriginalArenas<MatrixRecordArena<BabyBear>>>>,
    pub trace_generator: PowdrTraceGeneratorCpu,
}

impl PowdrChipCpu {
    pub(crate) fn new(
        precompile: PowdrPrecompile<BabyBear>,
        original_airs: OriginalAirs<BabyBear>,
        base_config: OriginalVmConfig,
        periphery: PowdrPeripheryInstancesCpu,
    ) -> Self {
        let PowdrPrecompile {
            name,
            apc,
            apc_record_arena_cpu: apc_record_arena,
            ..
        } = precompile;
        let trace_generator =
            PowdrTraceGeneratorCpu::new(apc, original_airs, base_config, periphery);

        Self {
            name,
            record_arena_by_air_name: apc_record_arena,
            trace_generator,
        }
    }
}

pub struct PowdrAir<F> {
    /// The columns in arbitrary order
    columns: Vec<AlgebraicReference>,
    apc: Arc<Apc<F, Instr<F>>>,
}

impl<F: PrimeField32> ColumnsAir<F> for PowdrAir<F> {
    fn columns(&self) -> Option<Vec<String>> {
        Some(self.columns.iter().map(|c| (*c.name).clone()).collect())
    }
}

impl<F: PrimeField32> PowdrAir<F> {
    pub fn new(apc: Arc<Apc<F, Instr<F>>>) -> Self {
        Self {
            columns: apc.machine().main_columns().collect(),
            apc,
        }
    }
}

impl<F: PrimeField32> BaseAir<F> for PowdrAir<F> {
    fn width(&self) -> usize {
        let res = self.columns.len();
        assert!(res > 0);
        res
    }
}

// No public values, but the trait is implemented
impl<F: PrimeField32> BaseAirWithPublicValues<F> for PowdrAir<F> {}

impl<AB: InteractionBuilder> Air<AB> for PowdrAir<AB::F>
where
    AB::F: PrimeField32,
{
    fn eval(&self, builder: &mut AB) {
        let main = builder.main();
        let witnesses = main.row_slice(0);
        // TODO: cache?
        let witness_values: BTreeMap<u64, AB::Var> = self
            .columns
            .iter()
            .map(|c| c.id)
            .zip_eq(witnesses.iter().cloned())
            .collect();

        let witness_evaluator = WitnessEvaluator::new(&witness_values);

        for constraint in &self.apc.machine().constraints {
            let constraint = witness_evaluator.eval_constraint(constraint);
            builder.assert_zero(constraint.expr);
        }

        for interaction in &self.apc.machine().bus_interactions {
            let interaction = witness_evaluator.eval_bus_interaction(interaction);
            // TODO: is this correct?
            let count_weight = 1;

            builder.push_interaction(
                interaction.id as u16,
                interaction.args,
                interaction.mult,
                count_weight,
            );
        }
    }
}

impl<F: PrimeField32> PartitionedBaseAir<F> for PowdrAir<F> {}

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

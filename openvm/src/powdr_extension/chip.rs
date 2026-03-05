use std::{cell::RefCell, rc::Rc};

use openvm_circuit::arch::MatrixRecordArena;
use openvm_stark_sdk::p3_baby_bear::BabyBear;

use crate::{
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    isa::OpenVmISA,
    powdr_extension::{
        executor::OriginalArenas,
        trace_generator::cpu::{PowdrPeripheryInstancesCpu, PowdrTraceGeneratorCpu},
        PowdrPrecompile,
    },
};

use std::collections::BTreeMap;

use itertools::Itertools;
use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_air::{Air, BaseAir},
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, ColumnsAir, PartitionedBaseAir},
};
use powdr_autoprecompiles::{
    expression::{AlgebraicEvaluator, AlgebraicReference, WitnessEvaluator},
    symbolic_machine::SymbolicMachine,
};

pub struct PowdrChipCpu<ISA: OpenVmISA> {
    pub name: String,
    pub record_arena_by_air_name: Rc<RefCell<OriginalArenas<MatrixRecordArena<BabyBear>>>>,
    pub trace_generator: PowdrTraceGeneratorCpu<ISA>,
}

impl<ISA: OpenVmISA> PowdrChipCpu<ISA> {
    pub fn new(
        precompile: PowdrPrecompile<BabyBear, ISA>,
        original_airs: OriginalAirs<BabyBear, ISA>,
        base_config: OriginalVmConfig<ISA>,
        periphery: PowdrPeripheryInstancesCpu<ISA>,
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
    machine: SymbolicMachine<F>,
}

impl<F: PrimeField32> ColumnsAir<F> for PowdrAir<F> {
    fn columns(&self) -> Option<Vec<String>> {
        Some(self.columns.iter().map(|c| (*c.name).clone()).collect())
    }
}

impl<F: PrimeField32> PowdrAir<F> {
    pub fn new(machine: SymbolicMachine<F>) -> Self {
        Self {
            columns: machine.main_columns().collect(),
            machine,
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

        for constraint in &self.machine.constraints {
            let constraint = witness_evaluator.eval_constraint(constraint);
            builder.assert_zero(constraint.expr);
        }

        for interaction in &self.machine.bus_interactions {
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
        executor::OriginalArenas,
        extraction_utils::{OriginalAirs, OriginalVmConfig},
        isa::OpenVmISA,
        trace_generator::cuda::{PowdrPeripheryInstancesGpu, PowdrTraceGeneratorGpu},
        vm::PowdrPrecompile,
    };

    pub struct PowdrChipGpu<ISA: OpenVmISA> {
        pub name: String,
        pub record_arena_by_air_name: Rc<RefCell<OriginalArenas<DenseRecordArena>>>,
        pub trace_generator: PowdrTraceGeneratorGpu<ISA>,
    }

    impl<ISA: OpenVmISA> PowdrChipGpu<ISA> {
        pub(crate) fn new(
            precompile: PowdrPrecompile<BabyBear, ISA>,
            original_airs: OriginalAirs<BabyBear, ISA>,
            base_config: OriginalVmConfig<ISA>,
            periphery: PowdrPeripheryInstancesGpu<ISA>,
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

// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/auipc/core.rs#L1)

use std::{cell::RefCell, collections::BTreeMap, rc::Rc, sync::Arc};

use crate::{
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    powdr_extension::{
        executor::OriginalArenas,
        trace_generator::{PowdrPeripheryInstances, PowdrTraceGenerator},
    },
    Instr,
};

use super::PowdrPrecompile;
use itertools::Itertools;
use openvm_stark_backend::{
    p3_air::{Air, BaseAir},
    p3_matrix::dense::DenseMatrix,
    prover::{hal::ProverBackend, types::AirProvingContext},
    rap::ColumnsAir,
};

use openvm_stark_backend::{
    interaction::InteractionBuilder,
    p3_field::PrimeField32,
    p3_matrix::Matrix,
    rap::{BaseAirWithPublicValues, PartitionedBaseAir},
    Chip,
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicEvaluator, AlgebraicReference, WitnessEvaluator},
    Apc,
};

#[cfg(feature = "cuda")]
use crate::DeviceMatrix;
#[cfg(feature = "cuda")]
use openvm_cuda_backend::chip::get_empty_air_proving_ctx;

pub struct PowdrChip {
    pub name: String,
    pub record_arena_by_air_name: Rc<RefCell<OriginalArenas>>,
    pub trace_generator: PowdrTraceGenerator,
}

impl PowdrChip {
    pub(crate) fn new(
        precompile: PowdrPrecompile<BabyBear>,
        original_airs: OriginalAirs<BabyBear>,
        base_config: OriginalVmConfig,
        periphery: PowdrPeripheryInstances,
        record_arena_by_air_name: Rc<RefCell<OriginalArenas>>,
    ) -> Self {
        let PowdrPrecompile { name, apc, .. } = precompile;
        let trace_generator = PowdrTraceGenerator::new(apc, original_airs, base_config, periphery);

        Self {
            name,
            record_arena_by_air_name,
            trace_generator,
        }
    }
}

#[cfg(not(feature = "cuda"))]
impl<R, PB: ProverBackend<Matrix = Arc<DenseMatrix<BabyBear>>>> Chip<R, PB> for PowdrChip {
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let trace = self
            .trace_generator
            .generate_witness(self.record_arena_by_air_name.take());

        AirProvingContext::simple(Arc::new(trace), vec![])
    }
}

#[cfg(feature = "cuda")]
impl<R, PB: ProverBackend<Matrix = DeviceMatrix<BabyBear>>> Chip<R, PB> for PowdrChip {
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        if self.record_arena_by_air_name.borrow().number_of_calls() == 0 {
            get_empty_air_proving_ctx::<PB>()
        } else {
            let trace = self
                .trace_generator
                .generate_witness(self.record_arena_by_air_name.take());

            AirProvingContext::simple(trace, vec![])
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

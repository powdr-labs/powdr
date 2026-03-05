use std::{collections::HashMap, sync::Arc};

use itertools::Itertools;
use openvm_circuit::{
    arch::{ChipInventory, MatrixRecordArena, VmChipComplex},
    system::SystemChipInventory,
    utils::next_power_of_two_or_zero,
};
use openvm_stark_backend::{
    config::Val,
    p3_field::{Field, FieldAlgebra, PrimeField32},
    p3_matrix::dense::{DenseMatrix, RowMajorMatrix},
    prover::{cpu::CpuBackend, hal::ProverBackend, types::AirProvingContext},
    Chip,
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::trace_handler::TraceTrait;
use powdr_constraint_solver::constraint_system::ComputationMethod;

use crate::{
    chip::PowdrChipCpu,
    executor::OriginalArenas,
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    isa::OpenVmISA,
    trace_generator::cpu::periphery::SharedPeripheryChipsCpu,
    BabyBearSC, IsaApc, PeripheryBusIds,
};
pub mod periphery;

/// A dummy inventory used for execution of autoprecompiles
/// It extends the `SdkVmConfigExecutor` and `SdkVmConfigPeriphery`, providing them with shared, pre-loaded periphery chips to avoid memory allocations by each SDK chip
pub type DummyChipComplex<SC> =
    VmChipComplex<SC, MatrixRecordArena<Val<SC>>, CpuBackend<SC>, SystemChipInventory<SC>>;

/// The shared chips which can be used by the PowdrChip.
#[derive(Clone)]
pub struct PowdrPeripheryInstancesCpu<S> {
    /// The real chips used for the main execution.
    pub real: S,
    /// The dummy chips used for all APCs. They share the range checker but create new instances of the bitwise lookup chip and the tuple range checker.
    pub dummy: S,
    /// The bus ids of the periphery
    pub bus_ids: PeripheryBusIds,
}

pub struct PowdrTraceGeneratorCpu<ISA: OpenVmISA> {
    pub apc: IsaApc<BabyBear, ISA>,
    pub original_airs: OriginalAirs<BabyBear, ISA>,
    pub config: OriginalVmConfig<ISA>,
    pub periphery: PowdrPeripheryInstancesCpu<SharedPeripheryChipsCpu<ISA>>,
}

impl<ISA: OpenVmISA> PowdrTraceGeneratorCpu<ISA> {
    pub fn new(
        apc: IsaApc<BabyBear, ISA>,
        original_airs: OriginalAirs<BabyBear, ISA>,
        config: OriginalVmConfig<ISA>,
        periphery: PowdrPeripheryInstancesCpu<SharedPeripheryChipsCpu<ISA>>,
    ) -> Self {
        Self {
            apc,
            original_airs,
            config,
            periphery,
        }
    }

    pub fn generate_witness(
        &self,
        original_arenas: OriginalArenas<MatrixRecordArena<BabyBear>>,
    ) -> DenseMatrix<BabyBear> {
        use powdr_autoprecompiles::trace_handler::{generate_trace, TraceData};

        let width = self.apc.machine().main_columns().count();

        let mut original_arenas = match original_arenas {
            OriginalArenas::Initialized(arenas) => arenas,
            OriginalArenas::Uninitialized => {
                // if the arenas are uninitialized, the apc was not called, so we return an empty trace
                return RowMajorMatrix::new(vec![], width);
            }
        };

        let num_apc_calls = original_arenas.number_of_calls;

        let chip_inventory: ChipInventory<
            BabyBearSC,
            MatrixRecordArena<BabyBear>,
            CpuBackend<BabyBearSC>,
        > = {
            let airs = ISA::create_dummy_airs(self.config.config(), self.periphery.dummy.clone())
                .expect("Failed to create dummy airs");

            ISA::create_dummy_chip_complex_cpu(
                self.config.config(),
                airs,
                self.periphery.dummy.clone(),
            )
            .expect("Failed to create chip complex")
            .inventory
        };

        let dummy_trace_by_air_name: HashMap<String, SharedCpuTrace<BabyBear>> = chip_inventory
            .chips()
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(insertion_idx, chip)| {
                let air_name = chip_inventory.airs().ext_airs()[insertion_idx].name();

                let record_arena = {
                    match original_arenas.take_real_arena(&air_name) {
                        Some(ra) => ra,
                        None => return None, // skip this iteration, because we only have record arena for chips that are used
                    }
                };

                let shared_trace = chip.generate_proving_ctx(record_arena).common_main.unwrap();

                Some((air_name, shared_trace.into()))
            })
            .collect();

        let TraceData {
            dummy_values,
            dummy_trace_index_to_apc_index_by_instruction,
            apc_poly_id_to_index,
            columns_to_compute,
        } = generate_trace(
            &dummy_trace_by_air_name,
            &self.original_airs,
            num_apc_calls,
            &self.apc,
        );

        // allocate for apc trace
        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut values = <BabyBear as FieldAlgebra>::zero_vec(height * width);

        // go through the final table and fill in the values
        values
            // a record is `width` values
            // TODO: optimize by parallelizing on chunks of rows, currently fails because `dyn AnyChip<MatrixRecordArena<Val<SC>>>` is not `Send`
            .chunks_mut(width)
            .zip(dummy_values)
            .for_each(|(row_slice, dummy_values)| {
                // map the dummy rows to the autoprecompile row

                use powdr_autoprecompiles::expression::MappingRowEvaluator;
                for (dummy_row, dummy_trace_index_to_apc_index) in dummy_values
                    .iter()
                    .map(|r| &r.data[r.start..r.start + r.length])
                    .zip_eq(&dummy_trace_index_to_apc_index_by_instruction)
                {
                    for (dummy_trace_index, apc_index) in dummy_trace_index_to_apc_index {
                        row_slice[*apc_index] = dummy_row[*dummy_trace_index];
                    }
                }

                // Fill in the columns we have to compute from other columns
                // (these are either new columns or for example the "is_valid" column).
                for (column, computation_method) in columns_to_compute {
                    let col_index = apc_poly_id_to_index[&column.id];
                    row_slice[col_index] = match computation_method {
                        ComputationMethod::Constant(c) => *c,
                        ComputationMethod::QuotientOrZero(e1, e2) => {
                            use powdr_number::ExpressionConvertible;

                            let divisor_val = e2.to_expression(&|n| *n, &|column_ref| {
                                row_slice[apc_poly_id_to_index[&column_ref.id]]
                            });
                            if divisor_val.is_zero() {
                                BabyBear::ZERO
                            } else {
                                divisor_val.inverse()
                                    * e1.to_expression(&|n| *n, &|column_ref| {
                                        row_slice[apc_poly_id_to_index[&column_ref.id]]
                                    })
                            }
                        }
                    };
                }

                let evaluator = MappingRowEvaluator::new(row_slice, &apc_poly_id_to_index);

                // replay the side effects of this row on the main periphery
                self.apc
                    .machine()
                    .bus_interactions
                    .iter()
                    .for_each(|interaction| {
                        use powdr_autoprecompiles::expression::{
                            AlgebraicEvaluator, ConcreteBusInteraction,
                        };

                        let ConcreteBusInteraction { id, mult, args } =
                            evaluator.eval_bus_interaction(interaction);
                        self.periphery.real.apply(
                            id as u16,
                            mult.as_canonical_u32(),
                            args.map(|arg| arg.as_canonical_u32()),
                            &self.periphery.bus_ids,
                        );
                    });
            });

        RowMajorMatrix::new(values, width)
    }
}

/// A wrapper around a DenseMatrix to implement `TraceTrait` which is required for `generate_trace`.
pub struct SharedCpuTrace<F> {
    pub matrix: Arc<RowMajorMatrix<F>>,
}

impl<F: Send + Sync> TraceTrait<F> for SharedCpuTrace<F> {
    type Values = Vec<F>;

    fn width(&self) -> usize {
        self.matrix.width
    }

    fn values(&self) -> &Self::Values {
        &self.matrix.values
    }
}

impl<F> From<Arc<RowMajorMatrix<F>>> for SharedCpuTrace<F> {
    fn from(matrix: Arc<RowMajorMatrix<F>>) -> Self {
        Self { matrix }
    }
}

impl<R, PB: ProverBackend<Matrix = Arc<RowMajorMatrix<BabyBear>>>, ISA: OpenVmISA> Chip<R, PB>
    for PowdrChipCpu<ISA>
{
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let trace = self
            .trace_generator
            .generate_witness(self.record_arena_by_air_name.take());

        AirProvingContext::simple(Arc::new(trace), vec![])
    }
}

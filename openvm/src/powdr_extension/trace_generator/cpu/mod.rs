use std::{collections::HashMap, sync::Arc};

use itertools::Itertools;
use openvm_circuit::{
    arch::{AirInventory, MatrixRecordArena},
    utils::next_power_of_two_or_zero,
};
use openvm_stark_backend::{
    p3_field::FieldAlgebra,
    p3_matrix::dense::{DenseMatrix, RowMajorMatrix},
    prover::{
        hal::ProverBackend,
        types::{AirProvingContext, AirProvingContexts},
    },
    Chip,
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{trace_handler::TraceTrait, Apc};
use powdr_constraint_solver::constraint_system::ComputationMethod;

use crate::{
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    powdr_extension::{
        chip::PowdrChipCpu,
        executor::OriginalArenas,
        trace_generator::{common::create_dummy_airs, cpu::inventory::create_dummy_chip_complex},
    },
    BabyBearSC, Instr,
};
use openvm_stark_backend::p3_field::PrimeField32;

use openvm_stark_backend::p3_field::Field;

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrTraceGenerator
mod periphery;

pub use periphery::PowdrPeripheryInstancesCpu;

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

impl<R, PB: ProverBackend<Matrix = Arc<RowMajorMatrix<BabyBear>>>> Chip<R, PB> for PowdrChipCpu {
    fn generate_proving_ctx(&self, records: R) -> AirProvingContext<PB> {
        unreachable!()
    }

    fn generate_proving_ctxs(&self, _: R) -> AirProvingContexts<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let (trace, rejected) = self
            .trace_generator
            .generate_witness(self.record_arena_by_air_name.take());

        let rejected = rejected
            .into_iter()
            .map(|(key, (rows, values))| (key, (rows, AirProvingContext::simple(values, vec![]))))
            .collect();

        AirProvingContexts {
            main: AirProvingContext::simple(Arc::new(trace), vec![]),
            rejected,
        }
    }
}

pub struct PowdrTraceGeneratorCpu {
    pub apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
    pub original_airs: OriginalAirs<BabyBear>,
    pub config: OriginalVmConfig,
    pub periphery: PowdrPeripheryInstancesCpu,
}

impl PowdrTraceGeneratorCpu {
    pub fn new(
        apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
        original_airs: OriginalAirs<BabyBear>,
        config: OriginalVmConfig,
        periphery: PowdrPeripheryInstancesCpu,
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
        mut original_arenas: OriginalArenas<MatrixRecordArena<BabyBear>>,
    ) -> (
        DenseMatrix<BabyBear>,
        HashMap<String, (Vec<usize>, Arc<DenseMatrix<BabyBear>>)>,
    ) {
        use powdr_autoprecompiles::trace_handler::{generate_trace, TraceData};

        let num_apc_calls = original_arenas.number_of_calls();
        if num_apc_calls == 0 {
            // If the APC isn't called, early return with an empty trace.
            let width = self.apc.machine().main_columns().count();
            return (RowMajorMatrix::new(vec![], width), HashMap::default());
        }

        let chip_inventory = {
            let airs: AirInventory<BabyBearSC> =
                create_dummy_airs(&self.config.sdk_config.sdk, self.periphery.dummy.clone())
                    .expect("Failed to create dummy airs");

            create_dummy_chip_complex(
                &self.config.sdk_config.sdk,
                airs,
                self.periphery.dummy.clone(),
            )
            .expect("Failed to create chip complex")
            .inventory
        };

        let mut dummy_trace_by_air_name: HashMap<String, SharedCpuTrace<BabyBear>> = chip_inventory
            .chips()
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(insertion_idx, chip)| {
                let air_name = chip_inventory.airs().ext_airs()[insertion_idx].name();

                let record_arena = {
                    match original_arenas.take_arena(&air_name) {
                        Some(ra) => ra,
                        None => return None, // skip this iteration, because we only have record arena for chips that are used
                    }
                };

                let shared_trace = chip.generate_proving_ctx(record_arena).common_main.unwrap();

                Some((air_name, SharedCpuTrace::from(shared_trace)))
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

        let mut rejected: HashMap<String, Vec<usize>> = dummy_trace_by_air_name
            .keys()
            .cloned()
            .map(|key| (key, vec![]))
            .collect();

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
                    .map(|r| &r.data[r.start()..r.start() + r.length])
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
                        ComputationMethod::InverseOrZero(expr) => {
                            use powdr_number::ExpressionConvertible;

                            let expr_val = expr.to_expression(&|n| *n, &|column_ref| {
                                row_slice[apc_poly_id_to_index[&column_ref.id]]
                            });
                            if expr_val.is_zero() {
                                BabyBear::ZERO
                            } else {
                                expr_val.inverse()
                            }
                        }
                    };
                }

                let evaluator = MappingRowEvaluator::new(row_slice, &apc_poly_id_to_index);

                // check the constraints and bus interactions
                let row_is_valid = true;

                if row_is_valid {
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
                } else {
                    // for each original row
                    for original_row_reference in dummy_values {
                        // TODO replay the side effects of this row on the real periphery

                        // add the row index to the rejected set
                        rejected
                            .get_mut(original_row_reference.air_id)
                            .unwrap()
                            .push(original_row_reference.row_index);
                    }
                }
            });

        // merge the rejected indices with the traces
        let rejected = rejected
            .into_iter()
            .map(|(name, indices)| {
                (
                    name.clone(),
                    (
                        indices,
                        dummy_trace_by_air_name.remove(&name).unwrap().matrix,
                    ),
                )
            })
            .collect();

        (RowMajorMatrix::new(values, width), rejected)
    }
}

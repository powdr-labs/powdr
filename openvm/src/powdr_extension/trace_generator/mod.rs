use std::{collections::HashMap, sync::Arc};

use itertools::Itertools;
use openvm_circuit::{arch::AirInventory, utils::next_power_of_two_or_zero};
use openvm_stark_backend::{
    p3_field::{Field, FieldAlgebra, PrimeField32},
    p3_matrix::dense::{DenseMatrix, RowMajorMatrix},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicEvaluator, ConcreteBusInteraction, MappingRowEvaluator},
    trace_handler::{generate_trace, Trace, TraceData},
    Apc,
};
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_number::ExpressionConvertible;

use crate::{
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    powdr_extension::{
        executor::OriginalArenas,
        trace_generator::inventory::{create_dummy_airs, create_dummy_chip_complex},
    },
    BabyBearSC, Instr,
};

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrTraceGenerator
mod periphery;

pub use periphery::PowdrPeripheryInstances;

pub struct PowdrTraceGenerator {
    pub apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
    pub original_airs: OriginalAirs<BabyBear>,
    pub config: OriginalVmConfig,
    pub periphery: PowdrPeripheryInstances,
}

impl PowdrTraceGenerator {
    pub fn new(
        apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
        original_airs: OriginalAirs<BabyBear>,
        config: OriginalVmConfig,
        periphery: PowdrPeripheryInstances,
    ) -> Self {
        Self {
            apc,
            original_airs,
            config,
            periphery,
        }
    }

    /// Generates the witness for the autoprecompile. The result will be a matrix of
    /// size `next_power_of_two(number_of_calls) * width`, where `width` is the number of
    /// nodes in the APC circuit.
    pub fn generate_witness(
        &self,
        mut original_arenas: OriginalArenas,
    ) -> RowMajorMatrix<BabyBear> {
        let num_apc_calls = original_arenas.number_of_calls();
        if num_apc_calls == 0 {
            // If the APC isn't called, early return with an empty trace.
            let width = self.apc.machine().main_columns().count();
            return RowMajorMatrix::new(vec![], width);
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

        let arenas = original_arenas.arenas_mut();

        let dummy_trace_by_air_name: HashMap<String, Trace<BabyBear>> = chip_inventory
            .chips()
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(insertion_idx, chip)| {
                let air_name = chip_inventory.airs().ext_airs()[insertion_idx].name();

                let record_arena = {
                    match arenas.remove(&air_name) {
                        Some(ra) => ra,
                        None => return None, // skip this iteration, because we only have record arena for chips that are used
                    }
                };

                // Arc<DenseMatrix>
                let shared_trace = chip.generate_proving_ctx(record_arena).common_main.unwrap();
                // Reference count should be 1 here as it's just created
                let DenseMatrix { values, width, .. } =
                    Arc::try_unwrap(shared_trace).expect("Can't unwrap shared Arc<DenseMatrix>");

                Some((air_name, Trace::new(values, width)))
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
            BabyBear::ONE,
        );

        // allocate for apc trace
        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut values = <BabyBear as FieldAlgebra>::zero_vec(height * width);

        // go through the final table and fill in the values
        values
            // a record is `width` values
            // TODO: optimize by parallelizing on chunks of rows, currently fails because `dyn AnyChip<MatrixRecordArena<Val<SC>>>` is not `Senf`
            .chunks_mut(width)
            .zip(dummy_values)
            .for_each(|(row_slice, dummy_values)| {
                // map the dummy rows to the autoprecompile row
                for (dummy_row, dummy_trace_index_to_apc_index) in dummy_values
                    .iter()
                    .zip_eq(&dummy_trace_index_to_apc_index_by_instruction)
                {
                    for (dummy_trace_index, apc_index) in dummy_trace_index_to_apc_index {
                        row_slice[*apc_index] = dummy_row[*dummy_trace_index];
                    }
                }

                // Fill in the columns we have to compute from other columns
                // (these are either new columns or for example the "is_valid" column).
                for (column, computation_method) in &columns_to_compute {
                    let col_index = apc_poly_id_to_index[&column.id];
                    row_slice[col_index] = match computation_method {
                        ComputationMethod::Constant(c) => *c,
                        ComputationMethod::InverseOrZero(expr) => {
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

                // replay the side effects of this row on the main periphery
                self.apc
                    .machine()
                    .bus_interactions
                    .iter()
                    .for_each(|interaction| {
                        let ConcreteBusInteraction { id, mult, args } =
                            evaluator.eval_bus_interaction(interaction);
                        self.periphery.real.apply(
                            id as u16,
                            mult.as_canonical_u32(),
                            args.map(|arg| arg.as_canonical_u32()),
                        );
                    });
            });

        RowMajorMatrix::new(values, width)
    }
}

use std::{collections::HashMap, sync::Arc};

use itertools::Itertools;
use openvm_circuit::{arch::AirInventory, utils::next_power_of_two_or_zero};
use openvm_cuda_common::d_buffer::DeviceBuffer;
use openvm_stark_backend::{
    p3_field::{Field, FieldAlgebra, PrimeField32},
    p3_matrix::dense::{DenseMatrix, RowMajorMatrix},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicEvaluator, ConcreteBusInteraction, MappingRowEvaluator},
    trace_handler::{generate_trace, TraceData, TraceTrait},
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

#[cfg(feature = "cuda")]
use crate::DeviceMatrix;
#[cfg(feature = "cuda")]
use openvm_cuda_common::copy::{MemCopyD2H, MemCopyH2D};
#[cfg(feature = "cuda")]
use openvm_stark_backend::prover::hal::MatrixDimensions;

#[cfg(feature = "cuda")]
pub type Witness<T> = DeviceMatrix<T>;

#[cfg(not(feature = "cuda"))]
pub type Witness<T> = RowMajorMatrix<T>;

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrTraceGenerator
mod periphery;

pub use periphery::PowdrPeripheryInstances;

/// A wrapper around a DenseMatrix to implement `TraceTrait` which is required for `generate_trace`.
pub struct SharedCpuTrace<F> {
    matrix: Arc<DenseMatrix<F>>,
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

impl<F> From<Arc<DenseMatrix<F>>> for SharedCpuTrace<F> {
    fn from(matrix: Arc<DenseMatrix<F>>) -> Self {
        Self { matrix }
    }
}

/// A wrapper around a DeviceMatrix to implement `TraceTrait` which is required for `generate_trace`.
pub struct SharedGpuTrace<F> {
    matrix: DenseMatrix<F>,
}

impl<F: Send + Sync> TraceTrait<F> for SharedGpuTrace<F> {
    type Values = Vec<F>;

    fn width(&self) -> usize {
        self.matrix.width
    }

    fn values(&self) -> &Self::Values {
        &self.matrix.values
    }
}

impl<F: Clone + Send + Sync + Copy + Default> From<DeviceMatrix<F>> for SharedGpuTrace<F> {
    fn from(matrix: DeviceMatrix<F>) -> Self {
        let width = matrix.width();
        let height = matrix.height();
        let values = matrix.buffer().to_host().unwrap();

        // Create column major matrix to transpose
        let column_major_matrix = DenseMatrix::new(values, height);
        // Transpose
        let row_major_matrix = column_major_matrix.transpose();

        Self {
            matrix: row_major_matrix,
        }
    }
}

#[cfg(feature = "cuda")]
type SharedTrace<F> = SharedGpuTrace<F>;
#[cfg(not(feature = "cuda"))]
type SharedTrace<F> = SharedCpuTrace<F>;

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

    #[cfg(not(feature = "cuda"))]
    pub fn generate_witness(&self, mut original_arenas: OriginalArenas) -> Witness<BabyBear> {
        assert!(
            original_arenas.number_of_calls() > 0,
            "APC must be called to generate witness"
        );
        let (values, width, _) = self.generate_witness_values(original_arenas);
        Witness::new(values, width)
    }

    #[cfg(feature = "cuda")]
    pub fn generate_witness(&self, mut original_arenas: OriginalArenas) -> Witness<BabyBear> {
        assert!(
            original_arenas.number_of_calls() > 0,
            "APC must be called to generate witness"
        );
        // Values are already padded
        let (values, width, height) = self.generate_witness_values(original_arenas);
        device_matrix_from_values(values, width, height)
    }

    /// Generates the witness for the autoprecompile. The result will be a matrix of
    /// size `next_power_of_two(number_of_calls) * width`, where `width` is the number of
    /// nodes in the APC circuit.
    pub fn generate_witness_values(
        &self,
        mut original_arenas: OriginalArenas,
    ) -> (Vec<BabyBear>, usize, usize) {
        let num_apc_calls = original_arenas.number_of_calls();

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

        let dummy_trace_by_air_name: HashMap<String, SharedTrace<BabyBear>> = chip_inventory
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

                let shared_trace = chip.generate_proving_ctx(record_arena).common_main.unwrap();

                Some((air_name, SharedTrace::from(shared_trace)))
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
                for (dummy_row, dummy_trace_index_to_apc_index) in dummy_values
                    .iter()
                    .zip_eq(&dummy_trace_index_to_apc_index_by_instruction)
                {
                    let dummy_row =
                        &dummy_row.data[dummy_row.start..(dummy_row.start + dummy_row.length)];

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
                        // self.periphery.real.apply(
                        //     id as u16,
                        //     mult.as_canonical_u32(),
                        //     args.map(|arg| arg.as_canonical_u32()),
                        // );
                    });
            });

        (values, width, height)
    }
}

#[cfg(feature = "cuda")]
pub fn device_matrix_from_values(
    values: Vec<BabyBear>,
    width: usize,
    height: usize,
) -> DeviceMatrix<BabyBear> {
    // TODO: we copy the values from host (CPU) to device (GPU), and should study how to generate APC trace natively in GPU
    // Transpose back to column major matrix before sending to device
    let row_major_matrix = DenseMatrix::new(values, width);
    let column_major_matrix = row_major_matrix.transpose();
    let device_buffer = column_major_matrix.values.to_device().unwrap();
    DeviceMatrix::new(Arc::new(device_buffer), height, width)
}

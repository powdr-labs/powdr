use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

use itertools::Itertools;
use openvm_circuit::{arch::AirInventory, utils::next_power_of_two_or_zero};
use openvm_cuda_common::d_buffer::DeviceBuffer;
use openvm_stark_backend::{p3_field::FieldAlgebra, p3_matrix::dense::DenseMatrix};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{trace_handler::TraceTrait, Apc};
use powdr_constraint_solver::constraint_system::ComputationMethod;

use crate::{
    cuda_abi::{self, OriginalAir, Subst, OpCode, DevInteraction, DevArgSpan},
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    powdr_extension::{
        executor::OriginalArenas,
        trace_generator::inventory::{create_dummy_airs, create_dummy_chip_complex},
    },
    BabyBearSC, Instr,
};
use crate::bus_map::DEFAULT_TUPLE_RANGE_CHECKER;
use powdr_autoprecompiles::{SymbolicBusInteraction, expression::AlgebraicExpression};
use powdr_expression::{AlgebraicBinaryOperation, AlgebraicUnaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperator};
use openvm_stark_backend::p3_field::PrimeField32;

#[cfg(feature = "cuda")]
use crate::DeviceMatrix;
#[cfg(feature = "cuda")]
use openvm_cuda_common::copy::MemCopyH2D;
#[cfg(feature = "cuda")]
use openvm_stark_backend::prover::hal::MatrixDimensions;

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
    pub matrix: DeviceMatrix<F>,
}

impl<F: Send + Sync> TraceTrait<F> for SharedGpuTrace<F> {
    type Values = DeviceBuffer<F>;

    fn width(&self) -> usize {
        self.matrix.width()
    }

    fn values(&self) -> &Self::Values {
        &self.matrix.buffer()
    }
}

impl<F> From<DeviceMatrix<F>> for SharedGpuTrace<F> {
    fn from(matrix: DeviceMatrix<F>) -> Self {
        Self { matrix }
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

    pub fn generate_witness(&self, original_arenas: OriginalArenas) -> SharedTrace<BabyBear> {
        assert!(
            original_arenas.number_of_calls() > 0,
            "APC must be called to generate witness"
        );
        // Values are already padded
        self.generate_witness_values(original_arenas)
    }

    /// Generates the witness for the autoprecompile. The result will be a matrix of
    /// size `next_power_of_two(number_of_calls) * width`, where `width` is the number of
    /// nodes in the APC circuit.
    pub fn generate_witness_values(
        &self,
        mut original_arenas: OriginalArenas,
    ) -> SharedTrace<BabyBear> {
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

        // Map from apc poly id to its index in the final apc trace
        let apc_poly_id_to_index: BTreeMap<u64, usize> = self
            .apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        // allocate for apc trace
        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(num_apc_calls);

        // Create a host-side buffer to prefill the output, column major, zero-initialized
        // TODO: do this on GPU instead, to avoid large host<->device copies
        let mut h_output = vec![BabyBear::ZERO; height * width];

        // Prefill `is_valid` column to 1 for the number of calls
        for (column_idx, computation_method) in &self.apc.machine.derived_columns {
            let col_index = apc_poly_id_to_index[&column_idx.id];
            match computation_method {
                ComputationMethod::Constant(c) => {
                    for row in 0..num_apc_calls {
                        h_output[row + col_index * height] = *c;
                    }
                }
                ComputationMethod::InverseOrZero(_) => {
                    unimplemented!("Cannot prefill inverse_or_zero without full row data")
                }
            }
        }

        let d_output = h_output.to_device().unwrap();

        let mut output = DeviceMatrix::<BabyBear>::new(Arc::new(d_output), height, width);

        // Prepare `OriginalAir` and `Subst` arrays
        let (airs, substitutions) = {
            self.apc
                // go through original instructions
                .instructions()
                .iter()
                // along with their substitutions
                .zip_eq(self.apc.subs())
                // map to `(air_name, substitutions)`
                .map(|(instr, subs)| (&self.original_airs.opcode_to_air[&instr.0.opcode], subs))
                // group by air name. This results in `HashMap<air_name, Vec<subs>>` where the length of the vector is the number of rows which are created in this air, per apc call
                .into_group_map()
                // go through each air and its substitutions
                .iter()
                .fold(
                    (Vec::new(), Vec::new()),
                    |(mut airs, mut substitutions), (air_name, subs_by_row)| {
                        // Find the substitutions that map to an apc column
                        let filtered_substitutions: Vec<Subst> = subs_by_row
                            .iter()
                            // enumerate over them to get the row index inside the air block
                            .enumerate()
                            .flat_map(|(row, subs)| {
                                // for each substitution, map to `Subst` struct if it exists in apc
                                subs.iter()
                                    .enumerate()
                                    .filter_map(|(dummy_index, poly_id)| {
                                        // Check if this dummy column is present in the final apc row
                                        apc_poly_id_to_index
                                            .get(poly_id)
                                            // If it is, map the dummy index to the apc index
                                            .map(|apc_index| Subst {
                                                col: dummy_index as i32,
                                                row: row as i32,
                                                apc_col: *apc_index as i32,
                                            })
                                    })
                                    .collect_vec()
                            })
                            // sort by column so that reads to the same column are coalesced, as the table is column major
                            .sorted_by(|left, right| left.col.cmp(&right.col))
                            .collect();

                        // get the device dummy trace for this air
                        let dummy_trace = &dummy_trace_by_air_name[*air_name];

                        airs.push(OriginalAir {
                            width: dummy_trace.matrix.width() as i32,
                            height: dummy_trace.matrix.height() as i32,
                            buffer: dummy_trace.matrix.buffer().as_ptr(),
                            row_block_size: subs_by_row.len() as i32,
                            substitutions_offset: substitutions.len() as i32,
                            substitutions_length: filtered_substitutions.len() as i32,
                        });

                        substitutions.extend(filtered_substitutions);

                        (airs, substitutions)
                    },
                )
        };

        // Send the airs and substitutions to device
        let airs = airs.to_device().unwrap();
        let substitutions = substitutions.to_device().unwrap();

        cuda_abi::apc_tracegen(&mut output, airs, substitutions, num_apc_calls).unwrap();

        // Encode bus interactions for GPU consumption
        let (bus_interactions, arg_spans, bytecode) = compile_bus_to_gpu(&self.apc.machine.bus_interactions, &apc_poly_id_to_index, height);
        let bus_interactions = bus_interactions.to_device().unwrap();
        let arg_spans = arg_spans.to_device().unwrap();
        let bytecode = bytecode.to_device().unwrap();

        // Gather GPU inputs for periphery (bus ids, count device buffers)
        let periphery = &self.periphery.real;

        // Range checker
        let var_range_bus_id = periphery
            .range_checker
            .cpu_chip
            .as_ref()
            .unwrap()
            .bus()
            .index() as u32;
        let var_range_count = &periphery.range_checker.count;

        // Tuple checker
        let chip = periphery.tuple_range_checker.as_ref().unwrap();
        let tuple2_bus_id = DEFAULT_TUPLE_RANGE_CHECKER as u32;
        let tuple2_sizes = chip.sizes;
        let tuple2_count_u32 = chip.count.as_ref();

        // Bitwise lookup; NUM_BITS is 8 in our setup
        let chip = periphery.bitwise_lookup_8.as_ref().unwrap();
        let bitwise_bus_id = chip.cpu_chip.as_ref().unwrap().bus().inner.index as u32;
        let bitwise_count_u32 = chip.count.as_ref();
        let bitwise_num_bits = 8u32;

        // Launch GPU apply-bus to update periphery histograms on device
        cuda_abi::apc_apply_bus(
            &output,
            bus_interactions,
            arg_spans,
            bytecode,
            var_range_bus_id,
            tuple2_bus_id,
            bitwise_bus_id,
            var_range_count,
            tuple2_count_u32,
            tuple2_sizes,
            bitwise_count_u32,
            bitwise_num_bits,
            num_apc_calls,
        )
        .unwrap();

        output.into()
    }
}

fn emit_expr(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<BabyBear>,
    id_to_apc_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) {
    match expr {
        AlgebraicExpression::Number(c) => {
            bc.push(OpCode::PushConst as u32);
            bc.push(c.as_canonical_u32());
        }
        AlgebraicExpression::Reference(r) => {
            let idx = (id_to_apc_index[&r.id] * apc_height) as u32;
            bc.push(OpCode::PushApc as u32);
            bc.push(idx);
        }
        AlgebraicExpression::UnaryOperation(u) => {
            emit_expr(bc, &u.expr, id_to_apc_index, apc_height);
            match u.op {
                AlgebraicUnaryOperator::Minus => bc.push(OpCode::Neg as u32),
            }
        }
        AlgebraicExpression::BinaryOperation(b) => {
            emit_expr(bc, &b.left, id_to_apc_index, apc_height);
            emit_expr(bc, &b.right, id_to_apc_index, apc_height);
            match b.op {
                AlgebraicBinaryOperator::Add => bc.push(OpCode::Add as u32),
                AlgebraicBinaryOperator::Sub => bc.push(OpCode::Sub as u32),
                AlgebraicBinaryOperator::Mul => bc.push(OpCode::Mul as u32),
            }
        }
    }
}

pub fn compile_bus_to_gpu(
    bus: &[SymbolicBusInteraction<BabyBear>],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DevInteraction>, Vec<DevArgSpan>, Vec<u32>) {
    let mut interactions = Vec::with_capacity(bus.len());
    let mut arg_spans = Vec::new();
    let mut bytecode = Vec::new();

    for bi in bus {
        // mult
        let mult_off = bytecode.len() as u32;
        emit_expr(&mut bytecode, &bi.mult, apc_poly_id_to_index, apc_height);
        let mult_len = (bytecode.len() as u32) - mult_off;

        // args
        let args_index_off = arg_spans.len() as u32;
        for arg in &bi.args {
            let off = bytecode.len() as u32;
            emit_expr(&mut bytecode, arg, apc_poly_id_to_index, apc_height);
            let len = (bytecode.len() as u32) - off;
            arg_spans.push(DevArgSpan { off, len });
        }

        interactions.push(DevInteraction {
            id: (bi.id as u32),
            num_args: bi.args.len() as u32,
            mult_off,
            mult_len,
            args_index_off,
        });
    }

    (interactions, arg_spans, bytecode)
}


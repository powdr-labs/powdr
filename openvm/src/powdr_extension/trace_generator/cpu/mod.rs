use std::{collections::HashMap, sync::Arc};

use itertools::Itertools;
use openvm_circuit::{arch::MatrixRecordArena, utils::next_power_of_two_or_zero};
use openvm_circuit_primitives::Chip;
use openvm_stark_backend::{
    p3_field::{Field, PrimeCharacteristicRing, PrimeField32},
    p3_matrix::dense::{DenseMatrix, RowMajorMatrix},
    prover::{AirProvingContext, ProverBackend},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::trace_handler::TraceTrait;
use powdr_constraint_solver::constraint_system::ComputationMethod;

use crate::{
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    isa::IsaApc,
    isa::OpenVmISA,
    powdr_extension::{chip::PowdrChipCpu, executor::OriginalArenas},
};

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrTraceGenerator
mod periphery;

pub use inventory::DummyChipComplex;
pub use periphery::{
    PowdrPeripheryInstancesCpu, SharedPeripheryChipsCpu, SharedPeripheryChipsCpuProverExt,
};

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

impl<R, PB: ProverBackend<Matrix = RowMajorMatrix<BabyBear>>, ISA: OpenVmISA> Chip<R, PB>
    for PowdrChipCpu<ISA>
{
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let row_major = self
            .trace_generator
            .generate_witness(self.record_arena_by_air_name.take());

        AirProvingContext::simple(row_major, vec![])
    }
}

pub struct PowdrTraceGeneratorCpu<ISA: OpenVmISA> {
    pub apc: IsaApc<BabyBear, ISA>,
    pub original_airs: OriginalAirs<BabyBear, ISA>,
    pub config: OriginalVmConfig<ISA>,
    pub periphery: PowdrPeripheryInstancesCpu<ISA>,
}

impl<ISA: OpenVmISA> PowdrTraceGeneratorCpu<ISA> {
    pub fn new(
        apc: IsaApc<BabyBear, ISA>,
        original_airs: OriginalAirs<BabyBear, ISA>,
        config: OriginalVmConfig<ISA>,
        periphery: PowdrPeripheryInstancesCpu<ISA>,
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

        let chip_inventory = {
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
                        None => return None,
                    }
                };

                let row_major_trace = chip.generate_proving_ctx(record_arena).common_main;

                Some((air_name, SharedCpuTrace::from(Arc::new(row_major_trace))))
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

        // Convert BTreeMap to dense Vec for O(1) lookups in the hot loop.
        let width = apc_poly_id_to_index.len();
        let id_to_idx: Vec<usize> = {
            let max_id = apc_poly_id_to_index.keys().last().copied().unwrap_or(0) as usize;
            let mut vec = vec![0usize; max_id + 1];
            for (&id, &index) in &apc_poly_id_to_index {
                vec[id as usize] = index;
            }
            vec
        };

        // Compile bus interactions once before the hot loop
        let compiled_interactions = {
            use powdr_autoprecompiles::expression::CompiledBusInteraction;
            CompiledBusInteraction::compile_all(
                &self.apc.machine().bus_interactions,
                &id_to_idx,
                BabyBear::ZERO,
                BabyBear::ONE,
            )
        };

        // allocate for apc trace
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut values = <BabyBear as PrimeCharacteristicRing>::zero_vec(height * width);

        // go through the final table and fill in the values (parallelized)
        {
            use openvm_stark_backend::p3_maybe_rayon::prelude::*;

            // Extract references to avoid capturing `self` (ISA::Config is not Sync)
            let periphery_real = &self.periphery.real;
            let periphery_bus_ids = &self.periphery.bus_ids;

            values
                .par_chunks_mut(width)
                .zip(dummy_values.into_par_iter())
                .for_each(|(row_slice, dummy_values)| {
                    // Copy dummy rows to APC row
                    for (dummy_row, dummy_trace_index_to_apc_index) in dummy_values
                        .iter()
                        .map(|r| &r.data[r.start..r.start + r.length])
                        .zip_eq(&dummy_trace_index_to_apc_index_by_instruction)
                    {
                        for (dummy_trace_index, apc_index) in dummy_trace_index_to_apc_index {
                            row_slice[*apc_index] = dummy_row[*dummy_trace_index];
                        }
                    }

                    // Compute derived columns
                    for derived_column in columns_to_compute {
                        let col_index = id_to_idx[derived_column.variable.id as usize];
                        row_slice[col_index] = match &derived_column.computation_method {
                            ComputationMethod::Constant(c) => *c,
                            ComputationMethod::QuotientOrZero(e1, e2) => {
                                use powdr_number::ExpressionConvertible;

                                let divisor_val = e2.to_expression(&|n| *n, &|column_ref| {
                                    row_slice[id_to_idx[column_ref.id as usize]]
                                });
                                if divisor_val.is_zero() {
                                    BabyBear::ZERO
                                } else {
                                    divisor_val.inverse()
                                        * e1.to_expression(&|n| *n, &|column_ref| {
                                            row_slice[id_to_idx[column_ref.id as usize]]
                                        })
                                }
                            }
                        };
                    }

                    // Evaluate bus interactions using compiled expressions.
                    // Periphery chips use AtomicU32 counters — thread-safe.
                    for ci in &compiled_interactions {
                        let mult = ci.mult.eval(row_slice);
                        periphery_real.apply(
                            ci.id as u16,
                            mult.as_canonical_u32(),
                            ci.args.iter().map(|a| a.eval(row_slice).as_canonical_u32()),
                            periphery_bus_ids,
                        );
                    }
                });
        }

        RowMajorMatrix::new(values, width)
    }
}

use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

use itertools::Itertools;
use openvm_circuit::{arch::MatrixRecordArena, utils::next_power_of_two_or_zero};
use openvm_circuit_primitives::Chip;
use openvm_stark_backend::{
    p3_field::{Field, PrimeCharacteristicRing, PrimeField32},
    p3_matrix::dense::{DenseMatrix, RowMajorMatrix},
    prover::{AirProvingContext, ProverBackend},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{expression::AlgebraicExpression, trace_handler::TraceTrait};
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_number::ExpressionConvertible;

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
                        None => return None, // skip this iteration, because we only have record arena for chips that are used
                    }
                };

                let row_major_trace = chip.generate_proving_ctx(record_arena).common_main;

                Some((air_name, SharedCpuTrace::from(Arc::new(row_major_trace))))
            })
            .collect();

        let TraceData {
            dummy_values,
            dummy_trace_index_to_apc_index_by_instruction,
            removed_column_dummy_index_by_instruction,
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
        let mut values = <BabyBear as PrimeCharacteristicRing>::zero_vec(height * width);

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

                // Collect the values of columns that were substituted out of the APC but are still
                // referenced by a derived column's computation method (e.g. re-encoding's original
                // group columns). These have no slot in `row_slice`; their values come straight from
                // the original instruction trace. Empty in the native optimizer path.
                let removed_column_values: HashMap<u64, BabyBear> = dummy_values
                    .iter()
                    .map(|r| &r.data[r.start..r.start + r.length])
                    .zip_eq(&removed_column_dummy_index_by_instruction)
                    .flat_map(|(dummy_row, removed)| {
                        removed.iter().map(move |(dummy_trace_index, poly_id)| {
                            (*poly_id, dummy_row[*dummy_trace_index])
                        })
                    })
                    .collect();

                // Fill in the columns we have to compute from other columns
                // (these are either new columns or for example the "is_valid" column).
                for derived_column in columns_to_compute {
                    if derived_column.is_new {
                        let col_index = apc_poly_id_to_index[&derived_column.variable.id];
                        row_slice[col_index] = evaluate_computation_method(
                            &derived_column.computation_method,
                            row_slice,
                            &apc_poly_id_to_index,
                            &removed_column_values,
                        );
                    }
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

fn evaluate_computation_method(
    method: &ComputationMethod<BabyBear, AlgebraicExpression<BabyBear>>,
    row_slice: &[BabyBear],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    removed_column_values: &HashMap<u64, BabyBear>,
) -> BabyBear {
    let eval = |e: &AlgebraicExpression<BabyBear>| {
        e.to_expression(&|n| *n, &|column_ref| {
            // Surviving columns live in `row_slice`; columns substituted out of the APC but still
            // referenced by this derived column are resolved from the original instruction trace.
            match apc_poly_id_to_index.get(&column_ref.id) {
                Some(index) => row_slice[*index],
                None => *removed_column_values
                    .get(&column_ref.id)
                    .unwrap_or_else(|| {
                        panic!(
                        "derived column references poly id {} which is neither an APC column nor a \
                         removed column recoverable from the original trace",
                        column_ref.id
                    )
                    }),
            }
        })
    };
    match method {
        ComputationMethod::Constant(c) => *c,
        ComputationMethod::QuotientOrZero(e1, e2) => {
            let divisor_val = eval(e2);
            if divisor_val.is_zero() {
                BabyBear::ZERO
            } else {
                divisor_val.inverse() * eval(e1)
            }
        }
        ComputationMethod::IfEqZero(condition, then, else_) => {
            if eval(condition).is_zero() {
                evaluate_computation_method(
                    then,
                    row_slice,
                    apc_poly_id_to_index,
                    removed_column_values,
                )
            } else {
                evaluate_computation_method(
                    else_,
                    row_slice,
                    apc_poly_id_to_index,
                    removed_column_values,
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use powdr_autoprecompiles::expression::AlgebraicReference;
    use std::sync::Arc;

    fn col(id: u64) -> AlgebraicExpression<BabyBear> {
        AlgebraicExpression::Reference(AlgebraicReference {
            name: Arc::new(format!("c{id}")),
            id,
        })
    }

    fn num(v: u32) -> AlgebraicExpression<BabyBear> {
        AlgebraicExpression::Number(BabyBear::from_u32(v))
    }

    /// A derived-column method that simply copies the value of column `id`
    /// (`QuotientOrZero(col, 1)` evaluates to `col`).
    fn copy_of(id: u64) -> ComputationMethod<BabyBear, AlgebraicExpression<BabyBear>> {
        ComputationMethod::QuotientOrZero(col(id), num(1))
    }

    /// A surviving column present in `apc_poly_id_to_index` is read from `row_slice`
    /// (unchanged native behavior; `removed_column_values` is empty here).
    #[test]
    fn resolves_surviving_column_from_row_slice() {
        let apc_poly_id_to_index: BTreeMap<u64, usize> = [(5u64, 1usize)].into_iter().collect();
        let row_slice = [BabyBear::from_u32(11), BabyBear::from_u32(22)];
        let removed = HashMap::new();
        let got =
            evaluate_computation_method(&copy_of(5), &row_slice, &apc_poly_id_to_index, &removed);
        assert_eq!(got, BabyBear::from_u32(22));
    }

    /// The fix: a column substituted out of the APC (absent from `apc_poly_id_to_index`, e.g.
    /// re-encoding's original group columns 830/831) is resolved from the original instruction
    /// trace via `removed_column_values` instead of panicking on the missing index.
    /// Pre-fix, `apc_poly_id_to_index[&830]` was an unconditional `BTreeMap` index and panicked.
    #[test]
    fn resolves_removed_column_from_original_trace() {
        let apc_poly_id_to_index: BTreeMap<u64, usize> = [(5u64, 0usize)].into_iter().collect();
        let row_slice = [BabyBear::from_u32(99)];
        let removed: HashMap<u64, BabyBear> =
            [(830u64, BabyBear::from_u32(7))].into_iter().collect();
        // 830 is NOT in apc_poly_id_to_index; without the fix this indexes a missing key.
        assert!(!apc_poly_id_to_index.contains_key(&830));
        let got =
            evaluate_computation_method(&copy_of(830), &row_slice, &apc_poly_id_to_index, &removed);
        assert_eq!(got, BabyBear::from_u32(7));
    }

    /// A reference that is neither a surviving APC column nor a recoverable removed column fails
    /// with a legible message rather than a bare map-index panic.
    #[test]
    #[should_panic(expected = "neither an APC column nor a")]
    fn panics_legibly_on_truly_unknown_column() {
        let apc_poly_id_to_index: BTreeMap<u64, usize> = BTreeMap::new();
        let row_slice: [BabyBear; 0] = [];
        let removed = HashMap::new();
        let _ =
            evaluate_computation_method(&copy_of(830), &row_slice, &apc_poly_id_to_index, &removed);
    }
}

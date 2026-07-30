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
use powdr_autoprecompiles::trace_handler::{DummyCoord, ResolvedMethod, TraceTrait};
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_expression::AlgebraicExpression;
use powdr_number::ExpressionConvertible;

use crate::{
    extraction_utils::OriginalVmConfig,
    isa::IsaApc,
    isa::OpenVmISA,
    powdr_extension::{
        chip::PowdrChipCpu, executor::OriginalArenas, trace_generator::cache::CachedApc,
    },
};

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrTraceGenerator
mod periphery;

pub use inventory::DummyChipComplex;
pub use periphery::{
    PowdrPeripheryInstancesCpu, SharedPeripheryChipsCpu, SharedPeripheryChipsCpuProverExt,
};

/// A wrapper around a DenseMatrix to implement `TraceTrait` for cached dummy-trace slicing.
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
    apc: CachedApc<BabyBear, ISA>,
    pub config: OriginalVmConfig<ISA>,
    pub periphery: PowdrPeripheryInstancesCpu<ISA>,
}

impl<ISA: OpenVmISA> PowdrTraceGeneratorCpu<ISA> {
    pub(crate) fn new(
        apc: CachedApc<BabyBear, ISA>,
        config: OriginalVmConfig<ISA>,
        periphery: PowdrPeripheryInstancesCpu<ISA>,
    ) -> Self {
        Self {
            apc,
            config,
            periphery,
        }
    }

    pub fn apc(&self) -> &IsaApc<BabyBear, ISA> {
        &self.apc.apc
    }

    pub fn generate_witness(
        &self,
        original_arenas: OriginalArenas<MatrixRecordArena<BabyBear>>,
    ) -> DenseMatrix<BabyBear> {
        let width = self.apc.width();

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

        let dummy_values = self
            .apc
            .dummy_values(&dummy_trace_by_air_name, num_apc_calls);

        // allocate for apc trace
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

                // The per-instruction dummy trace rows for this apc call.
                let dummy_rows: Vec<&[BabyBear]> = dummy_values
                    .iter()
                    .map(|r| &r.data[r.start..r.start + r.length])
                    .collect();

                for (&dummy_row, instruction) in dummy_rows.iter().zip_eq(&self.apc.instructions) {
                    for (dummy_trace_index, apc_index) in &instruction.copy_pairs {
                        row_slice[*apc_index] = dummy_row[*dummy_trace_index];
                    }
                }

                // Fill the computed columns (e.g. `is_valid`), each pre-resolved to its APC row
                // index and a method reading its inputs from the dummy trace.
                for (target_index, method) in &self.apc.columns_to_compute {
                    row_slice[*target_index] = evaluate_computation_method(method, &dummy_rows);
                }

                let evaluator = MappingRowEvaluator::new(row_slice, &self.apc.apc_poly_id_to_index);

                // replay the side effects of this row on the main periphery
                self.apc.bus_interactions.iter().for_each(|interaction| {
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
    method: &ResolvedMethod<BabyBear>,
    dummy_rows: &[&[BabyBear]],
) -> BabyBear {
    // References were resolved to dummy-trace coordinates at build time, so read them directly.
    let eval = |e: &AlgebraicExpression<BabyBear, DummyCoord>| {
        e.to_expression(&|n| *n, &|coord: &DummyCoord| {
            dummy_rows[coord.instruction][coord.index]
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
                evaluate_computation_method(then, dummy_rows)
            } else {
                evaluate_computation_method(else_, dummy_rows)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn coord(instruction: usize, index: usize) -> AlgebraicExpression<BabyBear, DummyCoord> {
        AlgebraicExpression::Reference(DummyCoord { instruction, index })
    }

    fn num(v: u32) -> AlgebraicExpression<BabyBear, DummyCoord> {
        AlgebraicExpression::Number(BabyBear::from_u32(v))
    }

    /// A derived-column method that simply copies the value at a dummy-trace coordinate
    /// (`QuotientOrZero(coord, 1)` evaluates to that value).
    fn copy_of(
        instruction: usize,
        index: usize,
    ) -> ComputationMethod<BabyBear, AlgebraicExpression<BabyBear, DummyCoord>> {
        ComputationMethod::QuotientOrZero(coord(instruction, index), num(1))
    }

    /// A derived-column input is read from the dummy trace at its resolved `(instruction, index)`
    /// coordinate.
    #[test]
    fn resolves_column_from_dummy_trace() {
        // input lives at index 1 of instruction 0's dummy row.
        let instr0 = [BabyBear::from_u32(11), BabyBear::from_u32(22)];
        let dummy_rows: Vec<&[BabyBear]> = vec![&instr0];
        let got = evaluate_computation_method(&copy_of(0, 1), &dummy_rows);
        assert_eq!(got, BabyBear::from_u32(22));
    }

    /// A column substituted out of the APC (e.g. re-encoding's original group columns) has no APC
    /// index but is still recovered from the original instruction trace, spanning multiple
    /// instruction dummy rows.
    #[test]
    fn resolves_removed_column_from_dummy_trace() {
        // input lives at index 2 of instruction 1's dummy row.
        let instr0 = [BabyBear::from_u32(99)];
        let instr1 = [
            BabyBear::from_u32(0),
            BabyBear::from_u32(0),
            BabyBear::from_u32(7),
        ];
        let dummy_rows: Vec<&[BabyBear]> = vec![&instr0, &instr1];
        let got = evaluate_computation_method(&copy_of(1, 2), &dummy_rows);
        assert_eq!(got, BabyBear::from_u32(7));
    }
}

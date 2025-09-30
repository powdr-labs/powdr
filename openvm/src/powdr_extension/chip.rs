// Mostly taken from [this openvm extension](https://github.com/openvm-org/openvm/blob/1b76fd5a900a7d69850ee9173969f70ef79c4c76/extensions/rv32im/circuit/src/auipc/core.rs#L1)

use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    rc::Rc,
    sync::Arc,
};

use crate::{
    bus_map::DEFAULT_VARIABLE_RANGE_CHECKER,
    extraction_utils::{
        record_arena_dimension_by_air_name_per_apc_call, OriginalAirs, OriginalVmConfig,
    },
    powdr_extension::executor::{
        create_dummy_airs, create_dummy_chip_complex, OriginalArenas, PowdrPeripheryInstances,
        RecordArenaDimension,
    },
    BabyBearSC, ExtendedVmConfig, Instr,
};

use super::{executor::PowdrExecutor, opcode::PowdrOpcode, PowdrPrecompile};
use itertools::Itertools;
use openvm_circuit::{
    arch::{AirInventory, Arena, ChipInventory, MatrixRecordArena},
    utils::next_power_of_two_or_zero,
};
use openvm_instructions::LocalOpcode;
use openvm_stark_backend::{
    p3_air::{Air, BaseAir},
    p3_field::{Field, FieldAlgebra},
    p3_matrix::dense::{DenseMatrix, RowMajorMatrix},
    prover::{cpu::CpuBackend, hal::ProverBackend, types::AirProvingContext},
    ChipUsageGetter,
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
    expression::{
        AlgebraicEvaluator, AlgebraicReference, ConcreteBusInteraction, MappingRowEvaluator,
        RowEvaluator, WitnessEvaluator,
    },
    trace_handler::{generate_trace, ComputationMethod, Trace, TraceData},
    Apc, InstructionHandler,
};

pub struct PowdrChip {
    pub name: String,
    pub opcode: PowdrOpcode,
    pub air: Arc<PowdrAir<BabyBear>>,
    pub apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
    pub original_airs: OriginalAirs<BabyBear>,
    pub config: ExtendedVmConfig,
    pub periphery: PowdrPeripheryInstances,
    pub record_arena_by_air_name: Rc<RefCell<OriginalArenas>>,
}

impl PowdrChip {
    pub(crate) fn new(
        precompile: PowdrPrecompile<BabyBear>,
        original_airs: OriginalAirs<BabyBear>,
        base_config: OriginalVmConfig,
        periphery: PowdrPeripheryInstances,
        record_arena_by_air_name: Rc<RefCell<OriginalArenas>>,
    ) -> Self {
        let PowdrPrecompile {
            name, opcode, apc, ..
        } = precompile;
        let air = Arc::new(PowdrAir::new(apc.clone()));

        Self {
            name,
            opcode,
            original_airs,
            config: base_config.config().clone(),
            apc,
            periphery,
            air,
            record_arena_by_air_name,
        }
    }

    /// Generates the witness for the autoprecompile. The result will be a matrix of
    /// size `next_power_of_two(number_of_calls) * width`, where `width` is the number of
    /// nodes in the APC circuit.
    pub fn generate_witness<R>(&self, records: R) -> RowMajorMatrix<BabyBear> {
        let chip_inventory = {
            let airs: AirInventory<BabyBearSC> =
                create_dummy_airs(&self.config.sdk_vm_config, self.periphery.dummy.clone())
                    .expect("Failed to create dummy airs");

            create_dummy_chip_complex(
                &self.config.sdk_vm_config,
                airs,
                self.periphery.dummy.clone(),
            )
            .expect("Failed to create chip complex")
            .inventory
        };

        let mut original_arenas = self.record_arena_by_air_name.as_ref().borrow_mut();
        let num_apc_calls = *original_arenas.number_of_calls();
        let arenas = original_arenas.arenas();

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
        );

        // precompute the symbolic bus sends to the range checker for each original instruction
        let range_checker_sends_per_original_instruction = self
            .apc
            .instructions()
            .iter()
            .map(|instruction| {
                self.original_airs
                    .get_instruction_air_and_id(instruction)
                    .1
                    .bus_interactions
                    .iter()
                    .filter(|interaction| interaction.id == DEFAULT_VARIABLE_RANGE_CHECKER)
                    .collect_vec()
            })
            .collect_vec();

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
                for ((dummy_row, range_checker_sends), dummy_trace_index_to_apc_index) in
                    dummy_values
                        .iter()
                        .zip_eq(&range_checker_sends_per_original_instruction)
                        .zip_eq(&dummy_trace_index_to_apc_index_by_instruction)
                {
                    let evaluator = RowEvaluator::new(dummy_row);

                    range_checker_sends.iter().for_each(|interaction| {
                        let ConcreteBusInteraction { mult, .. } =
                            evaluator.eval_bus_interaction(interaction);
                        for _ in 0..mult.as_canonical_u32() {
                            // TODO: remove count is not implemented in openvm 1.4.0
                            // self.periphery.range_checker.remove_count(
                            //     args.next().unwrap().as_canonical_u32(),
                            //     args.next().unwrap().as_canonical_u32() as usize,
                            // );
                        }
                    });

                    for (dummy_trace_index, apc_index) in dummy_trace_index_to_apc_index {
                        row_slice[*apc_index] = dummy_row[*dummy_trace_index];
                    }
                }

                // Fill in the columns we have to compute from other columns
                // (these are either new columns or for example the "is_valid" column).
                for (col_index, computation_method) in &columns_to_compute {
                    row_slice[*col_index] = match computation_method {
                        ComputationMethod::Constant(c) => BabyBear::from_canonical_u64(*c),
                        ComputationMethod::InverseOfSum(columns_to_sum) => columns_to_sum
                            .iter()
                            .map(|col| row_slice[*col])
                            .reduce(|a, b| a + b)
                            .unwrap()
                            .inverse(),
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

impl<R, PB: ProverBackend<Matrix = Arc<DenseMatrix<BabyBear>>>> Chip<R, PB> for PowdrChip {
    fn generate_proving_ctx(&self, records: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let trace = self.generate_witness(records);

        AirProvingContext::simple(Arc::new(trace), vec![])
    }
}

pub struct PowdrAir<F> {
    /// The columns in arbitrary order
    columns: Vec<AlgebraicReference>,
    apc: Arc<Apc<F, Instr<F>>>,
}

// impl<F: PrimeField32> ColumnsAir<F> for PowdrAir<F> {
//     fn columns(&self) -> Option<Vec<String>> {
//         Some(self.columns.iter().map(|c| (*c.name).clone()).collect())
//     }
// }

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

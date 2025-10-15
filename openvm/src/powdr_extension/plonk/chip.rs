use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::bus_map::BusMap;
use crate::extraction_utils::{OriginalAirs, OriginalVmConfig};
use crate::plonk::air_to_plonkish::build_circuit;
use crate::plonk::{Gate, PlonkCircuit, Variable};
use crate::powdr_extension::executor::OriginalArenas;
use crate::powdr_extension::plonk::air::PlonkColumns;
use crate::powdr_extension::plonk::copy_constraint::generate_permutation_columns;
use crate::powdr_extension::trace_generator::{PowdrPeripheryInstances, PowdrTraceGenerator};
use crate::powdr_extension::PowdrPrecompile;
use crate::Instr;
use itertools::Itertools;
use openvm_circuit::utils::next_power_of_two_or_zero;
use openvm_stark_backend::{
    p3_field::{FieldAlgebra, PrimeField32},
    p3_matrix::{
        dense::{DenseMatrix, RowMajorMatrix},
        Matrix,
    },
    prover::{hal::ProverBackend, types::AirProvingContext},
    Chip,
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_autoprecompiles::Apc;

#[cfg(feature = "cuda")]
use crate::powdr_extension::trace_generator::device_matrix_from_values;
#[cfg(feature = "cuda")]
use crate::DeviceMatrix;

pub struct PlonkChip {
    name: String,
    apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
    bus_map: BusMap,
    trace_generator: PowdrTraceGenerator,
    record_arena_by_air_name: Rc<RefCell<OriginalArenas>>,
}

impl PlonkChip {
    pub(crate) fn new(
        precompile: PowdrPrecompile<BabyBear>,
        original_airs: OriginalAirs<BabyBear>,
        base_config: OriginalVmConfig,
        periphery: PowdrPeripheryInstances,
        bus_map: BusMap,
    ) -> Self {
        let PowdrPrecompile {
            name,
            apc,
            apc_record_arena,
            ..
        } = precompile;
        let trace_generator = PowdrTraceGenerator::new(
            apc.clone(),
            original_airs.clone(),
            base_config.clone(),
            periphery.clone(),
        );

        Self {
            name,
            bus_map,
            apc,
            trace_generator,
            record_arena_by_air_name: apc_record_arena,
        }
    }
}

#[cfg(not(feature = "cuda"))]
impl<R, PB: ProverBackend<Matrix = Arc<DenseMatrix<BabyBear>>>> Chip<R, PB> for PlonkChip {
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        let (values, width, _) = self.generate_plonk_values();
        let trace = RowMajorMatrix::new(values, width);

        AirProvingContext::simple(Arc::new(trace), vec![])
    }
}

#[cfg(feature = "cuda")]
impl<R, PB: ProverBackend<Matrix = DeviceMatrix<BabyBear>>> Chip<R, PB> for PlonkChip {
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        let (values, width, height) = self.generate_plonk_values();
        let trace = device_matrix_from_values(values, width, height);

        AirProvingContext::simple(trace, vec![])
    }
}

impl PlonkChip {
    fn generate_plonk_values(&self) -> (Vec<BabyBear>, usize, usize) {
        tracing::debug!("Generating air proof input for PlonkChip {}", self.name);

        let plonk_circuit = build_circuit(self.apc.machine(), &self.bus_map);
        let record_arena_by_air_name = self.record_arena_by_air_name.take();
        let number_of_calls = record_arena_by_air_name.number_of_calls();
        let width = self.apc.machine().main_columns().count();
        let height = next_power_of_two_or_zero(number_of_calls * plonk_circuit.len());
        tracing::debug!("   Number of calls: {number_of_calls}");
        tracing::debug!("   Plonk gates: {}", plonk_circuit.len());
        tracing::debug!("   Trace width: {width}");
        tracing::debug!("   Trace height: {height}");

        // Get witness in a calls x variables matrix.
        // TODO: Currently, the #rows of this matrix is padded to the next power of 2,
        // which is unnecessary.
        let column_index_by_poly_id = self
            .apc
            .machine()
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        // Generate APC witness values
        let (witness_values, _, _) = self
            .trace_generator
            .generate_witness_values(record_arena_by_air_name);
        // TODO: confirm that this is the same width as that returned by `generate_witness_values`
        let witness = RowMajorMatrix::new(witness_values, width);

        // TODO: This should be parallelized.
        let mut values = BabyBear::zero_vec(height * width);
        let num_tmp_vars = plonk_circuit.num_tmp_vars();
        for (call_index, witness) in witness.rows().take(number_of_calls).enumerate() {
            // Computing the trace values for the current call (starting at row call_index * circuit_length).
            let witness = witness.collect_vec();
            let mut vars = PlonkVariables::new(num_tmp_vars, &witness, &column_index_by_poly_id);
            for (gate_index, gate) in plonk_circuit.gates().iter().enumerate() {
                let index = call_index * plonk_circuit.len() + gate_index;
                let columns: &mut PlonkColumns<_> =
                    values[index * width..(index + 1) * width].borrow_mut();
                let gate = Gate {
                    a: gate.a.clone(),
                    b: gate.b.clone(),
                    c: gate.c.clone(),
                    d: gate.d.clone(),
                    e: gate.e.clone(),

                    q_bitwise: gate.q_bitwise,
                    q_memory: gate.q_memory,
                    q_execution: gate.q_execution,
                    q_pc: gate.q_pc,
                    q_range_tuple: gate.q_range_tuple,
                    q_range_check: gate.q_range_check,

                    q_l: gate.q_l,
                    q_r: gate.q_r,
                    q_o: gate.q_o,
                    q_mul: gate.q_mul,
                    q_const: gate.q_const,
                };

                // TODO: These should be pre-processed columns (for soundness and efficiency).
                columns.q_bitwise = gate.q_bitwise;
                columns.q_memory = gate.q_memory;
                columns.q_execution = gate.q_execution;
                columns.q_pc = gate.q_pc;
                columns.q_range_tuple = gate.q_range_tuple;
                columns.q_range_check = gate.q_range_check;

                columns.q_l = gate.q_l;
                columns.q_r = gate.q_r;
                columns.q_o = gate.q_o;
                columns.q_mul = gate.q_mul;
                columns.q_const = gate.q_const;

                // We currently assume that:
                // - We can always solve for temporary variables, by processing the gates in order.
                // - Temporary variables appear in `c` for the first time.
                // TODO: Solve for tmp variables of other columns too.
                vars.derive_tmp_values_for_c(&gate);
                vars.assert_all_known_or_unused(&gate);

                for (witness_in_gate, witness_col) in [
                    (&gate.a, &mut columns.a),
                    (&gate.b, &mut columns.b),
                    (&gate.c, &mut columns.c),
                    (&gate.d, &mut columns.d),
                    (&gate.e, &mut columns.e),
                ] {
                    if let Some(value) = vars.get(witness_in_gate) {
                        *witness_col = value;
                    }
                }
            }
        }

        generate_permutation_columns(&mut values, &plonk_circuit, number_of_calls, width);

        (values, width, height)
    }
}

/// Variables of the PlonK circuit.
struct PlonkVariables<'a, F> {
    /// Temporary variables, indexed by their ID.
    /// If None, the value is not known yet.
    tmp_vars: Vec<Option<F>>,
    /// The vector of witness values, indexed by the column index.
    witness: &'a [F],
    /// Maps a poly ID to its index in the witness vector.
    column_index_by_poly_id: &'a BTreeMap<u64, usize>,
}

impl<'a, F: PrimeField32> PlonkVariables<'a, F> {
    fn new(
        num_tmp_vars: usize,
        witness: &'a [F],
        column_index_by_poly_id: &'a BTreeMap<u64, usize>,
    ) -> Self {
        Self {
            tmp_vars: vec![None; num_tmp_vars],
            witness,
            column_index_by_poly_id,
        }
    }

    /// Get the value of a variable. None if the variable is temporary but still unknown.
    fn get(&self, variable: &Variable<AlgebraicReference>) -> Option<F> {
        match variable {
            Variable::Witness(id) => Some(self.witness[self.column_index_by_poly_id[&id.id]]),
            Variable::Tmp(id) => self.tmp_vars[*id],
            // The value of unused cells should not matter.
            Variable::Unused => Some(F::ZERO),
        }
    }

    /// If the given gate's `c` value is unknown and `a` and `b` are known,
    /// derives the value of `c`.
    fn derive_tmp_values_for_c(&mut self, gate: &Gate<F, AlgebraicReference>) {
        if self.get(&gate.c).is_some() {
            // Already know the value.
            return;
        }
        if let (Some(a), Some(b), Variable::Tmp(id)) =
            (self.get(&gate.a), self.get(&gate.b), &gate.c)
        {
            // The PlonK constraint is:
            // q_l * a + q_r * b + q_o * c + q_mul * a * b + q_const = 0
            // We can derive c as:
            let value =
                -(gate.q_l * a + gate.q_r * b + gate.q_mul * a * b + gate.q_const) / gate.q_o;
            self.tmp_vars[*id] = Some(value);
        }
    }

    /// Asserts that all variables `a`, `b`, and `c` in the given gate are known or unused.
    fn assert_all_known_or_unused(&self, gate: &Gate<F, AlgebraicReference>) {
        if let Variable::Tmp(id) = gate.a {
            assert!(self.tmp_vars[id].is_some(), "Variable `a` is unknown.",);
        }
        if let Variable::Tmp(id) = gate.b {
            assert!(self.tmp_vars[id].is_some(), "Variable `b` is unknown.",);
        }
        if let Variable::Tmp(id) = gate.c {
            assert!(self.tmp_vars[id].is_some(), "Variable `c` is unknown.",);
        }
    }
}

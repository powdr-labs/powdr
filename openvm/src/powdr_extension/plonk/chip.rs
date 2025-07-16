use std::borrow::BorrowMut;
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::extraction_utils::OriginalAirs;
use crate::plonk::air_to_plonkish::build_circuit;
use crate::plonk::{Gate, Variable};
use crate::powdr_extension::executor::{PowdrExecutor, PowdrPeripheryInstances};
use crate::powdr_extension::plonk::air::PlonkColumns;
use crate::powdr_extension::plonk::copy_constraint::generate_permutation_columns;
use crate::powdr_extension::PowdrOpcode;
use crate::powdr_extension::PowdrPrecompile;
use crate::BusMap;
use itertools::Itertools;
use openvm_circuit::utils::next_power_of_two_or_zero;
use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult},
    system::memory::{MemoryController, OfflineMemory},
};
use openvm_instructions::instruction::Instruction;
use openvm_instructions::LocalOpcode;
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_backend::p3_air::BaseAir;
use openvm_stark_backend::p3_field::FieldAlgebra;
use openvm_stark_backend::p3_matrix::dense::RowMajorMatrix;
use openvm_stark_backend::p3_matrix::Matrix;
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::PrimeField32,
    prover::types::AirProofInput,
    rap::AnyRap,
    Chip, ChipUsageGetter,
};
use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_autoprecompiles::SymbolicMachine;

use super::air::PlonkAir;

pub struct PlonkChip<F: PrimeField32> {
    name: String,
    opcode: PowdrOpcode,
    air: Arc<PlonkAir<F>>,
    executor: PowdrExecutor<F>,
    machine: SymbolicMachine<F>,
    bus_map: BusMap,
}

impl<F: PrimeField32> PlonkChip<F> {
    #[allow(dead_code)]
    pub(crate) fn new(
        precompile: PowdrPrecompile<F>,
        original_airs: OriginalAirs<F>,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        base_config: SdkVmConfig,
        periphery: PowdrPeripheryInstances,
        bus_map: BusMap,
        copy_constraint_bus_id: u16,
    ) -> Self {
        let PowdrPrecompile {
            original_instructions,
            is_valid_column,
            name,
            opcode,
            machine,
            ..
        } = precompile;
        let air = PlonkAir {
            copy_constraint_bus_id,
            bus_map: bus_map.clone(),
            _marker: std::marker::PhantomData,
        };
        let executor = PowdrExecutor::new(
            original_instructions,
            original_airs,
            is_valid_column,
            memory,
            base_config,
            periphery,
        );

        Self {
            name,
            opcode,
            air: Arc::new(air),
            executor,
            machine,
            bus_map,
        }
    }
}

impl<F: PrimeField32> InstructionExecutor<F> for PlonkChip<F> {
    fn execute(
        &mut self,
        memory: &mut MemoryController<F>,
        instruction: &Instruction<F>,
        from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        let &Instruction { opcode, .. } = instruction;
        assert_eq!(opcode.as_usize(), self.opcode.global_opcode().as_usize());

        let execution_state = self.executor.execute(memory, from_state)?;

        Ok(execution_state)
    }

    fn get_opcode_name(&self, _opcode: usize) -> String {
        self.name.clone()
    }
}

impl<F: PrimeField32> ChipUsageGetter for PlonkChip<F> {
    fn air_name(&self) -> String {
        format!("powdr_plonk_air_for_opcode_{}", self.opcode.global_opcode()).to_string()
    }
    fn current_trace_height(&self) -> usize {
        self.executor.number_of_calls()
    }

    fn trace_width(&self) -> usize {
        self.air.width()
    }
}

impl<SC: StarkGenericConfig> Chip<SC> for PlonkChip<Val<SC>>
where
    Val<SC>: PrimeField32,
{
    fn air(&self) -> Arc<dyn AnyRap<SC>> {
        self.air.clone()
    }

    fn generate_air_proof_input(self) -> AirProofInput<SC> {
        tracing::debug!("Generating air proof input for PlonkChip {}", self.name);

        let plonk_circuit = build_circuit(&self.machine, &self.bus_map);
        let number_of_calls = self.executor.number_of_calls();
        let width = self.trace_width();
        let height = next_power_of_two_or_zero(number_of_calls * plonk_circuit.len());
        tracing::debug!("   Number of calls: {number_of_calls}");
        tracing::debug!("   Plonk gates: {}", plonk_circuit.len());
        tracing::debug!("   Trace width: {width}");
        tracing::debug!("   Trace height: {height}");

        // Get witness in a calls x variables matrix.
        // TODO: Currently, the #rows of this matrix is padded to the next power of 2,
        // which is unnecessary.
        let column_index_by_poly_id = self
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();
        let witness = self
            .executor
            .generate_witness::<SC>(&column_index_by_poly_id, &self.machine.bus_interactions);

        // TODO: This should be parallelized.
        let mut values = <Val<SC>>::zero_vec(height * width);
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

        AirProofInput::simple(RowMajorMatrix::new(values, width), vec![])
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

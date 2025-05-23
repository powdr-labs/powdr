use std::borrow::BorrowMut;
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::plonk::{Gate, PlonkCircuit, Variable};
use crate::powdr_extension::executor::PowdrExecutor;
use crate::powdr_extension::plonk::air::PlonkColumns;
use crate::powdr_extension::PowdrOpcode;
use crate::powdr_extension::{
    chip::SharedChips, chip::SymbolicMachine as ChipSymbolicMachine, PowdrPrecompile,
};
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
use powdr_autoprecompiles::powdr::UniqueColumns;
use powdr_autoprecompiles::SymbolicMachine;

use super::air::PlonkAir;

pub struct PlonkChip<F: PrimeField32> {
    name: String,
    opcode: PowdrOpcode,
    air: Arc<PlonkAir<F>>,
    executor: PowdrExecutor<F>,
    machine: SymbolicMachine<F>,
}

impl<F: PrimeField32> PlonkChip<F> {
    #[allow(dead_code)]
    pub(crate) fn new(
        precompile: PowdrPrecompile<F>,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        base_config: SdkVmConfig,
        periphery: SharedChips,
    ) -> Self {
        let air = PlonkAir {
            _marker: std::marker::PhantomData,
        };
        let name = precompile.name.clone();
        let opcode = precompile.opcode.clone();
        let machine = precompile.machine.clone();
        let executor = PowdrExecutor::new(precompile, memory, base_config, periphery);

        Self {
            name,
            opcode,
            air: Arc::new(air),
            executor,
            machine,
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
        format!("powdr_air_for_opcode_{}", self.opcode.global_opcode()).to_string()
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
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        // TODO: Compute the actual PlonK circuit from the machine.
        let plonk_circuit = PlonkCircuit::<Val<SC>, u64>::default();
        let number_of_calls = self.executor.number_of_calls();
        let width = self.trace_width();
        let height = next_power_of_two_or_zero(number_of_calls * plonk_circuit.len());

        // Get witness in a calls x variables matrix.
        // TODO: Currently, the #rows of this matrix is padded to the next power of 2,
        // which is unnecessary.
        let column_index_by_poly_id = self
            .machine
            .unique_columns()
            .enumerate()
            .map(|(index, c)| (c.id.id, index))
            .collect();
        let machine: ChipSymbolicMachine<Val<SC>> = self.machine.into();
        let witness = self
            .executor
            .generate_witness::<SC>(&column_index_by_poly_id, &machine.bus_interactions);

        // TODO: This should be parallelized.
        let mut values = <Val<SC>>::zero_vec(height * width);
        let num_tmp_vars = plonk_circuit.num_tmp_vars();
        for (call_index, witness) in witness.rows().take(number_of_calls).enumerate() {
            let witness = witness.collect_vec();
            let mut tmp_vars = vec![None; num_tmp_vars];
            for (gate_index, gate) in plonk_circuit.gates().iter().enumerate() {
                let index = call_index * plonk_circuit.len() + gate_index;
                let PlonkColumns {
                    q_l,
                    q_r,
                    q_o,
                    q_mul,
                    q_const,
                    a,
                    b,
                    c,
                } = values[index * width..(index + 1) * width].borrow_mut();

                // TODO: These should be pre-processed columns (for soundness and efficiency).
                *q_l = gate.q_l;
                *q_r = gate.q_r;
                *q_o = gate.q_o;
                *q_mul = gate.q_mul;
                *q_const = gate.q_const;

                derive_tmp_values(gate, &witness, &column_index_by_poly_id, &mut tmp_vars);
                *a = get_variable_value(&gate.a, &witness, &column_index_by_poly_id, &tmp_vars);
                *b = get_variable_value(&gate.b, &witness, &column_index_by_poly_id, &tmp_vars);
                *c = get_variable_value(&gate.c, &witness, &column_index_by_poly_id, &tmp_vars);
            }
        }

        AirProofInput::simple(RowMajorMatrix::new(values, width), vec![])
    }
}

fn derive_tmp_values<F: PrimeField32>(
    gate: &Gate<F, u64>,
    witness: &[F],
    column_index_by_poly_id: &BTreeMap<u64, usize>,
    tmp_vars: &mut [Option<F>],
) {
    // TODO: Solve for tmp variables of other columns too.
    if let Variable::Tmp(id) = &gate.c {
        if tmp_vars[*id].is_some() {
            // Already know the value.
            return;
        }

        // Note that this panics if any unknown tmp variable is in a or b.
        let a = get_variable_value(&gate.a, witness, column_index_by_poly_id, tmp_vars);
        let b = get_variable_value(&gate.b, witness, column_index_by_poly_id, tmp_vars);
        // The PlonK constraint is:
        // q_l * a + q_r * b + q_o * c + q_mul * a * b + q_const = 0
        // We can derive c as:
        tmp_vars[*id] =
            Some(-(gate.q_l * a + gate.q_r * b + gate.q_mul * a * b + gate.q_const) / gate.q_o);
    }
}

fn get_variable_value<F: PrimeField32>(
    variable: &Variable<u64>,
    witness: &[F],
    column_index_by_poly_id: &BTreeMap<u64, usize>,
    tmp_vars: &[Option<F>],
) -> F {
    match variable {
        Variable::Witness(id) => witness[column_index_by_poly_id[id]],
        Variable::Tmp(id) => tmp_vars[*id].expect("Temporary unknown!"),
    }
}

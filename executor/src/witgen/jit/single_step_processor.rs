#![allow(dead_code)]

use itertools::Itertools;
use powdr_ast::analyzed::{AlgebraicReference, PolyID};
use powdr_number::FieldElement;

use crate::witgen::{machines::MachineParts, FixedData};

use super::{
    effect::Effect,
    processor::Processor,
    variable::{Cell, Variable},
    witgen_inference::{CanProcessCall, FixedEvaluator, WitgenInference},
};

/// A processor for generating JIT code that computes the next row from the previous row.
pub struct SingleStepProcessor<'a, T: FieldElement> {
    fixed_data: &'a FixedData<'a, T>,
    machine_parts: MachineParts<'a, T>,
}

impl<'a, T: FieldElement> SingleStepProcessor<'a, T> {
    pub fn new(fixed_data: &'a FixedData<'a, T>, machine_parts: MachineParts<'a, T>) -> Self {
        SingleStepProcessor {
            fixed_data,
            machine_parts,
        }
    }

    pub fn generate_code<CanProcess: CanProcessCall<T> + Clone>(
        &self,
        can_process: CanProcess,
    ) -> Result<Vec<Effect<T, Variable>>, String> {
        let all_witnesses = self
            .machine_parts
            .witnesses
            .iter()
            .cloned()
            .sorted()
            .collect_vec();
        // All witness columns in row 0 are known.
        let known_variables = all_witnesses.iter().map(|&id| self.cell(id, 0));
        // and we want to know the ones in the next row.
        let requested_known = all_witnesses.iter().map(|&id| self.cell(id, 1));
        let identities = self.machine_parts.identities.iter().map(|&id| {
            let row_offset = if id.contains_next_ref() { 0 } else { 1 };
            (id, row_offset)
        });
        let block_size = 1;
        let witgen = WitgenInference::new(self.fixed_data, NoEval, known_variables);

        Processor::new(
            self.fixed_data,
            NoEval,
            identities,
            block_size,
            requested_known,
        )
        .generate_code(can_process, witgen)
    }

    fn cell(&self, id: PolyID, row_offset: i32) -> Variable {
        Variable::Cell(Cell {
            column_name: self.fixed_data.column_name(&id).to_string(),
            id: id.id,
            row_offset,
        })
    }
}

#[derive(Clone)]
pub struct NoEval;

impl<T: FieldElement> FixedEvaluator<T> for NoEval {
    fn evaluate(&self, _var: &AlgebraicReference, _row_offset: i32) -> Option<T> {
        // We can only return something here if the fixed column is constant
        // in the region we are considering.
        // This might be the case if we know we are not evaluating the first or the last
        // row, but this is not yet implemented.
        None
    }
}

#[cfg(test)]
mod test {

    use pretty_assertions::assert_eq;
    use test_log::test;

    use powdr_number::GoldilocksField;

    use crate::witgen::{
        data_structures::mutable_state::MutableState,
        global_constraints,
        jit::effect::{format_code, Effect},
        machines::KnownMachine,
        FixedData,
    };
    use itertools::Itertools;

    use crate::witgen::{
        jit::test_util::read_pil,
        machines::{machine_extractor::MachineExtractor, Machine},
    };

    use super::{SingleStepProcessor, Variable};

    fn generate_single_step(
        input_pil: &str,
        machine_name: &str,
    ) -> Result<Vec<Effect<GoldilocksField, Variable>>, String> {
        let (analyzed, fixed_col_vals) = read_pil(input_pil);

        let fixed_data = FixedData::new(&analyzed, &fixed_col_vals, &[], Default::default(), 0);
        let (fixed_data, retained_identities) =
            global_constraints::set_global_constraints(fixed_data, &analyzed.identities);
        let machines = MachineExtractor::new(&fixed_data).split_out_machines(retained_identities);
        let [KnownMachine::DynamicMachine(machine)] = machines
            .iter()
            .filter(|m| m.name().contains(machine_name))
            .collect_vec()
            .as_slice()
        else {
            panic!("Expected exactly one matching dynamic machine")
        };
        let machine_parts = machine.machine_parts().clone();
        let mutable_state = MutableState::new(machines.into_iter(), &|_| {
            Err("Query not implemented".to_string())
        });
        SingleStepProcessor::new(&fixed_data, machine_parts).generate_code(&mutable_state)
    }

    #[test]
    fn fib() {
        let input = "namespace M(256); let X; let Y; X' = Y; Y' = X + Y;";
        let code = generate_single_step(input, "M").unwrap();
        assert_eq!(
            format_code(&code),
            "M::X[1] = M::Y[0];\nM::Y[1] = (M::X[0] + M::Y[0]);"
        );
    }

    #[test]
    fn no_progress() {
        let input = "namespace M(256); let X; let Y; X' = X;";
        let err = generate_single_step(input, "M").err().unwrap();
        assert_eq!(
            err.to_string(),
            "Unable to derive algorithm to compute required values and \
            unable to branch on a variable.\nThe following variables are still missing: M::Y[1]\n\
            No code generated so far."
        );
    }

    #[test]
    fn branching() {
        let input = "
    namespace VM(256);
        let A: col;
        let B: col;
        let instr_add: col;
        let instr_mul: col;
        let pc: col;

        col fixed LINE = [0, 1] + [2]*;
        col fixed INSTR_ADD = [0, 1] + [0]*;
        col fixed INSTR_MUL = [1, 0] + [1]*;

        pc' = pc + 1;
        [ pc, instr_add, instr_mul ] in [ LINE, INSTR_ADD, INSTR_MUL ];

        instr_add * (A' - (A + B)) + instr_mul * (A' - A * B) + (1 - instr_add - instr_mul) * (A' - A) = 0;
        B' = B;
        ";
        let code = generate_single_step(input, "Main").unwrap();
        assert_eq!(
            format_code(&code),
            "\
VM::pc[1] = (VM::pc[0] + 1);
machine_call(1, [Known(VM::pc[1]), Unknown(ret(1, 1, 1)), Unknown(ret(1, 1, 2))]);
VM::instr_add[1] = ret(1, 1, 1);
VM::instr_mul[1] = ret(1, 1, 2);
VM::B[1] = VM::B[0];
if (VM::instr_add[0] == 1) {
    if (VM::instr_mul[0] == 1) {
        VM::A[1] = -((-(VM::A[0] + VM::B[0]) + -(VM::A[0] * VM::B[0])) + VM::A[0]);
    } else {
        VM::A[1] = (VM::A[0] + VM::B[0]);
    }
} else {
    if (VM::instr_mul[0] == 1) {
        VM::A[1] = (VM::A[0] * VM::B[0]);
    } else {
        VM::A[1] = VM::A[0];
    }
}"
        );
    }
}
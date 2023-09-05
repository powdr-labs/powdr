//! For all machines, enforce that the `operation_id` can only change when the `latch` is on

use ast::asm_analysis::{AnalysisASMFile, PilBlock};
use number::FieldElement;

use crate::utils::parse_pil_statement;

pub fn enforce<T: FieldElement>(mut file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    for machine in file.machines.values_mut() {
        let operation_id = machine.operation_id.as_ref().unwrap();
        let latch = machine.latch.as_ref().unwrap();
        let last_step = "_block_enforcer_last_step";
        let operation_id_no_change = "_operation_id_no_change";

        // add the necessary embedded constraints which apply to both static and dynamic machines
        let embedded_constraints = [
            // inject last step
            parse_pil_statement(&format!("col constant {last_step} = [0]* + [1]")),
            // the operation id must be constant within a block.
            // TODO: use an intermediate polynomial, currently it yields an error later in `analyzed`
            parse_pil_statement(&format!("col witness {operation_id_no_change}")),
            parse_pil_statement(&format!(
                "{operation_id_no_change} = (1 - {last_step}) * (1 - {latch})"
            )),
            parse_pil_statement(&format!(
                "{operation_id_no_change} * ({operation_id}' - {operation_id}) = 0"
            )),
        ];

        machine.constraints.push(PilBlock {
            start: 0,
            statements: embedded_constraints.into_iter().collect(),
        });
    }
    file
}

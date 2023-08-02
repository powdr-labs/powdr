use ast::asm_analysis::{AnalysisASMFile, PilBlock};
use number::FieldElement;

use crate::utils::parse_pil_statement;

pub fn enforce<T: FieldElement>(mut file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    for machine in file.machines.values_mut() {
        let function_id = machine.function_id.as_ref().unwrap();
        let latch = machine.latch.as_ref().unwrap();

        // add the necessary embedded constraints which apply to both static and dynamic machines
        let embedded_constraints = [
            // inject the function_id
            parse_pil_statement(&format!("pol witness {function_id}")),
            // the function id must be constant within a block
            parse_pil_statement(&format!(
                "(1 - {latch}) * ({function_id}' - {function_id}) = 0"
            )),
        ];

        machine.constraints.push(PilBlock {
            start: 0,
            statements: embedded_constraints.into_iter().collect(),
        });
    }
    file
}

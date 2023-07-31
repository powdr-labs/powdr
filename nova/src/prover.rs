use ast::{analyzed::Analyzed, asm_analysis::Machine};
use number::FieldElement;

use crate::circuit_builder::nova_prove;

pub fn prove_ast_read_params<T: FieldElement>(
    pil: &Analyzed<T>,
    main_machine: &Machine<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<(&str, Vec<T>)>,
) -> () {
    prove_ast(pil, main_machine, fixed, witness)
}
pub fn prove_ast<T: FieldElement>(
    pil: &Analyzed<T>,
    main_machine: &Machine<T>,
    fixed: Vec<(&str, Vec<T>)>,
    witness: Vec<(&str, Vec<T>)>,
) -> () {
    log::info!("Starting proof generation...");
    let _ = nova_prove(pil, main_machine, fixed, witness);
}

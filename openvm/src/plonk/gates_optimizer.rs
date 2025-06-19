use super::{Gate, PlonkCircuit, Variable};
use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_number::FieldElement;

pub fn addtion_gates_optimizer<T>(plonk_circuit: &mut PlonkCircuit<T, AlgebraicReference>)
where
    T: FieldElement,
{
    let potential_reducable_gates: Vec<_> = plonk_circuit
        .gates
        .iter()
        .filter(|gate| gate.b == Variable::Unused && gate.q_const != T::ZERO)
        .collect();
    println!(
        "Potential reducable gates: {}",
        potential_reducable_gates.len()
    );
}

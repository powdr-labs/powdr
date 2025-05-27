use powdr_number::FieldElement;
use std::fmt::{self, Display};

pub mod air_to_plonkish;

/// A variable in a PlonK gate.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Variable<V> {
    /// A variable from the input constraint system.
    /// At run-time, we can get the concrete values from the APC witness generation.
    Witness(V),
    /// A temporary variable (represented by an ID). Assuming there is at most one temporary variable in a gate,
    /// we can solve for its value at run-time.
    Tmp(usize),
    /// An unused variable. This cell will be unconstrained; the prover can choose any value.
    Unused,
}

impl<V: Display> Display for Variable<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Witness(v) => write!(f, "{v}"),
            Variable::Tmp(id) => write!(f, "tmp_{id}"),
            Variable::Unused => write!(f, "Unused"),
        }
    }
}

/// A PlonK gate. For each gate, the following equation must hold:
/// q_l * a + q_r * b + q_o * c + q_mul * a * b + q_const = 0
/// where q_l, q_r, q_o, q_mul, and q_const are fixed coefficients
/// and a, b, c are variables.
/// If the same variable appears in multiple gates, a copy constraint
/// must be enforced.
#[derive(Clone, Debug)]
pub struct Gate<T, V>
where
    T: Display,
{
    q_l: T,
    q_r: T,
    q_o: T,
    q_mul: T,
    q_const: T,
    a: Variable<V>,
    b: Variable<V>,
    c: Variable<V>,
}

/// The PlonK circuit, which is just a collection of gates.
#[derive(Clone, Debug)]
pub struct PlonkCircuit<T, V>
where
    T: Display,
{
    pub gates: Vec<Gate<T, V>>,
}

impl<T, V> PlonkCircuit<T, V>
where
    T: FieldElement,
{
    fn new() -> Self {
        PlonkCircuit { gates: Vec::new() }
    }

    fn add_gate(&mut self, gate: Gate<T, V>) {
        self.gates.push(gate);
    }
}

impl<T: FieldElement, V: Display> Display for PlonkCircuit<T, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fmt_fe = |v: &T| {
            if v.is_in_lower_half() {
                format!("{v}")
            } else {
                format!("-{}", -*v)
            }
        };
        for gate in &self.gates {
            writeln!(
                f,
                "{} * {} + {} * {} + {} * {} + {} * {} * {} + {} = 0",
                fmt_fe(&gate.q_l),
                gate.a,
                fmt_fe(&gate.q_r),
                gate.b,
                fmt_fe(&gate.q_o),
                gate.c,
                fmt_fe(&gate.q_mul),
                gate.a,
                gate.b,
                fmt_fe(&gate.q_const),
            )?;
        }
        Ok(())
    }
}

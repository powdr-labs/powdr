use powdr_number::FieldElement;
use std::fmt::{self, Display};

pub mod air_to_plonkish;

/// A variable in a PlonK gate.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Variable<V> {
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
pub struct Gate<T, V> {
    pub q_l: T,
    pub q_r: T,
    pub q_o: T,
    pub q_mul: T,
    pub q_const: T,
    pub a: Variable<V>,
    pub b: Variable<V>,
    pub c: Variable<V>,
}

impl<T: FieldElement, V: Display> Display for Gate<T, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fmt_fe = |v: &T| {
            if v.is_in_lower_half() {
                format!("{v}")
            } else {
                format!("-{}", -*v)
            }
        };
        writeln!(
            f,
            "{} * {} + {} * {} + {} * {} + {} * {} * {} + {} = 0",
            fmt_fe(&self.q_l),
            self.a,
            fmt_fe(&self.q_r),
            self.b,
            fmt_fe(&self.q_o),
            self.c,
            fmt_fe(&self.q_mul),
            self.a,
            self.b,
            fmt_fe(&self.q_const),
        )?;
        Ok(())
    }
}

/// The PlonK circuit, which is just a collection of gates.
#[derive(Clone, Debug, Default)]
pub struct PlonkCircuit<T, V> {
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
        for gate in &self.gates {
            writeln!(f, "{gate}",)?;
        }
        Ok(())
    }
}

impl<T, V> PlonkCircuit<T, V> {
    pub fn len(&self) -> usize {
        self.gates.len()
    }

    pub fn gates(&self) -> &[Gate<T, V>] {
        &self.gates
    }

    pub fn num_tmp_vars(&self) -> usize {
        self.gates
            .iter()
            .flat_map(|gate| {
                [&gate.a, &gate.b, &gate.c]
                    .iter()
                    .filter_map(|var| match var {
                        Variable::Tmp(id) => Some(*id),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            })
            .max()
            .map(|max_id| max_id + 1)
            .unwrap_or_default()
    }
}

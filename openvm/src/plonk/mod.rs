use powdr_ast::analyzed::AlgebraicReference;
use powdr_number::FieldElement;
use std::fmt::{self, Display};

use crate::BusType;

pub mod air_to_plonkish;
pub mod bus_interaction_handler;

/// A variable in a PlonK gate.
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub enum Variable<V> {
    /// A variable from the input constraint system.
    /// At run-time, we can get the concrete values from the APC witness generation.
    Witness(V),
    /// A temporary variable (represented by an ID). Assuming there is at most one temporary variable in a gate,
    /// we can solve for its value at run-time.
    Tmp(usize),
    /// An unused variable. This cell will be unconstrained; the prover can choose any value.
    #[default]
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

    // The selectors for bus interactions.
    pub q_bitwise: T,
    pub q_memory: T,
    pub q_range_check: T,
    pub q_execution: T,
    pub q_pc: T,
    pub q_rang_tuple: T,

    pub a: Variable<V>,
    pub b: Variable<V>,
    pub c: Variable<V>,
}

impl<T: FieldElement> Default for Gate<T, AlgebraicReference> {
    fn default() -> Self {
        Gate {
            q_l: T::ZERO,
            q_r: T::ZERO,
            q_o: T::ZERO,
            q_mul: T::ZERO,
            q_const: T::ZERO,
            q_bitwise: T::ZERO,
            q_memory: T::ZERO,
            q_range_check: T::ZERO,
            q_execution: T::ZERO,
            q_pc: T::ZERO,
            q_rang_tuple: T::ZERO,

            a: Variable::Unused,
            b: Variable::Unused,
            c: Variable::Unused,
        }
    }
}

impl<T: FieldElement, V> Gate<T, V> {
    pub fn get_bus_gate_type(&self) -> Option<BusType> {
        let selectors = [
            (BusType::BitwiseLookup, &self.q_bitwise),
            (BusType::Memory, &self.q_memory),
            (BusType::VariableRangeChecker, &self.q_range_check),
            (BusType::ExecutionBridge, &self.q_execution),
            (BusType::PcLookup, &self.q_pc),
            (BusType::TupleRangeChecker, &self.q_rang_tuple),
        ];

        let active: Vec<_> = selectors
            .iter()
            .filter(|(_, val)| *val == &T::ONE)
            .collect();

        // Assert that exactly one is active
        assert!(
            active.len() <= 1,
            "Active more than one bus gate selector {:?}",
            active.iter().map(|(name, _)| *name).collect::<Vec<_>>()
        );
        if active.is_empty() {
            None
        } else {
            Some(active[0].0)
        }
    }
}

fn format_bus_type<T, V>(gate: &Gate<T, V>) -> &'static str
where
    T: FieldElement,
    V: Display,
{
    match gate.get_bus_gate_type() {
        Some(BusType::BitwiseLookup) => "bitwise",
        Some(BusType::Memory) => "memory",
        Some(BusType::VariableRangeChecker) => "range_check",
        Some(BusType::ExecutionBridge) => "execution",
        Some(BusType::PcLookup) => "pc",
        Some(BusType::TupleRangeChecker) => "tuple_range",
        Some(BusType::Sha) => "sha",
        None => "none",
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
                "bus: {}, {} * {} + {} * {} + {} * {} + {} * {} * {} + {} = 0",
                format_bus_type(gate),
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

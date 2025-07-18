use itertools::Itertools;
use openvm_stark_backend::p3_field::PrimeField32;
use std::fmt::{self, Display};

use powdr_autoprecompiles::bus_map::BusType;

use crate::format_fe;

pub mod air_to_plonkish;
pub mod bus_interaction_handler;

pub const NUMBER_OF_WITNESS_COLS: u64 = 5;

/// A variable in a PlonK gate.
#[derive(Clone, Copy, Debug, PartialEq, Default, Eq, Hash)]
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
pub struct Gate<F, V> {
    pub q_l: F,
    pub q_r: F,
    pub q_o: F,
    pub q_mul: F,
    pub q_const: F,

    // The selectors for bus interactions.
    pub q_bitwise: F,
    pub q_memory: F,
    pub q_range_check: F,
    pub q_execution: F,
    pub q_pc: F,
    pub q_range_tuple: F,

    pub a: Variable<V>,
    pub b: Variable<V>,
    pub c: Variable<V>,
    pub d: Variable<V>,
    pub e: Variable<V>,
}

impl<F: PrimeField32, V> Default for Gate<F, V> {
    fn default() -> Self {
        Gate {
            q_l: F::ZERO,
            q_r: F::ZERO,
            q_o: F::ZERO,
            q_mul: F::ZERO,
            q_const: F::ZERO,
            q_bitwise: F::ZERO,
            q_memory: F::ZERO,
            q_range_check: F::ZERO,
            q_execution: F::ZERO,
            q_pc: F::ZERO,
            q_range_tuple: F::ZERO,

            a: Variable::Unused,
            b: Variable::Unused,
            c: Variable::Unused,
            d: Variable::Unused,
            e: Variable::Unused,
        }
    }
}

impl<F: PrimeField32, V> Gate<F, V> {
    pub fn get_bus_gate_type(&self) -> Option<BusType> {
        let selectors = [
            (BusType::BitwiseLookup, &self.q_bitwise),
            (BusType::Memory, &self.q_memory),
            (BusType::VariableRangeChecker, &self.q_range_check),
            (BusType::ExecutionBridge, &self.q_execution),
            (BusType::PcLookup, &self.q_pc),
            (BusType::TupleRangeChecker, &self.q_range_tuple),
        ];

        let mut active_selector = None;
        for (name, val) in selectors.iter() {
            if *val == &F::ONE {
                if active_selector.is_some() {
                    panic!(
                        "Active more than one bus gate selector: {:?}",
                        selectors
                            .iter()
                            .filter(|(_, val)| *val == &F::ONE)
                            .map(|(name, _)| *name)
                            .collect::<Vec<_>>()
                    );
                }
                active_selector = Some(name);
            }
        }
        active_selector.copied()
    }
}

fn format_bus_type<F, V>(gate: &Gate<F, V>) -> &'static str
where
    F: PrimeField32,
    V: Display,
{
    match gate.get_bus_gate_type() {
        Some(BusType::BitwiseLookup) => "bitwise",
        Some(BusType::Memory) => "memory",
        Some(BusType::VariableRangeChecker) => "range_check",
        Some(BusType::ExecutionBridge) => "execution",
        Some(BusType::PcLookup) => "pc",
        Some(BusType::TupleRangeChecker) => "tuple_range",
        None => "none",
    }
}

impl<F: PrimeField32, V: Display> Display for Gate<F, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut lhs = [
            format_product(self.q_l, &self.a),
            format_product(self.q_r, &self.b),
            format_product(self.q_mul, format!("{a} * {b}", a = self.a, b = self.b)),
            if self.q_const.is_zero() {
                None
            } else {
                Some(format_fe(self.q_const))
            },
        ]
        .into_iter()
        .flatten()
        .peekable();

        let lhs = if lhs.peek().is_none() {
            "0".to_string()
        } else {
            lhs.join(" + ")
        };
        let rhs = format_product(-self.q_o, &self.c).unwrap_or_else(|| "0".to_string());
        let gate_info = if rhs == "0" && lhs == "0" {
            format!("{}, {}, {}, {}, {}", self.a, self.b, self.c, self.d, self.e)
        } else {
            format!("{lhs} = {rhs}")
        };

        write!(f, "bus: {}, {}", format_bus_type(self), gate_info)
    }
}

/// Pretty-prints a product <scalar> * <factor>, returning `None` if the scalar is zero.
fn format_product<F: PrimeField32>(scalar: F, factor: impl Display) -> Option<String> {
    if scalar.is_zero() {
        None
    } else if scalar.is_one() {
        Some(factor.to_string())
    } else if scalar == -F::ONE {
        Some(format!("-{factor}"))
    } else {
        Some(format!("{} * {factor}", format_fe(scalar)))
    }
}
/// The PlonK circuit, which is just a collection of gates.
#[derive(Clone, Debug, Default)]
pub struct PlonkCircuit<F, V> {
    pub gates: Vec<Gate<F, V>>,
}

impl<F, V> PlonkCircuit<F, V> {
    fn new() -> Self {
        PlonkCircuit { gates: Vec::new() }
    }

    fn add_gate(&mut self, gate: Gate<F, V>) {
        self.gates.push(gate);
    }
}

impl<F: PrimeField32, V: Display> Display for PlonkCircuit<F, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for gate in &self.gates {
            writeln!(f, "{gate}",)?;
        }
        Ok(())
    }
}

impl<F, V> PlonkCircuit<F, V> {
    pub fn len(&self) -> usize {
        self.gates.len()
    }

    pub fn gates(&self) -> &[Gate<F, V>] {
        &self.gates
    }

    pub fn num_tmp_vars(&self) -> usize {
        self.gates
            .iter()
            .flat_map(|gate| {
                [&gate.a, &gate.b, &gate.c, &gate.d, &gate.e]
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

#[cfg(test)]
pub mod test_utils {
    use std::sync::Arc;

    use openvm_stark_backend::p3_field::FieldAlgebra;
    use openvm_stark_sdk::p3_baby_bear::BabyBear;
    use powdr_autoprecompiles::expression::{AlgebraicExpression, AlgebraicReference};

    pub fn var(name: &str, id: u64) -> AlgebraicExpression<BabyBear> {
        AlgebraicExpression::Reference(AlgebraicReference {
            name: Arc::new(name.into()),
            id,
        })
    }

    pub fn c(value: u64) -> AlgebraicExpression<BabyBear> {
        AlgebraicExpression::Number(BabyBear::from_canonical_u64(value))
    }
}

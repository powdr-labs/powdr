use itertools::Itertools;
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
    pub q_range_tuple: T,

    pub a: Variable<V>,
    pub b: Variable<V>,
    pub c: Variable<V>,
}

impl<T: FieldElement, V> Default for Gate<T, V> {
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
            q_range_tuple: T::ZERO,

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
            (BusType::TupleRangeChecker, &self.q_range_tuple),
        ];

        let mut active_selector = None;
        for (name, val) in selectors.iter() {
            if *val == &T::ONE {
                if active_selector.is_some() {
                    panic!(
                        "Active more than one bus gate selector: {:?}",
                        selectors
                            .iter()
                            .filter(|(_, val)| *val == &T::ONE)
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

impl<T: FieldElement, V: Display> Display for Gate<T, V> {
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
            format!("{}, {}, {}", self.a, self.b, self.c)
        } else {
            format!("{lhs} = {rhs}")
        };

        write!(f, "bus: {}, {}", format_bus_type(self), gate_info)
    }
}

fn format_fe<T: FieldElement>(v: T) -> String {
    if v.is_in_lower_half() {
        format!("{v}")
    } else {
        format!("-{}", -v)
    }
}

/// Pretty-prints a product <scalar> * <factor>, returning `None` if the scalar is zero.
fn format_product<T: FieldElement>(scalar: T, factor: impl Display) -> Option<String> {
    if scalar.is_zero() {
        None
    } else if scalar.is_one() {
        Some(factor.to_string())
    } else if scalar == -T::ONE {
        Some(format!("-{factor}"))
    } else {
        Some(format!("{} * {factor}", format_fe(scalar)))
    }
}
/// The PlonK circuit, which is just a collection of gates.
#[derive(Clone, Debug, Default)]
pub struct PlonkCircuit<T, V> {
    pub gates: Vec<Gate<T, V>>,
}

impl<T, V> PlonkCircuit<T, V> {
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

#[cfg(test)]
pub mod test_utils {
    use powdr_autoprecompiles::legacy_expression::{
        AlgebraicExpression, AlgebraicReference, PolyID, PolynomialType,
    };
    use powdr_number::BabyBearField;
    pub fn var(name: &str, id: u64) -> AlgebraicExpression<BabyBearField> {
        AlgebraicExpression::Reference(AlgebraicReference {
            name: name.into(),
            poly_id: PolyID {
                id,
                ptype: PolynomialType::Committed,
            },
            next: false,
        })
    }

    pub fn c(value: u64) -> AlgebraicExpression<BabyBearField> {
        AlgebraicExpression::Number(BabyBearField::from(value))
    }
}

use std::collections::BTreeMap;

use super::{Gate, PlonkCircuit, Variable};
use crate::plonk::bus_interaction_handler::add_bus_to_plonk_circuit;
use crate::BusMap;
use powdr_autoprecompiles::expression::{AlgebraicExpression, AlgebraicReference};
use powdr_autoprecompiles::SymbolicMachine;
use powdr_expression::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicUnaryOperation,
    AlgebraicUnaryOperator,
};
use powdr_number::FieldElement;

pub fn build_circuit<T>(
    machine: &SymbolicMachine<T>,
    bus_map: &BusMap,
) -> PlonkCircuit<T, AlgebraicReference>
where
    T: FieldElement,
{
    let mut circuit_builder = CircuitBuilder::<T>::new();
    let mut last_gate_id = circuit_builder.plonk_circuit.gates.len();
    for constraint in &machine.constraints {
        println!("Processing constraint: {}", constraint.expr);
        circuit_builder.evaluate_expression(&constraint.expr, true);

        let new_gates: &[Gate<_, _>] = &circuit_builder.plonk_circuit.gates[last_gate_id..];

        for gate in new_gates {
            println!("{}", gate);
        }

        last_gate_id = circuit_builder.plonk_circuit.gates.len();

        println!(
            "gates length til now  {}",
            circuit_builder.plonk_circuit.gates.len()
        );

        // println!("newly added gates til now: {}", circuit_builder.plonk_circuit.gates[circuit_builder.plonk_circuit.gates.len() - 1..]);
    }

    for bus_interaction in &machine.bus_interactions {
        add_bus_to_plonk_circuit(bus_interaction.clone(), &mut circuit_builder, bus_map);
    }

    circuit_builder.build()
}

pub struct CircuitBuilder<T> {
    plonk_circuit: PlonkCircuit<T, AlgebraicReference>,
    temp_id_offset: usize,
    cache: BTreeMap<AlgebraicExpression<T>, Variable<AlgebraicReference>>,
}

impl<T> CircuitBuilder<T>
where
    T: FieldElement,
{
    pub fn new() -> Self {
        Self {
            plonk_circuit: PlonkCircuit::new(),
            temp_id_offset: 0,
            cache: BTreeMap::new(),
        }
    }

    /// Returns (q_o, c), where:
    /// - If `assert_zero` is true, `q_o` is always zero and `c` is unused.
    /// - If `assert_zero` is false, `q_o` is -1 and `c` is a new temporary variable.
    fn make_output(&mut self, assert_zero: bool) -> (T, Variable<AlgebraicReference>) {
        if assert_zero {
            (T::ZERO, Variable::Unused)
        } else {
            let c = Variable::Tmp(self.temp_id_offset);
            self.temp_id_offset += 1;
            (-T::ONE, c)
        }
    }

    /// Adds a gate to the PlonK circuit.
    pub fn add_gate(&mut self, gate: Gate<T, AlgebraicReference>) {
        self.plonk_circuit.add_gate(gate);
    }

    /// Adds gates to the PlonK circuit to evaluate a given expression.
    /// If the expression has been computed before, it retrieves the result from the cache.
    /// If `assert_zero` is true, it adds a constraint that the result must be zero.
    /// Returns a variable representing the result of the expression IF `assert_zero` is false
    /// (otherwise, it returns `Variable::Unused`).
    pub fn evaluate_expression(
        &mut self,
        algebraic_expr: &AlgebraicExpression<T>,
        assert_zero: bool,
    ) -> Variable<AlgebraicReference> {
        if let Some(var) = self.cache.get(algebraic_expr) {
            return var.clone();
        }

        let result = match algebraic_expr {
            AlgebraicExpression::Reference(r) => {
                if assert_zero {
                    // Constraint of the form `w = 0`
                    self.plonk_circuit.add_gate(Gate {
                        q_l: T::ONE,

                        a: Variable::Witness(r.clone()),
                        ..Default::default()
                    });
                    Variable::Unused
                } else {
                    Variable::Witness(r.clone())
                }
            }
            // Ideally, we would never hit this case, because allocating a new gate just to
            // "compute" a constant is wasteful.
            // The implementations of the binary operations below already handle one of their
            // operands being a number directly. The only way to reach this path is by passing
            // a number to `build_circuit` directly, or by having a `-<number>` expression.
            AlgebraicExpression::Number(value) => {
                let (q_o, c) = self.make_output(assert_zero);
                self.plonk_circuit.add_gate(Gate {
                    q_o,
                    q_const: *value,
                    c: c.clone(),
                    ..Default::default()
                });
                c
            }
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let mut q_l = T::ZERO;
                let mut q_r = T::ZERO;
                let mut q_mul = T::ZERO;
                let mut q_const = T::ZERO;
                let mut a = Variable::Unused;
                let mut b = Variable::Unused;
                let (q_o, c) = self.make_output(assert_zero);
                match op {
                    AlgebraicBinaryOperator::Add => {
                        if let (
                            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                                left: inner_left_left,
                                op: AlgebraicBinaryOperator::Mul,
                                right: inner_left_right,
                            }),
                            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation {
                                left: inner_right_left,
                                op: AlgebraicBinaryOperator::Mul,
                                right: inner_right_right,
                            }),
                        ) = (left.as_ref(), right.as_ref())
                        {
                            if let (
                                AlgebraicExpression::Number(n_left),
                                AlgebraicExpression::Reference(poly_left),
                                AlgebraicExpression::Number(n_right),
                                AlgebraicExpression::Reference(poly_right),
                            ) = (
                                inner_left_left.as_ref(),
                                inner_left_right.as_ref(),
                                inner_right_left.as_ref(),
                                inner_right_right.as_ref(),
                            ) {
                                q_l = *n_left;
                                a = Variable::Witness(poly_left.clone());
                                q_r = *n_right;
                                b = Variable::Witness(poly_right.clone());
                            } else {
                                if let AlgebraicExpression::Number(n) = left.as_ref() {
                                    q_const += *n;
                                } else {
                                    q_l = T::ONE;
                                    a = self.evaluate_expression(left, false);
                                }

                                if let AlgebraicExpression::Number(n) = right.as_ref() {
                                    q_const += *n;
                                } else {
                                    q_r = T::ONE;
                                    b = self.evaluate_expression(right, false);
                                }
                            }
                        } else {
                            if let AlgebraicExpression::Number(n) = left.as_ref() {
                                q_const += *n;
                            } else {
                                q_l = T::ONE;
                                a = self.evaluate_expression(left, false);
                            }

                            if let AlgebraicExpression::Number(n) = right.as_ref() {
                                q_const += *n;
                            } else {
                                q_r = T::ONE;
                                b = self.evaluate_expression(right, false);
                            }
                        }
                    }
                    AlgebraicBinaryOperator::Sub => {
                        if let AlgebraicExpression::Number(n) = left.as_ref() {
                            q_const += *n;
                        } else {
                            q_l = T::ONE;
                            a = self.evaluate_expression(left, false);
                        }

                        if let AlgebraicExpression::Number(n) = right.as_ref() {
                            q_const -= *n;
                        } else {
                            q_r = -T::ONE;
                            b = self.evaluate_expression(right, false);
                        }
                    }
                    AlgebraicBinaryOperator::Mul => match (left.as_ref(), right.as_ref()) {
                        (AlgebraicExpression::Number(n), AlgebraicExpression::Number(m)) => {
                            q_const += *n * *m;
                        }
                        (AlgebraicExpression::Number(n), non_constant)
                        | (non_constant, AlgebraicExpression::Number(n)) => {
                            q_l = *n;
                            a = self.evaluate_expression(non_constant, false);
                        }
                        _ => {
                            q_mul = T::ONE;
                            a = self.evaluate_expression(left, false);
                            b = self.evaluate_expression(right, false);
                        }
                    },
                };
                self.plonk_circuit.add_gate(Gate {
                    q_l,
                    q_r,
                    q_o,
                    q_mul,
                    q_const,

                    a,
                    b,
                    c: c.clone(),
                    ..Default::default()
                });
                c
            }
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
                AlgebraicUnaryOperator::Minus => {
                    let (q_o, c) = self.make_output(assert_zero);
                    let a = self.evaluate_expression(expr, false);
                    self.plonk_circuit.add_gate(Gate {
                        q_l: -T::ONE,
                        q_o,
                        a,
                        c: c.clone(),
                        ..Default::default()
                    });
                    c
                }
            },
        };

        self.cache.insert(algebraic_expr.clone(), result.clone());
        result
    }

    pub fn build(self) -> PlonkCircuit<T, AlgebraicReference> {
        self.plonk_circuit
    }
}

#[cfg(test)]
mod tests {
    use crate::bus_map::default_openvm_bus_map;
    use crate::plonk::air_to_plonkish::build_circuit;
    use crate::plonk::test_utils::{c, var};
    use powdr_autoprecompiles::{SymbolicConstraint, SymbolicMachine};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_air_to_plonkish() {
        let x = var("x", 0);
        let y = var("y", 1);
        let bus_map = default_openvm_bus_map();

        let expr = -(x.clone() * y.clone() - (-x.clone() * (x.clone() + y.clone())));
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine, &bus_map)),
            "bus: none, x * y = tmp_1
bus: none, -x = tmp_3
bus: none, x + y = tmp_4
bus: none, tmp_3 * tmp_4 = tmp_2
bus: none, tmp_1 + -tmp_2 = tmp_0
bus: none, -tmp_0 = 0
"
        );
    }

    #[test]
    fn only_constants() {
        let expr = c(4) + c(2) * (c(3) - c(5));
        let bus_map = default_openvm_bus_map();
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine, &bus_map)),
            "bus: none, -2 = tmp_1
bus: none, 2 * tmp_1 = tmp_0
bus: none, tmp_0 + 4 = 0
"
        )
    }

    #[test]
    fn single_variable() {
        let expr = var("x", 0);
        let bus_map = default_openvm_bus_map();
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine, &bus_map)),
            "bus: none, x = 0
"
        )
    }

    #[test]
    fn constant_and_variables() {
        let x = var("x", 0);
        let y = var("y", 1);
        let bus_map = default_openvm_bus_map();
        let expr = -(c(3) - c(2) * x.clone() * y.clone()) + c(1);
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine, &bus_map)),
            "bus: none, 2 * x = tmp_3
bus: none, tmp_3 * y = tmp_2
bus: none, -tmp_2 + 3 = tmp_1
bus: none, -tmp_1 = tmp_0
bus: none, tmp_0 + 1 = 0
"
        );
    }

    #[test]
    fn negative_number() {
        let expr = -c(3);
        let bus_map = default_openvm_bus_map();
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine, &bus_map)),
            "bus: none, 3 = tmp_0
bus: none, -tmp_0 = 0
"
        );
    }

    #[test]
    fn negative_var_sub() {
        let x = var("x", 0);
        let y = var("y", 1);
        let expr = x.clone() - (-y.clone());
        let bus_map = default_openvm_bus_map();
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine, &bus_map)),
            "bus: none, -y = tmp_0
bus: none, x + -tmp_0 = 0
"
        );
    }
}

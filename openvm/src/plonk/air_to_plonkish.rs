use std::collections::BTreeMap;

use super::{Gate, PlonkCircuit, Variable};
use crate::plonk::bus_interaction_handler::add_bus_to_plonk_circuit;
use crate::BusMap;
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};
use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::FieldElement;

pub fn build_circuit<T>(machine: &SymbolicMachine<T>) -> PlonkCircuit<T, AlgebraicReference>
where
    T: FieldElement,
{
    let mut circuit_builder = CircuitBuilder::<T>::new();
    for constraint in &machine.constraints {
        circuit_builder.evaluate_expression(&constraint.expr, true,true);
    }

    for bus_interaction in &machine.bus_interactions {
        add_bus_to_plonk_circuit(
            bus_interaction.clone(),
            &mut circuit_builder,
            &BusMap::openvm_base(),
        );
    }

    circuit_builder.build()
}

pub struct CircuitBuilder<T> {
    plonk_circuit: PlonkCircuit<T, AlgebraicReference>,
    temp_id_offset: usize,
    cache: BTreeMap<AlgebraicExpression<T>, (bool, Variable<AlgebraicReference>)>,
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
        is_last: bool,
    ) -> (bool, Variable<AlgebraicReference>) {
        if let Some((neg, var)) = self.cache.get(algebraic_expr) {
            return (*neg, var.clone());
        }

        let (neg, result) = match algebraic_expr {
            AlgebraicExpression::Reference(r) => {
                if assert_zero {
                    // Constraint of the form `w = 0`
                    self.plonk_circuit.add_gate(Gate {
                        q_l: T::ONE,

                        a: Variable::Witness(r.clone()),
                        ..Default::default()
                    });
                    (false, Variable::Unused)
                } else {
                    (false, Variable::Witness(r.clone()))
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
                (false, c)
            }
            AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
                let mut q_l = T::ZERO;
                let mut q_r = T::ZERO;
                let mut q_mul = T::ZERO;
                let mut q_const = T::ZERO;
                let mut a = Variable::Unused;
                let mut b = Variable::Unused;
                let neg_left: bool;
                let neg_right: bool;
                let (q_o, c) = self.make_output(assert_zero);
                match op {
                    AlgebraicBinaryOperator::Add => {
                        if let AlgebraicExpression::Number(n) = left.as_ref() {
                            q_const += *n;
                        } else {
                            (neg_left, a) = self.evaluate_expression(left, false, false);
                            q_l = if neg_left { -T::ONE } else { T::ONE };
                        }

                        if let AlgebraicExpression::Number(n) = right.as_ref() {
                            q_const += *n;
                        } else {
                            (neg_right, b) = self.evaluate_expression(right, false, false);
                            q_r = if neg_right { -T::ONE } else { T::ONE };
                        }
                    }
                    AlgebraicBinaryOperator::Sub => {
                        if let AlgebraicExpression::Number(n) = left.as_ref() {
                            q_const += *n;
                        } else {
                            (neg_left, a) = self.evaluate_expression(left, false, false);
                            q_l = if neg_left { -T::ONE } else { T::ONE };
                        }

                        if let AlgebraicExpression::Number(n) = right.as_ref() {
                            q_const -= *n;
                        } else {
                            (neg_right, b) = self.evaluate_expression(right, false, false);
                            q_r = if neg_right { T::ONE } else { -T::ONE };
                        }
                    }
                    AlgebraicBinaryOperator::Mul => match (left.as_ref(), right.as_ref()) {
                        (AlgebraicExpression::Number(n), AlgebraicExpression::Number(m)) => {
                            q_const += *n * *m;
                        }
                        (AlgebraicExpression::Number(n), non_constant)
                        | (non_constant, AlgebraicExpression::Number(n)) => {
                            // use neg_left for both cases
                            (neg_left, a) = self.evaluate_expression(non_constant, false, false);
                            q_l = if neg_left { -*n } else { *n };
                        }
                        _ => {
                            (neg_left, a) = self.evaluate_expression(left, false, false);
                            (neg_right, b) = self.evaluate_expression(right, false, false);
                            q_mul = if neg_left ^ neg_right {
                                -T::ONE
                            } else {
                                T::ONE
                            };
                        }
                    },
                    AlgebraicBinaryOperator::Pow => unimplemented!(),
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
                (false, c)
            }
            AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
                AlgebraicUnaryOperator::Minus => {
                    let (neg, var) = self.evaluate_expression(expr, false, false);

                    if assert_zero {
                        self.plonk_circuit.add_gate(Gate {
                            q_l: -T::ONE,
                            a: var.clone(),
                            c: Variable::Unused,
                            ..Default::default()
                        });
                        (true, Variable::Unused)
                    } else if is_last{
                        // if is_last is ture, assert_zero is false, the expression is used in bus interaction
                        if let AlgebraicExpression::Reference(r)= &**expr {
                            self.plonk_circuit.add_gate(Gate {
                                q_l: -T::ONE,
                                q_o: T::ONE,
                                a: Variable::Witness(r.clone()),
                                c: Variable::Tmp(self.temp_id_offset),
                                ..Default::default()
                            });
                            self.temp_id_offset += 1;
                            (false, Variable::Tmp(self.temp_id_offset - 1))
                        } else {
                            self.plonk_circuit.gates.last_mut().unwrap().q_o = -T::ONE * self.plonk_circuit.gates.last().unwrap().q_o;
                            (false, self.plonk_circuit.gates.last().unwrap().c.clone())
                        }
                    } else {
                        (true, var.clone())

                    }
                }
            },
            _ => {
                panic!("Unsupported algebraic expression: {algebraic_expr:?}");
            }
        };

        self.cache
            .insert(algebraic_expr.clone(), (neg, result.clone()));
        (neg, result)
    }

    pub fn build(self) -> PlonkCircuit<T, AlgebraicReference> {
        self.plonk_circuit
    }
}

#[cfg(test)]
mod tests {
    use crate::plonk::air_to_plonkish::build_circuit;
    use crate::plonk::test_utils::{c, var};
    use powdr_autoprecompiles::{SymbolicConstraint, SymbolicMachine};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_air_to_plonkish() {
        let x = var("x", 0);
        let y = var("y", 1);

        let expr = -(x.clone() * y.clone() - (-x.clone() * (x.clone() + y.clone())));
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine)),
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
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine)),
            "bus: none, -2 = tmp_1
bus: none, 2 * tmp_1 = tmp_0
bus: none, tmp_0 + 4 = 0
"
        )
    }

    #[test]
    fn single_variable() {
        let expr = -var("x", 0);
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine)),
            "bus: none, -x = 0
"
        )
    }

    #[test]
    fn constant_and_variables() {
        let x = var("x", 0);
        let y = var("y", 1);
        let expr = -(c(3) - c(2) * x.clone() * y.clone()) + c(1);
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine)),
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
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine)),
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
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine)),
            "bus: none, x + y = 0
"
        );
    }

    #[test]
    fn addition_test() {
        let x = var("x", 0);
        let y = var("y", 1);
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr:x.clone() + y.clone() },
            SymbolicConstraint { expr: x.clone() + y.clone() + (-y.clone()) },
            SymbolicConstraint { expr: (-x.clone()) + y.clone() },
            SymbolicConstraint { expr: x.clone() + c(1) },
            SymbolicConstraint { expr: -(x.clone()) + c(1) },
            SymbolicConstraint { expr: c(1) + x.clone() },],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine)),
            "
"
        );
    }


}

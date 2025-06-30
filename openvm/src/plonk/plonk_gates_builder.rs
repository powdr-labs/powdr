use crate::plonk::bus_interaction_handler::add_bus_to_plonk_circuit_from_quadratic_symbolic_expression;

use super::{Gate, PlonkCircuit, Variable};
use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_autoprecompiles::expression_conversion::algebraic_to_quadratic_symbolic_expression;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::FieldElement;
use std::collections::{BTreeMap, HashMap};

pub fn build_circuit_from_quadratic_symbolic_expression<T>(
    machine: &SymbolicMachine<T>,
    bus_map: &BusMap,
) -> PlonkCircuit<T, AlgebraicReference>
where
    T: FieldElement,
{
    let mut circuit_builder = CircuitBuilderQuadratic::<T>::new();

    for constraint in &machine.constraints {
        let quadratic_symbolic_expr = algebraic_to_quadratic_symbolic_expression(&constraint.expr);

        circuit_builder.evaluate_expression(&quadratic_symbolic_expr, true);
    }

    for bus_interaction in &machine.bus_interactions {
        add_bus_to_plonk_circuit_from_quadratic_symbolic_expression(
            bus_interaction.clone(),
            &mut circuit_builder,
            bus_map,
        );
    }

    circuit_builder.build()
}

pub struct CircuitBuilderQuadratic<T>
where
    T: FieldElement,
{
    pub plonk_circuit: PlonkCircuit<T, AlgebraicReference>,
    temp_id_offset: usize,
    cache:
        HashMap<QuadraticSymbolicExpression<T, AlgebraicReference>, Variable<AlgebraicReference>>,
}

impl<T> CircuitBuilderQuadratic<T>
where
    T: FieldElement,
{
    pub fn new() -> Self {
        CircuitBuilderQuadratic {
            plonk_circuit: PlonkCircuit::new(),
            temp_id_offset: 0,
            cache: HashMap::new(),
        }
    }

    pub fn evaluate_expression(
        &mut self,
        expr: &QuadraticSymbolicExpression<T, AlgebraicReference>,
        assert_zero: bool,
    ) -> Variable<AlgebraicReference> {
        if let Some(var) = self.cache.get(expr) {
            return var.clone();
        }

        // If the expression is 1 * variable, we can return the variable directly
        if !expr.is_quadratic() && expr.get_linear_terms().len() == 1 {
            if assert_zero {
                self.plonk_circuit.add_gate(Gate {
                    a: Variable::Witness(expr.get_linear_terms().keys().next().unwrap().clone()),
                    q_l: expr
                        .get_linear_terms()
                        .values()
                        .next()
                        .unwrap()
                        .try_to_number()
                        .expect("Expected a constant for q_l"),
                    q_o: -T::ZERO,
                    q_const: expr
                        .get_constant()
                        .try_to_number()
                        .expect("Expected a constant for q_const"),
                    c: Variable::Unused,
                    ..Default::default()
                });
                return Variable::Unused;
            } else if *expr.get_linear_terms().values().next().unwrap()
                == SymbolicExpression::Concrete(T::ONE)
                && *expr.get_constant() == SymbolicExpression::Concrete(T::ZERO)
            {
                return Variable::Witness(expr.get_linear_terms().keys().next().unwrap().clone());
            }
        }

        // Compute all the quadratic terms
        let mut quadratic_result: Vec<Variable<AlgebraicReference>> = Vec::new();
        expr.get_quadratic_terms().iter().for_each(|(left, right)| {
            let temp_expr = QuadraticSymbolicExpression::new(
                [(left.clone(), right.clone())].to_vec(),
                BTreeMap::new(),
                SymbolicExpression::Concrete(T::ZERO),
            );
            if let Some(var) = self.cache.get(&temp_expr) {
                quadratic_result.push(var.clone());
            } else {
                let mut simplify_expr = |expr: &QuadraticSymbolicExpression<
                    T,
                    AlgebraicReference,
                >|
                 -> (T, Variable<AlgebraicReference>) {
                    if !expr.is_quadratic() && expr.get_linear_terms().len() == 1 {
                        let (witness, coeff) = expr.get_linear_terms().iter().next().unwrap();
                        if *expr.get_constant() == SymbolicExpression::Concrete(T::ZERO) {
                            (
                                coeff.try_to_number().expect("expected a constant"),
                                Variable::Witness(witness.clone()),
                            )
                        } else {
                            self.plonk_circuit.add_gate(Gate {
                                a: Variable::Witness(witness.clone()),
                                q_l: coeff.try_to_number().expect("Expected a constant for q_l"),
                                q_o: -T::ONE,
                                q_const: expr
                                    .get_constant()
                                    .try_to_number()
                                    .expect("Expected a constant for q_const"),
                                c: Variable::Tmp(self.temp_id_offset),
                                ..Default::default()
                            });
                            self.temp_id_offset += 1;
                            (T::ONE, Variable::Tmp(self.temp_id_offset - 1))
                        }
                    } else {
                        (T::ONE, self.evaluate_expression(expr, false))
                    }
                };

                let (mula, a) = simplify_expr(left);
                let (mulb, b) = simplify_expr(right);
                self.plonk_circuit.add_gate(Gate {
                    a,
                    b,
                    q_mul: mula * mulb,
                    q_o: -T::ONE,
                    c: Variable::Tmp(self.temp_id_offset),
                    ..Default::default()
                });
                self.temp_id_offset += 1;
                quadratic_result.push(Variable::Tmp(self.temp_id_offset - 1));
                self.cache
                    .insert(temp_expr, Variable::Tmp(self.temp_id_offset - 1));
            }
        });

        let mut temp = quadratic_result
            .iter()
            .cloned()
            .fold(Variable::Unused, |acc, var| {
                if let Variable::Unused = acc {
                    var
                } else {
                    self.plonk_circuit.add_gate(Gate {
                        q_l: T::ONE,
                        a: acc.clone(),
                        q_r: T::ONE,
                        b: var.clone(),
                        q_o: -T::ONE,
                        c: Variable::Tmp(self.temp_id_offset),
                        ..Default::default()
                    });
                    self.temp_id_offset += 1;
                    Variable::Tmp(self.temp_id_offset - 1)
                }
            });

        // Compute all the linear terms
        let mut iter = expr.get_linear_terms().iter();

        let mut linear_results: Vec<(T, Variable<AlgebraicReference>)> = Vec::new();

        while let (Some((poly_a, c_a)), Some((poly_b, c_b))) = (iter.next(), iter.next()) {
            let temp_expr = QuadraticSymbolicExpression::new(
                Vec::new(),
                BTreeMap::from([(poly_a.clone(), c_a.clone()), (poly_b.clone(), c_b.clone())]),
                SymbolicExpression::Concrete(T::ZERO),
            );
            if let Some(var) = self.cache.get(&temp_expr) {
                linear_results.push((T::ONE, var.clone()));
            } else {
                self.plonk_circuit.add_gate(Gate {
                    q_l: c_a.try_to_number().expect("Expected a constant for q_l"),
                    a: Variable::Witness(poly_a.clone()),
                    q_r: c_b.try_to_number().expect("Expected a constant for q_r"),
                    b: Variable::Witness(poly_b.clone()),
                    q_o: -T::ONE,
                    c: Variable::Tmp(self.temp_id_offset),
                    ..Default::default()
                });
                self.temp_id_offset += 1;
                linear_results.push((T::ONE, Variable::Tmp(self.temp_id_offset - 1)));
                self.cache
                    .insert(temp_expr.clone(), Variable::Tmp(self.temp_id_offset - 1));
            }
        }

        // Handle the last unpaired item if odd length
        if expr.get_linear_terms().len() % 2 != 0 {
            if let Some((poly_a, c_a)) = expr.get_linear_terms().iter().last() {
                let temp_expr = QuadraticSymbolicExpression::new(
                    Vec::new(),
                    BTreeMap::from([(poly_a.clone(), c_a.clone())]),
                    SymbolicExpression::Concrete(T::ZERO),
                );
                if let Some(var) = self.cache.get(&temp_expr) {
                    linear_results.push((T::ONE, var.clone()));
                } else {
                    linear_results.push((
                        c_a.try_to_number().expect("Expected a constant for c_a"),
                        Variable::Witness(poly_a.clone()),
                    ));
                }
            }
        }

        if temp == Variable::Unused
            && linear_results.len() == 1
            && *expr.get_constant() == SymbolicExpression::Concrete(T::ZERO)
            && !assert_zero
            && linear_results[0].0 == T::ONE
        {
            return linear_results[0].1.clone();
        }

        if !linear_results.is_empty() {
            temp = linear_results
                .iter()
                .cloned()
                .fold(temp.clone(), |acc, (mul, var)| {
                    if let Variable::Unused = acc {
                        if linear_results.len() == 1 {
                            self.plonk_circuit.add_gate(Gate {
                                q_l: linear_results[0].0,
                                a: var.clone(),
                                q_o: -T::ONE,
                                c: Variable::Tmp(self.temp_id_offset),
                                ..Default::default()
                            });
                            self.temp_id_offset += 1;
                            Variable::Tmp(self.temp_id_offset - 1)
                        } else {
                            var.clone()
                        }
                    } else {
                        self.plonk_circuit.add_gate(Gate {
                            q_l: T::ONE,
                            a: acc.clone(),
                            q_r: mul,
                            b: var.clone(),
                            q_o: -T::ONE,
                            c: Variable::Tmp(self.temp_id_offset),
                            ..Default::default()
                        });
                        self.temp_id_offset += 1;
                        Variable::Tmp(self.temp_id_offset - 1)
                    }
                });
        }

        // When a bus interaction has constants as arguments
        if temp == Variable::Unused {
            self.plonk_circuit.add_gate(Gate {
                q_const: expr
                    .get_constant()
                    .try_to_number()
                    .expect("Expected a constant for q_const"),
                q_o: -T::ONE,
                c: Variable::Tmp(self.temp_id_offset),
                ..Default::default()
            });
            self.temp_id_offset += 1;
            temp = Variable::Tmp(self.temp_id_offset - 1);
        }

        if *expr.get_constant() != SymbolicExpression::Concrete(T::ZERO) {
            self.plonk_circuit.gates.last_mut().unwrap().q_const = expr
                .get_constant()
                .try_to_number()
                .expect("Expected a constant for q_const");
        }

        self.cache.insert(expr.clone(), temp.clone());

        if assert_zero {
            self.plonk_circuit.gates.last_mut().unwrap().q_o = T::ZERO;
            self.plonk_circuit.gates.last_mut().unwrap().c = Variable::Unused;
            Variable::Unused
        } else {
            temp
        }
    }

    /// Adds a gate to the PlonK circuit.
    pub fn add_gate(&mut self, gate: Gate<T, AlgebraicReference>) {
        self.plonk_circuit.add_gate(gate);
    }

    pub fn build(self) -> PlonkCircuit<T, AlgebraicReference> {
        self.plonk_circuit
    }
}

#[cfg(test)]
mod tests {
    use crate::bus_map::default_openvm_bus_map;
    use crate::plonk::plonk_gates_builder::build_circuit_from_quadratic_symbolic_expression;
    use crate::plonk::test_utils::{c, var};
    use powdr_autoprecompiles::{SymbolicConstraint, SymbolicMachine};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_air_to_plonkish() {
        let x = var("x", 0);
        let y = var("y", 1);
        let bus_map = default_openvm_bus_map();

        let expr1 = x.clone() * y.clone() - (-x.clone() * (x.clone() + y.clone()));
        let expr2 = -(c(3) - c(2) * x.clone() * y.clone()) + c(1);
        let expr3 = c(5) * (c(3) * x.clone() + c(10)) * (c(1) + y.clone());
        let machine = SymbolicMachine {
            constraints: vec![
                SymbolicConstraint { expr: expr1 },
                SymbolicConstraint { expr: expr2 },
                SymbolicConstraint { expr: expr3 },
            ],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!(
                "{}",
                build_circuit_from_quadratic_symbolic_expression(&machine, &bus_map)
            ),
            "bus: none, x * y = tmp_0
bus: none, x + y = tmp_1
bus: none, x * tmp_1 = tmp_2
bus: none, tmp_0 + tmp_2 = 0
bus: none, 2 * x * y + -2 = 0
bus: none, 15 * x + 50 = tmp_5
bus: none, y + 1 = tmp_6
bus: none, tmp_5 * tmp_6 = 0
"
        );
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
            format!(
                "{}",
                build_circuit_from_quadratic_symbolic_expression(&machine, &bus_map)
            ),
            "bus: none, x = 0
"
        )
    }
}

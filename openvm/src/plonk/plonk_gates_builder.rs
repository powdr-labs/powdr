use crate::plonk::bus_interaction_handler::{
    add_bus_to_plonk_circuit, add_bus_to_plonk_circuit_from_quadratic_symbolic_expression,
};

use super::{Gate, PlonkCircuit, Variable};
use crate::plonk::air_to_plonkish::CircuitBuilder;
use openvm::platform::print;
use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::expression::AlgebraicReference;
use powdr_autoprecompiles::expression_conversion::algebraic_to_quadratic_symbolic_expression;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_constraint_solver::{
    quadratic_symbolic_expression::QuadraticSymbolicExpression,
    symbolic_expression::SymbolicExpression,
};
use powdr_number::FieldElement;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    env::consts::EXE_EXTENSION,
};

pub fn build_circuit_from_quadratic_simbolic_expression<T>(
    machine: &SymbolicMachine<T>,
    bus_map: &BusMap,
) -> PlonkCircuit<T, AlgebraicReference>
where
    T: FieldElement,
{
    let mut circuit_builder = CircuitBuilderQuadratic::<T>::new();

    let mut constraint_format: BTreeMap<(bool, usize, usize), (u64,u64)> = BTreeMap::new();
    let mut constraint_cache=HashSet::new();

    tracing::debug!("number of constraints: {}", machine.constraints.len());

    for constraint in &machine.constraints {
        let quadratic_symbolic_expr = algebraic_to_quadratic_symbolic_expression(&constraint.expr);

        circuit_builder.evaluate_expression(&quadratic_symbolic_expr, true);

        if constraint_format
            .get(&(
                quadratic_symbolic_expr.is_quadratic(),
                if quadratic_symbolic_expr.is_quadratic() {
                    quadratic_symbolic_expr.quadratic[0].0.linear.len()
                } else {
                    quadratic_symbolic_expr.linear.len()
                },
                if quadratic_symbolic_expr.is_quadratic() {
                    quadratic_symbolic_expr.quadratic[0].1.linear.len()
                } else {
                    0
                },
            ))
            .is_none()
        {
            constraint_format.insert(
                (
                    quadratic_symbolic_expr.is_quadratic(),
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].0.linear.len()
                    } else {
                        quadratic_symbolic_expr.linear.len()
                    },
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].1.linear.len()
                    } else {
                        0
                    },
                ),
                (1,1)
            );
            constraint_cache.insert(quadratic_symbolic_expr);
        } else {
            let (format_count,unique_count) = constraint_format
                .get_mut(&(
                    quadratic_symbolic_expr.is_quadratic(),
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].0.linear.len()
                    } else {
                        quadratic_symbolic_expr.linear.len()
                    },
                    if quadratic_symbolic_expr.is_quadratic() {
                        quadratic_symbolic_expr.quadratic[0].1.linear.len()
                    } else {
                        0
                    },
                ))
                .unwrap();
            *format_count += 1;

            if !constraint_cache.contains(&quadratic_symbolic_expr) {
                *unique_count += 1;
                constraint_cache.insert(quadratic_symbolic_expr);
            }
        }
    }

    tracing::debug!(
        "number of gates for constraints: {}",
        circuit_builder.plonk_circuit.len()
    );

    tracing::debug!(
        "number of bus interactions: {}",
        machine.bus_interactions.len()
    );

    for bus_interaction in &machine.bus_interactions {
        //length = circuit_builder.plonk_circuit.len();
        add_bus_to_plonk_circuit_from_quadratic_symbolic_expression(
            bus_interaction.clone(),
            &mut circuit_builder,
            bus_map,
            &mut constraint_format,
            &mut constraint_cache
        );

        //length = circuit_builder.plonk_circuit.len() - length;
        //tracing::debug!("Number of gates added from bus interaction: {}", length);

        // let slice = &circuit_builder.plonk_circuit.gates
        //     [circuit_builder.plonk_circuit.gates.len() - length..];

        // for gate in slice {
        //     tracing::debug!("Gate: {}", gate);
        // }
    }

    tracing::debug!(
        "Number of gates in plonk circuit: {}",
        circuit_builder.plonk_circuit.len()
    );
    for constraint in constraint_format {
        tracing::debug!("Constraint formats: {:?}", constraint);
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
        if !expr.is_quadratic()
            && expr.linear.len() == 1
            && *expr.linear.values().next().unwrap() == SymbolicExpression::Concrete(T::ONE)
        {
            if expr.constant == SymbolicExpression::Concrete(T::ZERO) {
                if assert_zero {
                    self.plonk_circuit.add_gate(Gate {
                        a: Variable::Witness(expr.linear.keys().next().unwrap().clone()),
                        q_l: T::ONE,
                        q_o: -T::ZERO,
                        q_const: T::ZERO,
                        c: Variable::Unused,
                        ..Default::default()
                    });
                    return Variable::Unused;
                } else {
                    return Variable::Witness(expr.linear.keys().next().unwrap().clone());
                }
            } else {
                if assert_zero {
                    self.plonk_circuit.add_gate(Gate {
                        a: Variable::Witness(expr.linear.keys().next().unwrap().clone()),
                        q_l: T::ONE,
                        q_o: -T::ZERO,
                        q_const: expr
                            .constant
                            .try_to_number()
                            .expect("Expected a constant for q_const"),
                        c: Variable::Unused,
                        ..Default::default()
                    });
                    return Variable::Unused;
                } else {
                    self.plonk_circuit.add_gate(Gate {
                        a: Variable::Witness(expr.linear.keys().next().unwrap().clone()),
                        q_l: T::ONE,
                        q_o: -T::ONE,
                        q_const: expr
                            .constant
                            .try_to_number()
                            .expect("Expected a constant for q_const"),
                        c: Variable::Tmp(self.temp_id_offset),
                        ..Default::default()
                    });
                    self.temp_id_offset += 1;
                    self.cache
                        .insert(expr.clone(), Variable::Tmp(self.temp_id_offset - 1));
                    return Variable::Tmp(self.temp_id_offset - 1);
                }
            }
        }

        if !expr.is_quadratic()
            && expr.linear.len() == 1
            && *expr.linear.values().next().unwrap() != SymbolicExpression::Concrete(T::ONE)
        {
            if assert_zero {
                self.plonk_circuit.add_gate(Gate {
                    a: Variable::Witness(expr.linear.keys().next().unwrap().clone()),
                    q_l: expr
                        .linear
                        .values()
                        .next()
                        .unwrap()
                        .try_to_number()
                        .expect("Expected a constant for q_l"),
                    q_o: -T::ZERO,
                    q_const: expr
                        .constant
                        .try_to_number()
                        .expect("Expected a constant for q_const"),
                    c: Variable::Unused,
                    ..Default::default()
                });
                return Variable::Unused;
            } else {
                if let Some(var)= self.cache.get(expr) {
                    return var.clone();
                }
                self.plonk_circuit.add_gate(Gate {
                    a: Variable::Witness(expr.linear.keys().next().unwrap().clone()),
                    q_l: expr
                        .linear
                        .values()
                        .next()
                        .unwrap()
                        .try_to_number()
                        .expect("Expected a constant for q_l"),
                    q_o: -T::ONE,
                    q_const: expr
                        .constant
                        .try_to_number()
                        .expect("Expected a constant for q_const"),
                    c: Variable::Tmp(self.temp_id_offset),
                    ..Default::default()
                });
                self.temp_id_offset += 1;
                self.cache
                    .insert(expr.clone(), Variable::Tmp(self.temp_id_offset - 1));
                return Variable::Tmp(self.temp_id_offset - 1);
            }
        }

        let mut quadratic_result: Vec<Variable<AlgebraicReference>> = Vec::new();

        expr.quadratic.iter().for_each(|(left, right)| {
            let temp_expr = QuadraticSymbolicExpression {
                quadratic: [(left.clone(), right.clone())].to_vec(),
                linear: BTreeMap::new(),
                constant: SymbolicExpression::Concrete(T::ZERO),
            };
            if let Some(var) = self.cache.get(&temp_expr) {
                quadratic_result.push(var.clone());
            } else {
                let (mula, a) = if !left.is_quadratic() && left.linear.len() == 1 {
                    if left.constant == SymbolicExpression::Concrete(T::ZERO) {
                        (
                            left.linear
                                .values()
                                .next()
                                .unwrap()
                                .try_to_number()
                                .expect("expected a constant"),
                            Variable::Witness(left.linear.keys().next().unwrap().clone()),
                        )
                    } else {
                        self.plonk_circuit.add_gate(Gate {
                            a: Variable::Witness(left.linear.keys().next().unwrap().clone()),
                            q_l: left
                                .linear
                                .values()
                                .next()
                                .unwrap()
                                .try_to_number()
                                .expect("Expected a constant for q_l"),
                            q_o: -T::ONE,
                            q_const: left
                                .constant
                                .try_to_number()
                                .expect("Expected a constant for q_const"),
                            c: Variable::Tmp(self.temp_id_offset),
                            ..Default::default()
                        });
                        self.temp_id_offset += 1;
                        (T::ONE, Variable::Tmp(self.temp_id_offset - 1))
                    }
                } else {
                    (T::ONE, self.evaluate_expression(left, false))
                };
                let (mulb, b) = if !right.is_quadratic() && right.linear.len() == 1 {
                    if right.constant == SymbolicExpression::Concrete(T::ZERO) {
                        (
                            right
                                .linear
                                .values()
                                .next()
                                .unwrap()
                                .try_to_number()
                                .expect("expected a constant"),
                            Variable::Witness(right.linear.keys().next().unwrap().clone()),
                        )
                    } else {
                        self.plonk_circuit.add_gate(Gate {
                            a: Variable::Witness(right.linear.keys().next().unwrap().clone()),
                            q_l: right
                                .linear
                                .values()
                                .next()
                                .unwrap()
                                .try_to_number()
                                .expect("Expected a constant for q_l"),
                            q_o: -T::ONE,
                            q_const: right
                                .constant
                                .try_to_number()
                                .expect("Expected a constant for q_const"),
                            c: Variable::Tmp(self.temp_id_offset),
                            ..Default::default()
                        });
                        self.temp_id_offset += 1;
                        (T::ONE, Variable::Tmp(self.temp_id_offset - 1))
                    }
                } else {
                    (T::ONE, self.evaluate_expression(right, false))
                };
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

        // add all the quadratic products together
        let mut temp = quadratic_result
            .iter()
            .next()
            .cloned()
            .unwrap_or(Variable::Unused);
        quadratic_result.iter().skip(1).for_each(|var| {
            self.plonk_circuit.add_gate(Gate {
                q_l: T::ONE,
                a: temp.clone(),
                q_r: T::ONE,
                b: var.clone(),
                q_o: -T::ONE,
                c: Variable::Tmp(self.temp_id_offset),
                ..Default::default()
            });
            self.temp_id_offset += 1;
            temp = Variable::Tmp(self.temp_id_offset - 1);
        });

        // Compute all the linear terms
        let mut iter = expr.linear.iter();

        let mut linear_results: Vec<(T, Variable<AlgebraicReference>)> = Vec::new();

        while let (Some((poly_a, c_a)), Some((poly_b, c_b))) = (iter.next(), iter.next()) {
            let temp_expr = QuadraticSymbolicExpression {
                quadratic: Vec::new(),
                linear: BTreeMap::from([
                    (poly_a.clone(), c_a.clone()),
                    (poly_b.clone(), c_b.clone()),
                ]),
                constant: SymbolicExpression::Concrete(T::ZERO),
            };
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
        if expr.linear.len() % 2 != 0 {
            if let Some((poly_a, c_a)) = expr.linear.iter().last() {
                let temp_expr = QuadraticSymbolicExpression {
                    quadratic: Vec::new(),
                    linear: BTreeMap::from([(poly_a.clone(), c_a.clone())]),
                    constant: SymbolicExpression::Concrete(T::ZERO),
                };
                if let Some(var) = self.cache.get(&temp_expr) {
                    linear_results.push((T::ONE, var.clone()));
                } else {
                    linear_results.push((
                        c_a.try_to_number().expect("Expected a constant for c_a"),
                        Variable::Witness(poly_a.clone()),
                    ));
                    //self.cache.insert(temp_expr.clone(), Variable::Tmp(self.temp_id_offset - 1));
                }
            }
        }

        if !linear_results.is_empty() {
            if temp == Variable::Unused {
                temp = linear_results
                    .iter()
                    .next()
                    .cloned()
                    .unwrap_or((T::ONE, Variable::Unused))
                    .1;
                if linear_results.len() == 1 {
                    self.plonk_circuit.add_gate(Gate {
                        q_l: linear_results[0].0,
                        a: temp.clone(),
                        q_o: -T::ONE,
                        c: Variable::Tmp(self.temp_id_offset),
                        ..Default::default()
                    });
                    self.temp_id_offset += 1;
                    temp = Variable::Tmp(self.temp_id_offset - 1);
                }
            } else {
                self.plonk_circuit.add_gate(Gate {
                    q_l: linear_results[0].0,
                    a: temp.clone(),
                    q_r: T::ONE,
                    b: linear_results[0].clone().1,
                    q_o: -T::ONE,
                    c: Variable::Tmp(self.temp_id_offset),
                    ..Default::default()
                });
                self.temp_id_offset += 1;
                temp = Variable::Tmp(self.temp_id_offset - 1);
            }

            linear_results.iter().skip(1).for_each(|(mul, var)| {
                // println!("plonk circuit til now in linear iter: {}", self.plonk_circuit);
                self.plonk_circuit.add_gate(Gate {
                    q_l: T::ONE,
                    a: temp.clone(),
                    q_r: *mul,
                    b: var.clone(),
                    q_o: -T::ONE,
                    c: Variable::Tmp(self.temp_id_offset),
                    ..Default::default()
                });
                self.temp_id_offset += 1;
                temp = Variable::Tmp(self.temp_id_offset - 1);
            });
        }

        if temp != Variable::Unused {
            if expr.constant != SymbolicExpression::Concrete(T::ZERO) {
                self.plonk_circuit.gates.last_mut().unwrap().q_const = expr
                    .constant
                    .try_to_number()
                    .expect("Expected a constant for q_const");
            }
        } else {
            self.plonk_circuit.add_gate(Gate {
                q_const: expr
                    .constant
                    .try_to_number()
                    .expect("Expected a constant for q_const"),
                q_o: -T::ONE,
                c: Variable::Tmp(self.temp_id_offset),
                ..Default::default()
            });
            self.temp_id_offset += 1;
            temp = Variable::Tmp(self.temp_id_offset - 1);
        }

        self.cache.insert(expr.clone(), temp.clone());

        if assert_zero {
            self.plonk_circuit.gates.last_mut().unwrap().q_o = -T::ZERO;
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
    use crate::plonk::plonk_gates_builder::build_circuit_from_quadratic_simbolic_expression;
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
                build_circuit_from_quadratic_simbolic_expression(&machine, &bus_map)
            ),
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
                build_circuit_from_quadratic_simbolic_expression(&machine, &bus_map)
            ),
            "bus: none, x = 0
"
        )
    }
}

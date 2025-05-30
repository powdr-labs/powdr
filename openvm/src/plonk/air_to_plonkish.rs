use super::{Gate, PlonkCircuit, Variable};
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
    let mut circuit = PlonkCircuit::new();
    let mut temp_id_offset = 0;
    for constraint in &machine.constraints {
        air_to_plonkish(&constraint.expr, &mut circuit, &mut temp_id_offset, true);
    }

    // TODO: Add bus interactions

    circuit
}

pub fn air_to_plonkish<T>(
    algebraic_expr: &AlgebraicExpression<T>,
    plonk_circuit: &mut PlonkCircuit<T, AlgebraicReference>,
    temp_id_offset: &mut usize,
    assert_zero: bool,
) -> Variable<AlgebraicReference>
where
    T: FieldElement,
{
    // Returns (q_o, c), where:
    // - If `assert_zero` is true, `q_o` is always zero and `c` is unused.
    // - If `assert_zero` is false, `q_o` is -1 and `c` is a new temporary variable.
    let mut make_output = || -> (T, Variable<AlgebraicReference>) {
        if assert_zero {
            (T::ZERO, Variable::Unused)
        } else {
            let c = Variable::Tmp(*temp_id_offset);
            *temp_id_offset += 1;
            (-T::ONE, c)
        }
    };

    match algebraic_expr {
        AlgebraicExpression::Reference(r) => {
            if assert_zero {
                // Constraint of the form `w = 0`
                plonk_circuit.add_gate(Gate {
                    q_l: T::ONE,
                    q_r: T::ZERO,
                    q_o: T::ZERO,
                    q_mul: T::ZERO,
                    q_const: T::ZERO,

                    q_bitwise: T::ZERO,
                    q_memory: T::ZERO,
                    q_execution: T::ZERO,
                    q_pc: T::ZERO,
                    q_rang_tuple: T::ZERO,
                    q_range_check: T::ZERO,

                    a: Variable::Witness(r.clone()),
                    b: Variable::Unused,
                    c: Variable::Unused,
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
            let (q_o, c) = make_output();
            plonk_circuit.add_gate(Gate {
                q_l: T::ZERO,
                q_r: T::ZERO,
                q_o,
                q_mul: T::ZERO,
                q_const: *value,

                q_bitwise: T::ZERO,
                q_memory: T::ZERO,
                q_execution: T::ZERO,
                q_pc: T::ZERO,
                q_rang_tuple: T::ZERO,
                q_range_check: T::ZERO,

                a: Variable::Unused,
                b: Variable::Unused,
                c: c.clone(),
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
            let (q_o, c) = make_output();
            match op {
                AlgebraicBinaryOperator::Add => {
                    if let AlgebraicExpression::Number(n) = left.as_ref() {
                        q_const += *n;
                    } else {
                        q_l = T::ONE;
                        a = air_to_plonkish(left, plonk_circuit, temp_id_offset, false);
                    }

                    if let AlgebraicExpression::Number(n) = right.as_ref() {
                        q_const += *n;
                    } else {
                        q_r = T::ONE;
                        b = air_to_plonkish(right, plonk_circuit, temp_id_offset, false);
                    }
                }
                AlgebraicBinaryOperator::Sub => {
                    if let AlgebraicExpression::Number(n) = left.as_ref() {
                        q_const += *n;
                    } else {
                        q_l = T::ONE;
                        a = air_to_plonkish(left, plonk_circuit, temp_id_offset, false);
                    }

                    if let AlgebraicExpression::Number(n) = right.as_ref() {
                        q_const -= *n;
                    } else {
                        q_r = -T::ONE;
                        b = air_to_plonkish(right, plonk_circuit, temp_id_offset, false);
                    }
                }
                AlgebraicBinaryOperator::Mul => match (left.as_ref(), right.as_ref()) {
                    (AlgebraicExpression::Number(n), AlgebraicExpression::Number(m)) => {
                        q_const += *n * *m;
                    }
                    (AlgebraicExpression::Number(n), non_constant)
                    | (non_constant, AlgebraicExpression::Number(n)) => {
                        q_l = *n;
                        a = air_to_plonkish(non_constant, plonk_circuit, temp_id_offset, false);
                    }
                    _ => {
                        q_mul = T::ONE;
                        a = air_to_plonkish(left, plonk_circuit, temp_id_offset, false);
                        b = air_to_plonkish(right, plonk_circuit, temp_id_offset, false);
                    }
                },
                AlgebraicBinaryOperator::Pow => unimplemented!(),
            };
            plonk_circuit.add_gate(Gate {
                q_l,
                q_r,
                q_o,
                q_mul,
                q_const,

                q_bitwise: T::ZERO,
                q_memory: T::ZERO,
                q_execution: T::ZERO,
                q_pc: T::ZERO,
                q_rang_tuple: T::ZERO,
                q_range_check: T::ZERO,

                a,
                b,
                c: c.clone(),
            });
            c
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
            AlgebraicUnaryOperator::Minus => {
                let (q_o, c) = make_output();
                let a = air_to_plonkish(expr, plonk_circuit, temp_id_offset, false);
                plonk_circuit.add_gate(Gate {
                    q_l: -T::ONE,
                    q_r: T::ZERO,
                    q_o,
                    q_mul: T::ZERO,
                    q_const: T::ZERO,

                    q_bitwise: T::ZERO,
                    q_memory: T::ZERO,
                    q_execution: T::ZERO,
                    q_pc: T::ZERO,
                    q_rang_tuple: T::ZERO,
                    q_range_check: T::ZERO,

                    a,
                    b: Variable::Unused,
                    c: c.clone(),
                });
                c
            }
        },
        _ => {
            panic!("Unsupported algebraic expression: {algebraic_expr:?}");
        }
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
        let expr = var("x", 0);
        let machine = SymbolicMachine {
            constraints: vec![SymbolicConstraint { expr }],
            bus_interactions: vec![],
        };

        assert_eq!(
            format!("{}", build_circuit(&machine)),
            "bus: none, x = 0
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
            "bus: none, -y = tmp_0
bus: none, x + -tmp_0 = 0
"
        );
    }
}

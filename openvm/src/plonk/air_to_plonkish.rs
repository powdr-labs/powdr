use super::{Gate, PlonkCircuit, Variable};
use powdr_ast::analyzed::{
    AlgebraicBinaryOperation, AlgebraicBinaryOperator, AlgebraicExpression, AlgebraicReference,
    AlgebraicUnaryOperation, AlgebraicUnaryOperator,
};
use powdr_number::FieldElement;

pub fn build_plonk_expr<T>(
    algebraic_expr: &AlgebraicExpression<T>,
    temp_id_offset: &mut usize,
) -> PlonkCircuit<T, AlgebraicReference>
where
    T: FieldElement,
{
    let mut plonkish_expr = PlonkCircuit::new();
    let a = air_to_plonkish(algebraic_expr, &mut plonkish_expr, temp_id_offset);
    // The last gate's output is the result of the expression, which is evaluated to zero.
    if !plonkish_expr.gates.is_empty() {
        plonkish_expr.gates.last_mut().unwrap().q_o = T::ZERO;
        plonkish_expr.gates.last_mut().unwrap().c = Variable::Unused;
    } else {
        // For expression like x=0, the gates will be empty, but we add a dummy gate to represent this expression.
        plonkish_expr.add_gate(Gate {
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

            a,
            b: Variable::Unused,
            c: Variable::Unused,
        });
    }

    plonkish_expr
}
pub fn air_to_plonkish<T>(
    algebraic_expr: &AlgebraicExpression<T>,
    plonk_circuit: &mut PlonkCircuit<T, AlgebraicReference>,
    temp_id_offset: &mut usize,
) -> Variable<AlgebraicReference>
where
    T: FieldElement,
{
    match algebraic_expr {
        AlgebraicExpression::Reference(r) => Variable::Witness(r.clone()),
        // Ideally, we would never hit this case, because allocating a new gate just to
        // "compute" a constant is wasteful.
        // The implementations of the binary operations below already handle one of their
        // operands being a number directly. The only way to reach this path is by passing
        // a number to `build_plonk_expr` directly, or by having a `-<number>` expression.
        AlgebraicExpression::Number(value) => {
            let c = Variable::Tmp(*temp_id_offset);
            *temp_id_offset += 1;
            plonk_circuit.add_gate(Gate {
                q_l: T::ZERO,
                q_r: T::ZERO,
                q_o: -T::ONE,
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
                c,
            });
            plonk_circuit.gates.last().unwrap().c.clone()
        }
        AlgebraicExpression::BinaryOperation(AlgebraicBinaryOperation { left, op, right }) => {
            let mut q_l = T::ZERO;
            let mut q_r = T::ZERO;
            let mut q_mul = T::ZERO;
            let mut q_const = T::ZERO;
            let mut a = Variable::Unused;
            let mut b = Variable::Unused;
            match op {
                AlgebraicBinaryOperator::Add => {
                    if let AlgebraicExpression::Number(n) = left.as_ref() {
                        q_const += *n;
                    } else {
                        q_l = T::ONE;
                        a = air_to_plonkish(left, plonk_circuit, temp_id_offset);
                    }

                    if let AlgebraicExpression::Number(n) = right.as_ref() {
                        q_const += *n;
                    } else {
                        q_r = T::ONE;
                        b = air_to_plonkish(right, plonk_circuit, temp_id_offset);
                    }
                }
                AlgebraicBinaryOperator::Sub => {
                    if let AlgebraicExpression::Number(n) = left.as_ref() {
                        q_const += *n;
                    } else {
                        q_l = T::ONE;
                        a = air_to_plonkish(left, plonk_circuit, temp_id_offset);
                    }

                    if let AlgebraicExpression::Number(n) = right.as_ref() {
                        q_const -= *n;
                    } else {
                        q_r = -T::ONE;
                        b = air_to_plonkish(right, plonk_circuit, temp_id_offset);
                    }
                }
                AlgebraicBinaryOperator::Mul => match (left.as_ref(), right.as_ref()) {
                    (AlgebraicExpression::Number(n), AlgebraicExpression::Number(m)) => {
                        q_const += *n * *m;
                    }
                    (AlgebraicExpression::Number(n), non_constant)
                    | (non_constant, AlgebraicExpression::Number(n)) => {
                        q_l = *n;
                        a = air_to_plonkish(non_constant, plonk_circuit, temp_id_offset);
                    }
                    _ => {
                        q_mul = T::ONE;
                        a = air_to_plonkish(left, plonk_circuit, temp_id_offset);
                        b = air_to_plonkish(right, plonk_circuit, temp_id_offset);
                    }
                },
                AlgebraicBinaryOperator::Pow => unimplemented!(),
            };
            let c = Variable::Tmp(*temp_id_offset);
            *temp_id_offset += 1;
            plonk_circuit.add_gate(Gate {
                q_l,
                q_r,
                q_o: -T::ONE,
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
                c,
            });
            plonk_circuit.gates.last().unwrap().c.clone()
        }
        AlgebraicExpression::UnaryOperation(AlgebraicUnaryOperation { op, expr }) => match op {
            AlgebraicUnaryOperator::Minus => {
                let a = air_to_plonkish(expr, plonk_circuit, temp_id_offset);
                let c = Variable::Tmp(*temp_id_offset);
                *temp_id_offset += 1;
                plonk_circuit.add_gate(Gate {
                    q_l: -T::ONE,
                    q_r: T::ZERO,
                    q_o: -T::ONE,
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
                    c,
                });
                plonk_circuit.gates.last().unwrap().c.clone()
            }
        },
        _ => {
            panic!("Unsupported algebraic expression: {algebraic_expr:?}");
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::plonk::air_to_plonkish::build_plonk_expr;
    use crate::plonk::test_utils::{c, var};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_air_to_plonkish() {
        let x = var("x", 0);
        let y = var("y", 1);
        let expr = -(x.clone() * y.clone() - (-x.clone() * (x.clone() + y.clone())));
        let mut temp_id_offset = 0;

        assert_eq!(
            format!("{}", build_plonk_expr(&expr, &mut temp_id_offset)),
            // tmp_0 = x * y
            // tmp_1 = -x
            // tmp_2 = x + y
            // tmp_3 = tmp_1 * tmp_2
            // tmp_4 = tmp_0 - tmp_3
            // -tmp_4 = 0
            "bus: none, 0 * x + 0 * y + -1 * tmp_0 + 1 * x * y + 0 = 0
bus: none, -1 * x + 0 * Unused + -1 * tmp_1 + 0 * x * Unused + 0 = 0
bus: none, 1 * x + 1 * y + -1 * tmp_2 + 0 * x * y + 0 = 0
bus: none, 0 * tmp_1 + 0 * tmp_2 + -1 * tmp_3 + 1 * tmp_1 * tmp_2 + 0 = 0
bus: none, 1 * tmp_0 + -1 * tmp_3 + -1 * tmp_4 + 0 * tmp_0 * tmp_3 + 0 = 0
bus: none, -1 * tmp_4 + 0 * Unused + 0 * Unused + 0 * tmp_4 * Unused + 0 = 0
"
        );
    }

    #[test]
    fn only_constants() {
        let expr = c(4) + c(2) * (c(3) - c(5));
        let mut temp_id_offset = 0;

        assert_eq!(
            format!("{}", build_plonk_expr(&expr, &mut temp_id_offset)),
            // tmp_0 = -2        (3 - 5)
            // tmp_1 = 2 * tmp_0
            // 4 + tmp_1 = 0
            "bus: none, 0 * Unused + 0 * Unused + -1 * tmp_0 + 0 * Unused * Unused + -2 = 0
bus: none, 2 * tmp_0 + 0 * Unused + -1 * tmp_1 + 0 * tmp_0 * Unused + 0 = 0
bus: none, 0 * Unused + 1 * tmp_1 + 0 * Unused + 0 * Unused * tmp_1 + 4 = 0
"
        )
    }

    #[test]
    fn single_variable() {
        let x = var("x", 0);
        let expr = x.clone();
        let mut temp_id_offset = 0;

        assert_eq!(
            format!("{}", build_plonk_expr(&expr, &mut temp_id_offset)),
            "bus: none, 1 * x + 0 * Unused + 0 * Unused + 0 * x * Unused + 0 = 0
"
        )
    }

    #[test]
    fn constant_and_variables() {
        let x = var("x", 0);
        let y = var("y", 1);
        let expr = -(c(3) - c(2) * x.clone() * y.clone()) + c(1);
        let mut temp_id_offset = 0;

        assert_eq!(
            format!("{}", build_plonk_expr(&expr, &mut temp_id_offset)),
            // tmp_0 = 2 * x
            // tmp_1 = tmp_0 * x
            // tmp_2 = -tmp_1 + 3
            // tmp_3 = -tmp_2
            // tmp_3 + 1 = 0
            "bus: none, 2 * x + 0 * Unused + -1 * tmp_0 + 0 * x * Unused + 0 = 0
bus: none, 0 * tmp_0 + 0 * y + -1 * tmp_1 + 1 * tmp_0 * y + 0 = 0
bus: none, 0 * Unused + -1 * tmp_1 + -1 * tmp_2 + 0 * Unused * tmp_1 + 3 = 0
bus: none, -1 * tmp_2 + 0 * Unused + -1 * tmp_3 + 0 * tmp_2 * Unused + 0 = 0
bus: none, 1 * tmp_3 + 0 * Unused + 0 * Unused + 0 * tmp_3 * Unused + 1 = 0
"
        );
    }

    #[test]
    fn negative_number() {
        let expr = -c(3);
        let mut temp_id_offset = 0;

        assert_eq!(
            format!("{}", build_plonk_expr(&expr, &mut temp_id_offset)),
            // tmp_0 = 3
            // -tmp_0
            "bus: none, 0 * Unused + 0 * Unused + -1 * tmp_0 + 0 * Unused * Unused + 3 = 0
bus: none, -1 * tmp_0 + 0 * Unused + 0 * Unused + 0 * tmp_0 * Unused + 0 = 0
"
        );
    }

    #[test]
    fn negative_var_sub() {
        let x = var("x", 0);
        let y = var("y", 1);
        let expr = x.clone() - (-y.clone());
        let mut temp_id_offset = 0;
        assert_eq!(
            format!("{}", build_plonk_expr(&expr, &mut temp_id_offset)),
            // tmp_0 = -y
            // x - tmp_0 = 0
            "bus: none, -1 * y + 0 * Unused + -1 * tmp_0 + 0 * y * Unused + 0 = 0
bus: none, 1 * x + -1 * tmp_0 + 0 * Unused + 0 * x * tmp_0 + 0 = 0
"
        );
    }
}

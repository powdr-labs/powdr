mod symbolic_builder;
mod symbolic_expression;
mod symbolic_variable;

use core::panic;
use std::{rc::Rc, vec};

use p3_field::Field;
pub use symbolic_builder::*;
use symbolic_expression::SymbolicExpression;
use symbolic_variable::{Entry, SymbolicVariable};

pub fn get_pil<F: Field>(
    name: &str,
    ab: SymbolicAirBuilder<F>,
    columns: Vec<String>,
    public_values: Vec<String>,
) -> String {
    let mut pil = format!(
        "
namespace {name};
    // Preamble
    col fixed is_first_row = [1] + [0]*;
    col fixed is_last_row = [0] + [1]*;
    col fixed is_transition = [0] + [1]* + [0];

    // Bus receives (interaction_id, tuple, multiplicity)
"
    );

    for (interaction_id, values, multiplicity) in ab.bus_receives {
        pil.push_str(&format!(
            "    std::protocols::bus::bus_receive({}, [{}], {});\n",
            format_expr(&interaction_id, &columns, &public_values),
            values
                .iter()
                .map(|value| format_expr(value, &columns, &public_values))
                .collect::<Vec<String>>()
                .join(", "),
            format_expr(&multiplicity, &columns, &public_values)
        ));
    }

    pil.push_str(
        "
    // Bus sends (interaction_id, tuple, multiplicity)
",
    );

    for (interaction_id, values, multiplicity) in ab.bus_sends {
        pil.push_str(&format!(
            "    std::protocols::bus::bus_send({}, [{}], {});\n",
            format_expr(&interaction_id, &columns, &public_values),
            values
                .iter()
                .map(|value| format_expr(value, &columns, &public_values))
                .collect::<Vec<String>>()
                .join(", "),
            format_expr(&multiplicity, &columns, &public_values)
        ));
    }

    pil.push_str(
        "
    // Witness columns
",
    );

    // Declare witness columns
    for column in &columns {
        pil.push_str(&format!("    col witness {column};\n"));
    }

    pil.push_str(
        "
    // Constraints
",
    );

    for constraint in &ab.constraints {
        // println!("{}", format_expr(constraint, &columns));
        pil.push_str(&format!(
            "    {} = 0;\n",
            format_expr(constraint, &columns, &public_values)
        ));
    }
    pil
}

pub fn get_pil2<F: Field>(
    name: &str,
    constraints: Vec<SymbolicExpression<F>>,
    columns: Vec<String>,
) -> String {
    let mut pil = format!(
        "
namespace {name};
    // Preamble
    col fixed ifr = [1] + [0]*;
    col fixed ilr = [0] + [1]*;
    col fixed it = [0] + [1]* + [0];

"
    );

    pil.push_str(
        "
    // Witness columns
",
    );

    // Declare witness columns
    for column in &columns {
        pil.push_str(&format!("    col witness {column};\n"));
    }

    pil.push_str(
        "
    // Constraints
",
    );

    for constraint in &constraints {
        // println!("{}", format_expr(constraint, &columns));
        pil.push_str(&format!(
            "    {} = 0;\n",
            format_expr(constraint, &columns, &[])
        ));
    }
    pil
}

fn format_expr<F: Field>(
    expr: &SymbolicExpression<F>,
    columns: &[String],
    public_values: &[String],
) -> String {
    match expr {
        SymbolicExpression::Variable(SymbolicVariable {
            entry,
            index,
            _phantom,
        }) => {
            let offset_str = |offset| match offset {
                0 => "",
                1 => "'",
                _ => unimplemented!(),
            };
            match entry {
                Entry::Preprocessed { .. } => {
                    unimplemented!()
                }
                Entry::Main { offset } => {
                    let column_name = columns.get(*index).unwrap_or_else(|| {
                        panic!(
                            "Column index out of bounds: {}\nColumns: {:?}",
                            index, columns
                        )
                    });
                    format!("{column_name}{}", offset_str(*offset))
                }
                Entry::Permutation { .. } => unimplemented!(),
                Entry::Public => {
                    let public_value = public_values.get(*index).unwrap_or_else(|| {
                        panic!(
                            "Public value index out of bounds: {}\nPublic values: {:?}",
                            index, public_values
                        )
                    });
                    format!(":{public_value}")
                }
                Entry::Challenge => unimplemented!(),
            }
        }
        SymbolicExpression::IsFirstRow => "ifr".to_string(),
        SymbolicExpression::IsLastRow => "ilr".to_string(),
        SymbolicExpression::IsTransition => "it".to_string(),
        SymbolicExpression::Constant(c) => format!("{}", c),
        SymbolicExpression::Add { x, y, .. } => {
            format!(
                "({}+{})",
                format_expr(x, columns, public_values),
                format_expr(y, columns, public_values)
            )
        }
        SymbolicExpression::Sub { x, y, .. } => {
            format!(
                "({}-{})",
                format_expr(x, columns, public_values),
                format_expr(y, columns, public_values)
            )
        }
        SymbolicExpression::Neg { x, .. } => {
            format!("(-{})", format_expr(x, columns, public_values))
        }
        SymbolicExpression::Mul { x, y, .. } => {
            format!(
                "({}*{})",
                format_expr(x, columns, public_values),
                format_expr(y, columns, public_values)
            )
        }
        SymbolicExpression::Pow {
            base,
            exponent,
            degree_multiple,
        } => {
            format!(
                "({}**{})",
                format_expr(base, columns, public_values),
                exponent,
            )
        }
    }
}

fn optimize_constraints<F: Field>(
    constraints: Vec<SymbolicExpression<F>>,
) -> Vec<SymbolicExpression<F>> {
    constraints
        .into_iter()
        .map(|c| optimize_expression(&c))
        .filter(|c| !is_trivial_constraint(c))
        .collect()
}

fn optimize_expression<F: Field>(expr: &SymbolicExpression<F>) -> SymbolicExpression<F> {
    match expr {
        SymbolicExpression::Mul {
            x,
            y,
            degree_multiple,
        } => {
            let opt_x = optimize_expression(x);
            let opt_y = optimize_expression(y);

            match (&opt_x, &opt_y) {
                // Basic constant folding
                (SymbolicExpression::Constant(c1), SymbolicExpression::Constant(c2)) => {
                    SymbolicExpression::Constant(c1.mul(*c2))
                }
                // Handle multiplication with constants
                (SymbolicExpression::Constant(c), expr)
                | (expr, SymbolicExpression::Constant(c)) => {
                    if c.is_zero() {
                        SymbolicExpression::Constant(F::zero())
                    } else if c.is_one() {
                        expr.clone()
                    } else {
                        SymbolicExpression::Mul {
                            x: Rc::new(opt_x),
                            y: Rc::new(opt_y),
                            degree_multiple: *degree_multiple,
                        }
                    }
                }
                // Optimize multiplication of same expression into power
                (e1, e2) if e1 == e2 => SymbolicExpression::Pow {
                    base: Rc::new(opt_x),
                    exponent: 2,
                    degree_multiple: *degree_multiple,
                },
                // Combine powers with same base
                (
                    SymbolicExpression::Pow {
                        base: b1,
                        exponent: e1,
                        ..
                    },
                    SymbolicExpression::Pow {
                        base: b2,
                        exponent: e2,
                        ..
                    },
                ) if b1 == b2 => SymbolicExpression::Pow {
                    base: b1.clone(),
                    exponent: e1 + e2,
                    degree_multiple: *degree_multiple,
                },
                // Double negation elimination
                (
                    SymbolicExpression::Neg { x: neg_x, .. },
                    SymbolicExpression::Neg { x: neg_y, .. },
                ) => optimize_expression(&SymbolicExpression::Mul {
                    x: neg_x.clone(),
                    y: neg_y.clone(),
                    degree_multiple: *degree_multiple,
                }),
                // (x + y)(x - y) = x² - y²
                (
                    SymbolicExpression::Add { x: x1, y: y1, .. },
                    SymbolicExpression::Sub { x: x2, y: y2, .. },
                ) if x1 == x2 && y1 == y2 => optimize_expression(&SymbolicExpression::Sub {
                    x: Rc::new(SymbolicExpression::Pow {
                        base: x1.clone(),
                        exponent: 2,
                        degree_multiple: *degree_multiple,
                    }),
                    y: Rc::new(SymbolicExpression::Pow {
                        base: y1.clone(),
                        exponent: 2,
                        degree_multiple: *degree_multiple,
                    }),
                    degree_multiple: *degree_multiple,
                }),
                _ => SymbolicExpression::Mul {
                    x: Rc::new(opt_x),
                    y: Rc::new(opt_y),
                    degree_multiple: *degree_multiple,
                },
            }
        }
        SymbolicExpression::Add {
            x,
            y,
            degree_multiple,
        } => {
            let opt_x = optimize_expression(x);
            let opt_y = optimize_expression(y);

            match (&opt_x, &opt_y) {
                // Basic constant folding
                (SymbolicExpression::Constant(c1), SymbolicExpression::Constant(c2)) => {
                    SymbolicExpression::Constant(c1.add(*c2))
                }
                // Same variable addition -> multiplication by 2
                (expr1, expr2) if expr1 == expr2 => SymbolicExpression::Mul {
                    x: Rc::new(expr1.clone()),
                    y: Rc::new(SymbolicExpression::Constant(F::two())),
                    degree_multiple: *degree_multiple,
                },
                // Handle nested additions with constants
                (
                    SymbolicExpression::Add { x: x1, y: y1, .. },
                    SymbolicExpression::Constant(c2),
                ) => {
                    if let SymbolicExpression::Constant(c1) = y1.as_ref() {
                        // Combine constants: (a + c1) + c2 -> a + (c1 + c2)
                        SymbolicExpression::Add {
                            x: x1.clone(),
                            y: Rc::new(SymbolicExpression::Constant(c1.add(*c2))),
                            degree_multiple: *degree_multiple,
                        }
                    } else {
                        // Default case
                        SymbolicExpression::Add {
                            x: Rc::new(opt_x),
                            y: Rc::new(opt_y),
                            degree_multiple: *degree_multiple,
                        }
                    }
                }
                // Handle constant + expression
                (SymbolicExpression::Constant(c), expr) => {
                    if c.is_zero() {
                        expr.clone()
                    } else {
                        // Move constant to the right
                        SymbolicExpression::Add {
                            x: Rc::new(expr.clone()),
                            y: Rc::new(SymbolicExpression::Constant(*c)),
                            degree_multiple: *degree_multiple,
                        }
                    }
                }
                // Rest of existing matches remain the same
                _ => SymbolicExpression::Add {
                    x: Rc::new(opt_x),
                    y: Rc::new(opt_y),
                    degree_multiple: *degree_multiple,
                },
            }
        }
        SymbolicExpression::Sub {
            x,
            y,
            degree_multiple,
        } => {
            let opt_x = optimize_expression(x);
            let opt_y = optimize_expression(y);

            match (&opt_x, &opt_y) {
                (SymbolicExpression::Constant(c1), SymbolicExpression::Constant(c2)) => {
                    SymbolicExpression::Constant(c1.sub(*c2))
                }
                (expr, SymbolicExpression::Constant(c)) => {
                    if c.is_zero() {
                        expr.clone()
                    } else {
                        SymbolicExpression::Sub {
                            x: Rc::new(opt_x),
                            y: Rc::new(opt_y),
                            degree_multiple: *degree_multiple,
                        }
                    }
                }
                (e1, e2) if e1 == e2 => SymbolicExpression::Constant(F::zero()),
                _ => SymbolicExpression::Sub {
                    x: Rc::new(opt_x),
                    y: Rc::new(opt_y),
                    degree_multiple: *degree_multiple,
                },
            }
        }
        SymbolicExpression::Neg { x, degree_multiple } => {
            let opt_x = optimize_expression(x);
            match &opt_x {
                SymbolicExpression::Constant(c) => SymbolicExpression::Constant(c.neg()),
                SymbolicExpression::Neg { x, .. } => (*x).as_ref().clone(),
                _ => SymbolicExpression::Neg {
                    x: Rc::new(opt_x),
                    degree_multiple: *degree_multiple,
                },
            }
        }
        SymbolicExpression::Pow {
            base,
            exponent,
            degree_multiple,
        } => {
            let opt_base = optimize_expression(base);
            match (&opt_base, exponent) {
                (SymbolicExpression::Constant(c), exp) => {
                    SymbolicExpression::Constant(F::exp_u64_generic(*c, *exp as u64))
                }
                (_, 0) => SymbolicExpression::Constant(F::one()),
                (_, 1) => opt_base,
                (
                    SymbolicExpression::Pow {
                        base: inner_base,
                        exponent: inner_exp,
                        ..
                    },
                    exp,
                ) => SymbolicExpression::Pow {
                    base: inner_base.clone(),
                    exponent: inner_exp * exp,
                    degree_multiple: *degree_multiple,
                },
                _ => SymbolicExpression::Pow {
                    base: Rc::new(opt_base),
                    exponent: *exponent,
                    degree_multiple: *degree_multiple,
                },
            }
        }
        // Base cases that don't need optimization
        SymbolicExpression::Variable(_)
        | SymbolicExpression::IsFirstRow
        | SymbolicExpression::IsLastRow
        | SymbolicExpression::IsTransition
        | SymbolicExpression::Constant(_) => expr.clone(),
    }
}
fn is_trivial_constraint<F: Field>(expr: &SymbolicExpression<F>) -> bool {
    match expr {
        SymbolicExpression::Constant(c) => c.is_zero(),
        SymbolicExpression::Mul { x, y, .. } => {
            matches!(x.as_ref(), SymbolicExpression::Constant(c) if c.is_zero())
                || matches!(y.as_ref(), SymbolicExpression::Constant(c) if c.is_zero())
        }
        SymbolicExpression::Add { x, y, .. } => {
            is_trivial_constraint(x) && is_trivial_constraint(y)
        }
        SymbolicExpression::Sub { x, y, .. } => {
            x.as_ref() == y.as_ref() || (is_trivial_constraint(x) && is_trivial_constraint(y))
        }
        SymbolicExpression::Neg { x, .. } => is_trivial_constraint(x),
        _ => false,
    }
}

fn collect_like_terms<F: Field>(
    expr: &SymbolicExpression<F>,
) -> (Vec<Rc<SymbolicExpression<F>>>, F) {
    let mut variables = Vec::new();
    let mut constant_sum = F::zero();

    fn collect_recursive<F: Field>(
        expr: &SymbolicExpression<F>,
        variables: &mut Vec<Rc<SymbolicExpression<F>>>,
        constant_sum: &mut F,
    ) {
        match expr {
            SymbolicExpression::Add { x, y, .. } => {
                collect_recursive(x, variables, constant_sum);
                collect_recursive(y, variables, constant_sum);
            }
            SymbolicExpression::Constant(c) => {
                *constant_sum = constant_sum.add(*c);
            }
            _ => variables.push(Rc::new(expr.clone())),
        }
    }

    collect_recursive(expr, &mut variables, &mut constant_sum);
    (variables, constant_sum)
}

fn try_cancellation<F: Field>(
    expr: &SymbolicExpression<F>,
    neg_term: &SymbolicExpression<F>,
) -> Option<SymbolicExpression<F>> {
    match expr {
        SymbolicExpression::Add { x, y, .. } => {
            if y.as_ref() == neg_term {
                Some((*x).as_ref().clone())
            } else if x.as_ref() == neg_term {
                Some((*y).as_ref().clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use p3_air::Air;
    use p3_baby_bear::BabyBear;
    use p3_keccak_air::KeccakAir;

    #[test]

    fn test_keccak_air_to_pil() {
        // Create KeccakAir instance
        let keccak = KeccakAir {};

        // Create our symbolic builder that implements AirBuilder
        let mut builder = SymbolicAirBuilder::<BabyBear>::new(0, 2633, 0);

        // Use eval to generate actual Keccak constraints
        keccak.eval(&mut builder);

        // Define column names
        let columns = (0..=2634)
            .map(|i| format!("s{}", i))
            .collect::<Vec<String>>();

        // Generate PIL with the actual constraints from eval
        let cs = builder.constraints();
        let opt_cs = optimize_constraints(cs.clone());
        println!(
            "Optimized constraints: {} vs Non-opt {}",
            opt_cs.len(),
            cs.len()
        );
        let pil = get_pil2("KeccakTest", opt_cs, columns);

        std::fs::write("sp1.pil", pil).unwrap();
        println!("PIL written to sp1.pil 1.15");
    }
}

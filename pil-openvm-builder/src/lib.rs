mod symbolic_builder;
mod symbolic_expression;
mod symbolic_variable;

use core::panic;
use std::rc::Rc;

use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_field::Field;
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
        SymbolicExpression::IsFirstRow => "is_first_row".to_string(),
        SymbolicExpression::IsLastRow => "is_last_row".to_string(),
        SymbolicExpression::IsTransition => "is_transition".to_string(),
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
    }
}

pub fn get_asm<F: Field>(
    name: &str,
    constraints: Vec<SymbolicExpression<F>>,
    columns: Vec<String>,
) -> String {
    let mut pil = format!(
        "
machine Keccakf16 with
	latch: final_step,
    call_selectors: sel, {{

    col fixed is_first_row = [1] + [0]*;
    col fixed is_last_row = [0] + [1]*;
    col fixed is_transition = [0] + [1]* + [0];

    let NUM_ROUNDS: int = 24;
    let step_flags: col[NUM_ROUNDS] = std::array::new(NUM_ROUNDS, |i| |row| if row % NUM_ROUNDS == i {{ 1 }} else {{ 0 }} );
    let final_step: expr = step_flags[NUM_ROUNDS - 1];

    operation keccakf16
        w3, w2, w1, w0, 
        w7, w6, w5, w4, 
        w11, w10, w9, w8, 
        w15, w14, w13, w12, 
        w19, w18, w17, w16, 
        w23, w22, w21, w20, 
        w27, w26, w25, w24, 
        w31, w30, w29, w28, 
        w35, w34, w33, w32, 
        w39, w38, w37, w36, 
        w43, w42, w41, w40, 
        w47, w46, w45, w44, 
        w51, w50, w49, w48, 
        w55, w54, w53, w52, 
        w59, w58, w57, w56, 
        w63, w62, w61, w60, 
        w67, w66, w65, w64, 
        w71, w70, w69, w68, 
        w75, w74, w73, w72, 
        w79, w78, w77, w76, 
        w83, w82, w81, w80, 
        w87, w86, w85, w84, 
        w91, w90, w89, w88, 
        w95, w94, w93, w92, 
        w99, w98, w97, w96 
        ->
        w2536, w2535, w2534, w2533,
        w2540, w2539, w2538, w2537,
        w2544, w2543, w2542, w2541,
        w2548, w2547, w2546, w2545,
        w2552, w2551, w2550, w2549,
        w2556, w2555, w2554, w2553,
        w2560, w2559, w2558, w2557,
        w2564, w2563, w2562, w2561,
        w2568, w2567, w2566, w2565,
        w2572, w2571, w2570, w2569,
        w2576, w2575, w2574, w2573,
        w2580, w2579, w2578, w2577,
        w2584, w2583, w2582, w2581,
        w2588, w2587, w2586, w2585,
        w2592, w2591, w2590, w2589,
        w2596, w2595, w2594, w2593,
        w2600, w2599, w2598, w2597,
        w2604, w2603, w2602, w2601,
        w2608, w2607, w2606, w2605,
        w2612, w2611, w2610, w2609,
        w2616, w2615, w2614, w2613,
        w2620, w2619, w2618, w2617,
        w2624, w2623, w2622, w2621,
        w2628, w2627, w2626, w2625,
        w2632, w2631, w2630, w2629;

"
    );

    pil.push_str(
        "
    // Witness columns
",
    );

    // Declare witness columns
    for (_, column) in columns.iter().enumerate() {
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

    pil.push_str("}");

    pil
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
                (SymbolicExpression::Constant(c), expr)
                | (expr, SymbolicExpression::Constant(c)) => {
                    if c.is_zero() {
                        SymbolicExpression::Constant(F::ZERO)
                    } else if c.is_one() {
                        expr.clone()
                    } else {
                        // Try to extract nested constant from multiplication
                        match expr {
                            SymbolicExpression::Mul { x, y, .. } => {
                                if let SymbolicExpression::Constant(c2) = x.as_ref() {
                                    // c * (c2 * expr) -> (c * c2) * expr
                                    SymbolicExpression::Mul {
                                        x: Rc::new(SymbolicExpression::Constant(c.mul(*c2))),
                                        y: y.clone(),
                                        degree_multiple: *degree_multiple,
                                    }
                                } else if let SymbolicExpression::Constant(c2) = y.as_ref() {
                                    // c * (expr * c2) -> (c * c2) * expr
                                    SymbolicExpression::Mul {
                                        x: Rc::new(SymbolicExpression::Constant(c.mul(*c2))),
                                        y: x.clone(),
                                        degree_multiple: *degree_multiple,
                                    }
                                } else {
                                    // No nested constant to combine with
                                    SymbolicExpression::Mul {
                                        x: Rc::new(opt_x),
                                        y: Rc::new(opt_y),
                                        degree_multiple: *degree_multiple,
                                    }
                                }
                            }
                            _ => SymbolicExpression::Mul {
                                x: Rc::new(opt_x),
                                y: Rc::new(opt_y),
                                degree_multiple: *degree_multiple,
                            },
                        }
                    }
                }
                // Double negation elimination
                (
                    SymbolicExpression::Neg { x: neg_x, .. },
                    SymbolicExpression::Neg { x: neg_y, .. },
                ) => optimize_expression(&SymbolicExpression::Mul {
                    x: neg_x.clone(),
                    y: neg_y.clone(),
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
                    x: Rc::new(SymbolicExpression::Constant(F::TWO)),
                    y: Rc::new(expr1.clone()),
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
                    } else if let SymbolicExpression::Constant(c1) = x1.as_ref() {
                        // Combine constants when x1 is constant
                        SymbolicExpression::Add {
                            x: y1.clone(),
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
                // Handle expression + constant
                (expr, SymbolicExpression::Constant(c)) => {
                    if c.is_zero() {
                        expr.clone()
                    } else {
                        SymbolicExpression::Add {
                            x: Rc::new(opt_x),
                            y: Rc::new(opt_y),
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
                //(SymbolicExpression::Constant(c1), SymbolicExpression::Constant(c2)) => {
                //    SymbolicExpression::Constant(c1.sub(*c2))
                //}
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
                (e1, e2) if e1 == e2 => SymbolicExpression::Constant(F::ZERO),
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

#[cfg(test)]
mod tests {
    use super::*;
    use openvm_circuit::{
        arch::testing::{VmChipTestBuilder, BITWISE_OP_LOOKUP_BUS},
        openvm_stark_sdk::p3_baby_bear::BabyBear,
    };
    use openvm_circuit_primitives::bitwise_op_lookup::{
        BitwiseOperationLookupBus, SharedBitwiseOperationLookupChip,
    };
    use openvm_instructions::LocalOpcode;
    use openvm_keccak256_circuit::KeccakVmChip;
    use openvm_keccak256_transpiler::Rv32KeccakOpcode;
    //use p3_baby_bear::BabyBear as P3BabyBear;

    #[test]

    fn test_keccak_air_to_pil() {
        let bitwise_bus = BitwiseOperationLookupBus::new(BITWISE_OP_LOOKUP_BUS);
        let bitwise_chip = SharedBitwiseOperationLookupChip::<8>::new(bitwise_bus);

        let tester = VmChipTestBuilder::<BabyBear>::default();
        let chip = KeccakVmChip::new(
            tester.execution_bus(),
            tester.program_bus(),
            tester.memory_bridge(),
            tester.address_bits(),
            bitwise_chip.clone(),
            Rv32KeccakOpcode::CLASS_OFFSET,
            tester.offline_memory_mutex_arc(),
        );

        // Create our symbolic builder that implements AirBuilder
        let n_columns = 2633;
        let mut builder = SymbolicAirBuilder::<BabyBear>::new(0, n_columns, 0);
        chip.air.eval_keccak_f(&mut builder);

        // Use eval to generate actual Keccak constraints
        //keccak.eval(&mut builder);

        // Define column names
        let columns = (0..=n_columns)
            .map(|i| format!("w{}", i))
            .collect::<Vec<String>>();

        // Generate PIL with the actual constraints from eval
        let cs = builder.constraints();
        let opt_cs = optimize_constraints(cs.clone());

        let pil = get_asm("Keccakf16", opt_cs, columns);

        std::fs::write("openvm_keccak.asm", pil).unwrap();
        println!("PIL written to sp1.pil");
    }
}

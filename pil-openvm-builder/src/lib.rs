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
machine {name} with
	latch: latch,
    call_selectors: sel, {{

    col fixed is_first_row = [1] + [0]*;
    col fixed is_last_row = [0] + [1]*;
    col fixed is_transition = [0] + [1]* + [0];

    col fixed latch = [1]*;

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
    use openvm_circuit::arch::{
        BasicAdapterInterface, DynAdapterInterface, MinimalInstruction, VmAdapterInterface,
        VmCoreAir,
    };
    //use openvm_circuit::openvm_stark_sdk::p3_baby_bear::BabyBearParameters;
    use openvm_circuit::arch::testing::{VmChipTestBuilder, BITWISE_OP_LOOKUP_BUS};
    use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_air::AirBuilder;
    use openvm_circuit::openvm_stark_sdk::p3_baby_bear::BabyBear;
    use openvm_circuit_primitives::bitwise_op_lookup::{
        BitwiseOperationLookupBus, SharedBitwiseOperationLookupChip,
    };
    use openvm_rv32im_circuit::adapters::Rv32LoadStoreAdapterAirInterface;
    //use openvm_keccak256_circuit::KeccakVmChip;
    //use openvm_keccak256_transpiler::Rv32KeccakOpcode;
    use openvm_rv32im_circuit::Rv32AuipcCoreAir;
    use openvm_rv32im_circuit::Rv32AuipcCoreChip;
    //use p3_baby_bear::BabyBear;

    #[test]

    fn test_keccak_air_to_pil() {
        //let bitwise_bus = BitwiseOperationLookupBus::new(BITWISE_OP_LOOKUP_BUS);
        //let bitwise_chip = SharedBitwiseOperationLookupChip::<8>::new(bitwise_bus);

        //let n_columns = 2633;
        let mut builder = SymbolicAirBuilder::<BabyBear>::new(0, 11, 0);

        let bitwise_lu_bus = BitwiseOperationLookupBus::new(10);
        let bitwise_lu_chip = SharedBitwiseOperationLookupChip::new(bitwise_lu_bus);

        //let tester = VmChipTestBuilder::<BabyBear>::default();
        // let chip = KeccakVmChip::new(
        //     tester.execution_bus(),
        //     tester.program_bus(),
        //     tester.memory_bridge(),
        //     tester.address_bits(),
        //     bitwise_chip.clone(),
        //     Rv32KeccakOpcode::CLASS_OFFSET,
        //     tester.offline_memory_mutex_arc(),
        // );

        // let chip = Rv32AuipcChip::new(
        //     Rv32RdWriteAdapterChip::new(
        //         tester.execution_bus(),
        //         tester.program_bus(),
        //         tester.memory_bridge(),
        //     ),
        //     Rv32AuipcCoreChip::new(bitwise_lu_chip.clone()),
        //     tester.offline_memory_mutex_arc(),
        // );

        let chip = Rv32AuipcCoreChip::new(bitwise_lu_chip.clone());

        let from_pc = SymbolicVariable::new(symbolic_variable::Entry::Main { offset: 0 }, 0);
        //chip.air.eval(&mut builder, &[], from_pc);

        // <Rv32AuipcCoreAir as VmCoreAir<
        //     symbolic_builder::SymbolicAirBuilder<BabyBear>,
        //     Rv32LoadStoreAdapterAirInterface<symbolic_builder::SymbolicAirBuilder<BabyBear>>,
        // >>::eval(&chip.air, &mut builder, &[], from_pc);

        //let columns = (0..11).map(|i| format!("w{}", i)).collect::<Vec<String>>();

        // Allocate variables using those names
        //let cols = columns.iter().map(|name| name).collect::<Vec<_>>();
        let cols = builder.main().values;

        <Rv32AuipcCoreAir as VmCoreAir<
            symbolic_builder::SymbolicAirBuilder<BabyBear>,
            DynAdapterInterface<symbolic_expression::SymbolicExpression<BabyBear>>,
        >>::eval(&chip.air, &mut builder, &cols, from_pc);

        // <Rv32AuipcCoreAir as VmCoreAir<
        //     symbolic_builder::SymbolicAirBuilder<BabyBear>,
        //     BasicAdapterInterface<Expr, MinimalInstruction<Expr>, 0, 0, 0, 0>,
        // >>::eval(&chip.air, &mut builder, &[], from_pc);

        // Use eval to generate actual Keccak constraint
        //keccak.eval(&mut builder);

        // Define column names
        let columns = (0..=12).map(|i| format!("w{}", i)).collect::<Vec<String>>();

        // Generate PIL with the actual constraints from eval
        let cs = builder.constraints();
        //let opt_cs = optimize_constraints(cs.clone());

        let pil = get_asm("Rv32Auipc", cs, columns);

        std::fs::write("openvm_rv32auipc.asm", pil).unwrap();
        println!("PIL written to openvm_rv32auipc.asm");
    }
}

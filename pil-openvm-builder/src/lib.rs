mod symbolic_builder;
mod symbolic_expression;
mod symbolic_variable;

use core::panic;

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
                "({} + {})",
                format_expr(x, columns, public_values),
                format_expr(y, columns, public_values)
            )
        }
        SymbolicExpression::Sub { x, y, .. } => {
            format!(
                "({} - {})",
                format_expr(x, columns, public_values),
                format_expr(y, columns, public_values)
            )
        }
        SymbolicExpression::Neg { x, .. } => {
            format!("(-{})", format_expr(x, columns, public_values))
        }
        SymbolicExpression::Mul { x, y, .. } => {
            format!(
                "({} * {})",
                format_expr(x, columns, public_values),
                format_expr(y, columns, public_values)
            )
        }
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
    use p3_baby_bear::BabyBear as P3BabyBear;

    #[test]

    fn test_keccak_air_to_pil() {
        // Create KeccakAir instance

        // air: KeccakVmAir::new(
        //     ExecutionBridge::new(execution_bus, program_bus),
        //     memory_bridge,
        //     bitwise_lookup_chip.bus(),
        //     address_bits,
        //     offset,
        // ),

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
        let mut builder = SymbolicAirBuilder::<BabyBear>::new(0, 2633, 0);
        chip.air.eval_keccak_f(&mut builder);

        // Use eval to generate actual Keccak constraints
        //keccak.eval(&mut builder);

        // Define column names
        let columns = (0..=2634)
            .map(|i| format!("s{}", i))
            .collect::<Vec<String>>();

        // Generate PIL with the actual constraints from eval
        let cs = builder.constraints();
        //let opt_cs = optimize_constraints(cs.clone());
        // println!(
        //     "Optimized constraints: {} vs Non-opt {}",
        //     opt_cs.len(),
        //     cs.len()
        // );
        let pil = get_pil2("KeccakTest", cs, columns);

        std::fs::write("sp1.pil", pil).unwrap();
        println!("PIL written to sp1.pil 1.15");
    }
}

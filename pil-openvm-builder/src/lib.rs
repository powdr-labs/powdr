mod symbolic_builder;
mod symbolic_expression;
mod symbolic_variable;

use core::panic;

use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::{
    self,
    air_builders::symbolic::{
        symbolic_expression::SymbolicExpression as SymbolicExpressionOVM, SymbolicRapBuilder,
    },
    interaction::{InteractionBuilder, InteractionType},
    p3_field::{Field, FieldAlgebra},
};
pub use symbolic_builder::*;
use symbolic_expression::SymbolicExpression;
use symbolic_variable::{Entry, SymbolicVariable};

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
                _ => unimplemented!("Offset = {offset}"),
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
    imports: &[(&str, &str, usize, usize)],
    ab: SymbolicAirBuilder<F>,
    columns: Vec<String>,
    public_values: Vec<String>,
) -> String {
    let machine_name = if imports.len() > 0 {
        format!(
            "{name}({})",
            imports
                .iter()
                .map(|(name, kind, _, _)| format!("{name}:{kind}"))
                .collect::<Vec<String>>()
                .join(",")
        )
    } else {
        name.to_string()
    };

    let mut pil = format!(
        "
machine {machine_name} with
	latch: latch,
    call_selectors: sel, {{

    col fixed is_first_row = [1] + [0]*;
    col fixed is_last_row = [0] + [1]*;
    col fixed is_transition = [0] + [1]* + [0];

    col fixed latch = [1]*;
"
    );

    for (_, _, index, size) in imports {
        for (n, interaction) in ab
            .bus_interactions
            .iter()
            .filter(|i| (i.bus_index == *index) && (i.interaction_type == InteractionType::Receive))
            .enumerate()
        {
            pil.push_str(&format!("    col witness r{n};\n"));
            let vals = interaction
                .fields
                .iter()
                .take(*size)
                .map(|value| format_expr(&value.clone().into(), &columns, &[]))
                .collect::<Vec<String>>()
                .join(", ");
            pil.push_str(&format!("    OPERATION WITH {vals}\n"));
        }
    }

    for (_, _, index, size) in imports {
        for (n, interaction) in ab
            .bus_interactions
            .iter()
            .filter(|i| (i.bus_index == *index) && (i.interaction_type == InteractionType::Send))
            .enumerate()
        {
            pil.push_str(&format!("    col witness r{n};\n"));
            let vals = interaction
                .fields
                .iter()
                .take(*size)
                .map(|value| format_expr(&value.clone().into(), &columns, &[]))
                .collect::<Vec<String>>()
                .join(", ");
            pil.push_str(&format!("    link => r{n} = byte.run({vals});\n"));
        }
    }

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

    for constraint in &ab.constraints {
        pil.push_str(&format!(
            "    {} = 0;\n",
            format_expr(constraint, &columns, &[])
        ));
    }

    pil.push_str("}");

    pil
}

pub fn get_bus_asm<F: Field>(
    name: &str,
    bus_builder: SymbolicRapBuilder<F>,
    bus_index: usize,
    num_lookup_cols: usize,
) -> String {
    let mut pil = format!(
        "

use std::convert::int;
use std::utils::cross_product;

machine {name} with
    latch: latch,
    degree: 65536,
    operation_id: operation_id,
    call_selectors: sel, {{

    col fixed is_first_row = [1] + [0]*;
    col fixed is_last_row = [0] + [1]*;
    col fixed is_transition = [0] + [1]* + [0];

    let operation_id;
    col fixed latch = [1]*;
    
"
    );

    let all_sends = bus_builder
        .all_interactions()
        .iter()
        .filter(|i| i.interaction_type == InteractionType::Send);

    let all_receives = bus_builder
        .all_interactions()
        .iter()
        .filter(|i| i.interaction_type == InteractionType::Receive);

    let columns = ["p1", "p2", "p3"] //Hardcoded columns in this case
        .iter()
        .map(|i| i.to_string())
        .collect::<Vec<String>>();
    for (n, interaction) in all_receives
        .filter(|i| (i.bus_index == bus_index) && (i.interaction_type == InteractionType::Receive))
        .take(1) // Take first to avoid range_check op
        .enumerate()
    {
        //pil.push_str(&format!("    col witness r{n};\n"));
        let vals = interaction
            .fields
            .iter()
            .take(num_lookup_cols)
            .map(|value| format_expr(&value.clone().into(), &columns, &[]))
            .collect::<Vec<String>>()
            .join(", ");
        pil.push_str(&format!("    operation run<0> {vals} -> p3 \n"));
    }

    pil.push_str(&format!(
        "
    let bit_counts = [256, 256];
    let min_degree = std::array::product(bit_counts);
    let inputs: (int -> int)[] = cross_product(bit_counts);
    let a = inputs[0];
    let b = inputs[1];
    let p1: col = a;
    let p2: col = b;
    let p3: col = |i| {{
        a(i) ^ b(i)
    }};
    "
    ));

    for (n, interaction) in all_sends.enumerate() {
        let vals = interaction
            .fields
            .iter()
            .take(num_lookup_cols)
            .map(|value| format_expr(&value.clone().into(), &columns, &[]))
            .collect::<Vec<String>>()
            .join(", ");
        pil.push_str(&format!("    link => r{n} = byte.run({vals});\n"));
    }

    pil.push_str("\n}");

    pil
}

#[cfg(test)]
mod tests {
    use super::*;
    use openvm_circuit::arch::testing::{VmChipTestBuilder, BITWISE_OP_LOOKUP_BUS};
    use openvm_circuit::arch::{DynAdapterInterface, VmCoreAir};
    use openvm_circuit::openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Config;
    use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::air_builders::symbolic::{
        get_symbolic_builder, SymbolicRapBuilder,
    };
    use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::interaction::RapPhaseSeqKind;
    use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::keygen::types::TraceWidth;
    use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::p3_air::AirBuilder;
    use openvm_circuit::openvm_stark_sdk::openvm_stark_backend::Chip;
    use openvm_circuit::openvm_stark_sdk::p3_baby_bear::BabyBear;
    use openvm_circuit_primitives::bitwise_op_lookup::{
        BitwiseOperationLookupAir, BitwiseOperationLookupBus, SharedBitwiseOperationLookupChip,
        NUM_BITWISE_OP_LOOKUP_COLS, NUM_BITWISE_OP_LOOKUP_PREPROCESSED_COLS,
    };

    use openvm_instructions::riscv::RV32_CELL_BITS;
    use openvm_rv32im_circuit::adapters::Rv32RdWriteAdapterChip;
    use openvm_rv32im_circuit::Rv32AuipcCoreChip;
    use openvm_rv32im_circuit::{Rv32AuipcChip, Rv32AuipcCoreAir};

    #[test]

    fn test_rv32auipc_air_to_pil() {
        // Rv32AuipcCoreChip
        let mut builder = SymbolicAirBuilder::<BabyBear>::new(0, 11, 0);

        let bitwise_lu_bus = BitwiseOperationLookupBus::new(BITWISE_OP_LOOKUP_BUS);
        let bitwise_lu_chip =
            SharedBitwiseOperationLookupChip::<RV32_CELL_BITS>::new(bitwise_lu_bus);

        let tester = VmChipTestBuilder::default();
        let adapter = Rv32RdWriteAdapterChip::<BabyBear>::new(
            tester.execution_bus(),
            tester.program_bus(),
            tester.memory_bridge(),
        );
        let core = Rv32AuipcCoreChip::new(bitwise_lu_chip.clone());
        let chip = Rv32AuipcChip::<BabyBear>::new(adapter, core, tester.offline_memory_mutex_arc());
        let from_pc = SymbolicVariable::new(symbolic_variable::Entry::Main { offset: 0 }, 11);
        let cols = builder.main().values;

        <Rv32AuipcCoreAir as VmCoreAir<
            symbolic_builder::SymbolicAirBuilder<BabyBear>,
            DynAdapterInterface<symbolic_expression::SymbolicExpression<BabyBear>>,
        >>::eval(&chip.core.air, &mut builder, &cols, from_pc);

        let columns = (0..=11).map(|i| format!("w{}", i)).collect::<Vec<String>>();
        let imports = vec![(
            "byte",
            "BitwiseOperationLookupBus",
            BITWISE_OP_LOOKUP_BUS,
            NUM_BITWISE_OP_LOOKUP_COLS,
        )];
        let pil = get_asm("Rv32Auipc", &imports, builder, columns.clone(), vec![]);

        // Bus
        let binding = Chip::<BabyBearPoseidon2Config>::air(&bitwise_lu_chip);
        let air = binding
            .as_any()
            .downcast_ref::<BitwiseOperationLookupAir<RV32_CELL_BITS>>()
            .expect("Failed to downcast to BitwiseOperationLookupAir");

        let width = TraceWidth {
            preprocessed: Some(NUM_BITWISE_OP_LOOKUP_PREPROCESSED_COLS),
            cached_mains: vec![],
            common_main: NUM_BITWISE_OP_LOOKUP_COLS,
            after_challenge: vec![],
        };

        // get_symbolic_builder returns a builder that was already evaluated using the air passed as parameter
        let bus_builder: SymbolicRapBuilder<_> =
            get_symbolic_builder(air, &width, &[], &[], RapPhaseSeqKind::FriLogUp, 2);

        let asm_bus = get_bus_asm::<BabyBear>(
            "BitwiseOperationLookupBus",
            bus_builder,
            BITWISE_OP_LOOKUP_BUS,
            NUM_BITWISE_OP_LOOKUP_COLS,
        );

        let main = "machine Main with degree: 32 {
  BitwiseOperationLookupBus byte;
	Rv32Auipc r(byte, 32, 32);
}"
        .to_string();

        let asm = main + "\n" + &pil + "\n" + &asm_bus;
        std::fs::write("openvm_rv32auipc.asm", asm).unwrap();

        println!("PIL written to openvm_rv32auipc.asm");
    }
}

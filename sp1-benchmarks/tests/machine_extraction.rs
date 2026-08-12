use std::{collections::BTreeMap, fs, path::Path};

use enum_map::Enum;
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use pretty_assertions::assert_eq;
use sp1_core_executor::{Instruction, Opcode};
use sp1_core_machine::autoprecompiles::{
    bus_map::sp1_bus_map,
    instruction::Sp1Instruction,
    instruction_handler::{try_instruction_type_to_air_id, InstructionType, Sp1InstructionHandler},
};
use sp1_primitives::SP1Field;

/// Snapshots the base SP1 instruction AIRs (before any autoprecompile synthesis).
///
/// This is the SP1 analog of `openvm-riscv/tests/openvm_constraints.txt`: it renders
/// every unique instruction machine the [`Sp1InstructionHandler`] holds, keyed by its
/// `RiscvAirId`.
#[test]
fn extract_machines() {
    let handler = Sp1InstructionHandler::<SP1Field>::new();

    // Collect the unique instruction AIRs, keyed by their index in the handler (which is
    // stable across runs), so that opcodes sharing an AIR (e.g. XOR/OR/AND) render once.
    let mut machines: BTreeMap<usize, (&'static str, &SymbolicMachine<SP1Field>)> = BTreeMap::new();

    let mut record = |instruction_type: InstructionType, instruction: Instruction| {
        let Some(air_id) = try_instruction_type_to_air_id(instruction_type) else {
            // Not an instruction AIR (e.g. ECALL, EBREAK, UNIMP).
            return;
        };
        let (idx, (machine, _stats)) = handler
            .get_instruction_air_and_stats(&Sp1Instruction::from(instruction))
            .expect("instruction type maps to an AIR");
        machines.entry(idx).or_insert((air_id.into(), machine));
    };

    for i in 0..<Opcode as Enum>::LENGTH {
        let opcode = <Opcode as Enum>::from_usize(i);
        // `op_a = 1` (i.e. not x0) so that loads map to their regular AIR rather than LoadX0.
        record(
            InstructionType::NonLoadX0(opcode),
            Instruction::new(opcode, 1, 0, 0, false, false),
        );
    }
    // A load with `op_a = 0` (x0) is handled by the dedicated LoadX0 AIR.
    record(
        InstructionType::LoadX0,
        Instruction::new(Opcode::LD, 0, 0, 0, false, true),
    );

    let bus_map = sp1_bus_map();
    let rendered = machines
        .values()
        .map(|(name, machine)| format!("# {name}\n{}", machine.render(&bus_map)))
        .collect::<Vec<_>>()
        .join("\n\n\n");

    let expected_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("sp1_constraints.txt");

    let should_update_expectation = std::env::var("UPDATE_EXPECT")
        .map(|v| v == "1")
        .unwrap_or(false);

    match fs::read_to_string(&expected_path) {
        Ok(expected) if expected == rendered => {
            // Test succeeded.
        }
        Ok(expected) if !should_update_expectation => {
            assert_eq!(
                expected,
                rendered,
                "The extracted SP1 constraints do not match the snapshot. \
                 To overwrite it with the currently generated output, re-run with \
                 the environment variable `UPDATE_EXPECT=1` or delete the file \
                 `{}`.",
                expected_path.display(),
            );
        }
        _ => {
            fs::write(&expected_path, &rendered).unwrap();
            panic!(
                "Wrote SP1 constraints snapshot at {}. Inspect it, then rerun the test.",
                expected_path.display()
            );
        }
    }
}

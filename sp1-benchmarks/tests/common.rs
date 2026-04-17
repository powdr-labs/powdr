use std::{fs, path::Path};

use powdr_autoprecompiles::{
    blocks::{BasicBlock, SuperBlock},
    build,
    empirical_constraints::EmpiricalConstraints,
    evaluation::evaluate_apc,
    export::ExportOptions,
    InstructionHandler as _,
};
use pretty_assertions::assert_eq;
use sp1_core_executor::Instruction;
use sp1_core_machine::autoprecompiles::{
    adapter::Sp1ApcAdapter, bus_map::sp1_bus_map, instruction_handler::Sp1InstructionHandler,
    sp1_vm_config,
};
use sp1_primitives::SP1Field;

pub fn assert_machine_output(basic_block: Vec<Instruction>, module_name: &str, test_name: &str) {
    let instruction_handler = Sp1InstructionHandler::<SP1Field>::new();
    let vm_config = sp1_vm_config(&instruction_handler);
    let block: SuperBlock<_> = BasicBlock {
        start_pc: 0,
        instructions: basic_block.iter().cloned().map(Into::into).collect(),
    }
    .into();

    let apc = build::<Sp1ApcAdapter>(
        block,
        vm_config,
        instruction_handler.degree_bound(),
        ExportOptions::default(),
        &EmpiricalConstraints::default(),
    )
    .unwrap();

    let basic_block_str = basic_block
        .iter()
        .enumerate()
        .map(|(i, inst)| format!("  {i:>3}: {inst:?}"))
        .collect::<Vec<_>>()
        .join("\n");
    let apc_with_stats = evaluate_apc::<Sp1ApcAdapter>(&instruction_handler, apc);
    let actual = format!(
        "Instructions:\n{basic_block_str}\n\n{}\n\n{}",
        apc_with_stats.evaluation_result(),
        apc_with_stats.apc().machine.render(&sp1_bus_map())
    );

    let expected_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("apc_snapshots")
        .join(module_name)
        .join(format!("{test_name}.txt"));
    let expected = fs::read_to_string(&expected_path).unwrap();

    assert_eq!(
        expected.trim(),
        actual.trim(),
        "The output of `{test_name}` does not match {}",
        expected_path.display()
    );
}

use powdr_autoprecompiles::{
    build,
    symbolic_instruction_builder::{add, blt, loadw, storew},
    DegreeBound, SymbolicInstructionStatement, SymbolicMachine, VmConfig,
};
use powdr_number::BabyBearField;
use powdr_openvm::{bus_interaction_handler::OpenVmBusInteractionHandler, BusMap};
use powdr_openvm::{OPENVM_DEGREE_BOUND, POWDR_OPCODE};
use std::collections::BTreeMap;

fn build_airs(airs_path: String) -> BTreeMap<usize, SymbolicMachine<BabyBearField>> {
    let file = std::fs::File::open(airs_path).unwrap();
    let reader = std::io::BufReader::new(file);
    serde_cbor::from_reader(reader).unwrap()
}

fn compile(
    program: Vec<SymbolicInstructionStatement<BabyBearField>>,
) -> (SymbolicMachine<BabyBearField>, Vec<Vec<u64>>) {
    // Build VmConfig with an airs.cbor that should contain map from all possible opcodes to symbolic machines.
    let vm_config = VmConfig {
        instruction_machines: &build_airs("tests/airs.cbor".to_string()),
        bus_interaction_handler: OpenVmBusInteractionHandler::<BabyBearField>::new(
            BusMap::openvm_base(),
        ),
    };

    let degree_bound = DegreeBound {
        identities: OPENVM_DEGREE_BOUND,
        bus_interactions: OPENVM_DEGREE_BOUND - 1,
    };

    build(program, vm_config, degree_bound, POWDR_OPCODE as u32).unwrap()
}

#[test]
fn simple_program() {
    // ADD (rd_ptr = 8, rs1_ptr = 8, rs2 = 16777200, rs2_as = 0)
    // 512 8 8 16777200 1 0 0 0

    // ADD (rd_ptr = 40, rs1_ptr = 0, rs2 = 1024, rs2_as = 0)
    // 512 40 0 1024 1 0 0 0

    // STOREW (rd_rs2_ptr = 40, rs1_ptr = 8, imm = 12, mem_as = 2, needs_write = 0, imm_sign = 0)
    // 531 40 8 12 1 2 0 0

    // ADD (rd_ptr = 40, rs1_ptr = 8, rs2 = 12, rs2_as = 0)
    // 512 40 8 12 1 0 0 0

    // LOADW (rd_rs2_ptr = 44, rs1_ptr = 8, imm = 12, mem_as = 2, needs_write = 0, imm_sign = 0)
    // 528 44 8 12 1 2 0 0

    // ADD (rd_ptr = 40, rs1_ptr = 0, rs2 = 2, rs2_as = 0)
    // 512 40 0 2 1 0 0 0

    // BLT (rs1_ptr = 44, ps2_ptr = 40, immediate = 56)
    // 549 44 40 56 1 1 0 0

    let program = vec![
        add(8, 8, 16777200, 0),
        add(40, 0, 1024, 0),
        storew(40, 8, 12, 2, 0, 0),
        add(40, 8, 12, 0),
        loadw(44, 8, 12, 2, 0, 0),
        add(40, 0, 2, 0),
        blt(44, 40, 56, 1, 1),
    ];

    let (machine, _) = compile(program);

    println!("machine: ");
    println!("{}", machine.to_string());

    // assert_eq!(machine.to_string(), "<TODO>");
}

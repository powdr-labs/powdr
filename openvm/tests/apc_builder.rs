use openvm_instructions::VmOpcode;
use openvm_sdk::config::SdkVmConfig;
use powdr_autoprecompiles::openvm::ALL_OPCODES;
use powdr_autoprecompiles::{
    build, openvm::default_openvm_bus_map, symbolic_instruction_builder::prelude::*, DegreeBound,
    SymbolicInstructionStatement, SymbolicMachine, VmConfig,
};
use powdr_number::BabyBearField;
use powdr_openvm::bus_interaction_handler::OpenVmBusInteractionHandler;
use powdr_openvm::{instructions_to_airs, OPENVM_DEGREE_BOUND, POWDR_OPCODE};
use std::collections::HashSet;

fn compile(
    program: Vec<SymbolicInstructionStatement<BabyBearField>>,
) -> (SymbolicMachine<BabyBearField>, Vec<Vec<u64>>) {
    let sdk_vm_config = SdkVmConfig::builder()
        .system(Default::default())
        .rv32i(Default::default())
        .rv32m(Default::default())
        .io(Default::default())
        .keccak(Default::default())
        .build();

    let all_instructions = ALL_OPCODES
        .iter()
        .map(|&opcode| VmOpcode::from_usize(opcode as usize))
        .collect::<HashSet<_>>();

    let airs = instructions_to_airs(sdk_vm_config, &all_instructions);

    // Build VmConfig with an airs.cbor that should contain map from all possible opcodes to symbolic machines.
    let vm_config = VmConfig {
        instruction_machines: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::<BabyBearField>::new(
            default_openvm_bus_map(),
        ),
        bus_map: default_openvm_bus_map(),
    };

    let degree_bound = DegreeBound {
        identities: OPENVM_DEGREE_BOUND,
        bus_interactions: OPENVM_DEGREE_BOUND - 1,
    };

    build(program, vm_config, degree_bound, POWDR_OPCODE as u32).unwrap()
}

#[test]
fn guest_top_block() {
    // Top block from `guest` with `--pgo cell`, with 4 instructions:
    // SymbolicInstructionStatement { opcode: 512, args: [BabyBearField(8), BabyBearField(8), BabyBearField(16777200), BabyBearField(1), BabyBearField(0), BabyBearField(0), BabyBearField(0)] }
    // SymbolicInstructionStatement { opcode: 531, args: [BabyBearField(4), BabyBearField(8), BabyBearField(12), BabyBearField(1), BabyBearField(2), BabyBearField(1), BabyBearField(0)] }
    // SymbolicInstructionStatement { opcode: 576, args: [BabyBearField(4), BabyBearField(0), BabyBearField(0), BabyBearField(1), BabyBearField(0), BabyBearField(0), BabyBearField(0)] }
    // SymbolicInstructionStatement { opcode: 565, args: [BabyBearField(4), BabyBearField(4), BabyBearField(1780), BabyBearField(1), BabyBearField(0), BabyBearField(1), BabyBearField(0)] }

    let program = vec![
        add(8, 8, 16777200, 0),
        storew(4, 8, 12, 2, 1, 0),
        auipc(4, 0, 0, 1, 0),
        blt(4, 4, 1780, 1, 0),
    ];

    let (machine, _) = compile(program);

    let expected = r#"(7864320 * rs1_data__0_1 + 125829121 * is_valid - 7864320 * writes_aux__prev_data__0_0) * (7864320 * rs1_data__0_1 + 125829120 - 7864320 * writes_aux__prev_data__0_0)
(30720 * rs1_data__0_1 + 7864320 * rs1_data__1_1 + 491521 * is_valid - (30720 * writes_aux__prev_data__0_0 + 7864320 * writes_aux__prev_data__1_0)) * (30720 * rs1_data__0_1 + 7864320 * rs1_data__1_1 + 491520 - (30720 * writes_aux__prev_data__0_0 + 7864320 * writes_aux__prev_data__1_0))
(120 * rs1_data__0_1 + 30720 * rs1_data__1_1 + 7864320 * rs1_data__2_1 + 1921 * is_valid - (120 * writes_aux__prev_data__0_0 + 30720 * writes_aux__prev_data__1_0 + 7864320 * writes_aux__prev_data__2_0)) * (120 * rs1_data__0_1 + 30720 * rs1_data__1_1 + 7864320 * rs1_data__2_1 + 1920 - (120 * writes_aux__prev_data__0_0 + 30720 * writes_aux__prev_data__1_0 + 7864320 * writes_aux__prev_data__2_0))
(943718400 * writes_aux__prev_data__0_0 + 120 * rs1_data__1_1 + 30720 * rs1_data__2_1 + 7864320 * rs1_data__3_1 - (120 * writes_aux__prev_data__1_0 + 30720 * writes_aux__prev_data__2_0 + 7864320 * writes_aux__prev_data__3_0 + 943718400 * rs1_data__0_1 + 1006632952 * is_valid)) * (943718400 * writes_aux__prev_data__0_0 + 120 * rs1_data__1_1 + 30720 * rs1_data__2_1 + 7864320 * rs1_data__3_1 - (120 * writes_aux__prev_data__1_0 + 30720 * writes_aux__prev_data__2_0 + 7864320 * writes_aux__prev_data__3_0 + 943718400 * rs1_data__0_1 + 1006632953))
(30720 * mem_ptr_limbs__0_1 - (30720 * rs1_data__0_1 + 7864320 * rs1_data__1_1 + 368640 * is_valid)) * (30720 * mem_ptr_limbs__0_1 - (30720 * rs1_data__0_1 + 7864320 * rs1_data__1_1 + 368641))
(943718400 * rs1_data__0_1 + 30720 * mem_ptr_limbs__1_1 - (120 * rs1_data__1_1 + 943718400 * mem_ptr_limbs__0_1 + 30720 * rs1_data__2_1 + 7864320 * rs1_data__3_1 + 754974726 * is_valid)) * (943718400 * rs1_data__0_1 + 30720 * mem_ptr_limbs__1_1 - (120 * rs1_data__1_1 + 943718400 * mem_ptr_limbs__0_1 + 30720 * rs1_data__2_1 + 7864320 * rs1_data__3_1 + 754974727))
(7864320 * a__1_3 - 7864320 * pc_limbs__0_2) * (7864320 * a__1_3 - (7864320 * pc_limbs__0_2 + 1))
(7864320 * a__2_3 + 30720 * a__1_3 - (30720 * pc_limbs__0_2 + 7864320 * pc_limbs__1_2)) * (7864320 * a__2_3 + 30720 * a__1_3 - (30720 * pc_limbs__0_2 + 7864320 * pc_limbs__1_2 + 1))
(943718400 * from_state__pc_0 + 7864320 * b__3_3 + 30720 * a__2_3 + 120 * a__1_3 - (943718400 * a__0_3 + 503316484 * is_valid)) * (943718400 * from_state__pc_0 + 7864320 * b__3_3 + 30720 * a__2_3 + 120 * a__1_3 - (943718400 * a__0_3 + 503316485))
cmp_lt_3 * (cmp_lt_3 - 1)
(b__3_3 - a_msb_f_3) * (a_msb_f_3 + 256 - b__3_3)
(b__3_3 - b_msb_f_3) * (b_msb_f_3 + 256 - b__3_3)
diff_marker__3_3 * (diff_marker__3_3 - 1)
(1 - diff_marker__3_3) * ((b_msb_f_3 - a_msb_f_3) * (2 * cmp_lt_3 - 1))
diff_marker__3_3 * ((a_msb_f_3 - b_msb_f_3) * (2 * cmp_lt_3 - 1) + diff_val_3)
diff_marker__2_3 * (diff_marker__2_3 - 1)
diff_marker__2_3 * diff_val_3
diff_marker__1_3 * (diff_marker__1_3 - 1)
diff_marker__1_3 * diff_val_3
diff_marker__0_3 * (diff_marker__0_3 - 1)
diff_marker__0_3 * diff_val_3
(diff_marker__3_3 + diff_marker__2_3 + diff_marker__1_3 + diff_marker__0_3) * (diff_marker__3_3 + diff_marker__2_3 + diff_marker__1_3 + diff_marker__0_3 - 1)
(1 - (diff_marker__3_3 + diff_marker__2_3 + diff_marker__1_3 + diff_marker__0_3)) * cmp_lt_3
(1 - is_valid) * (diff_marker__3_3 + diff_marker__2_3 + diff_marker__1_3 + diff_marker__0_3)
is_valid * (is_valid - 1)
(id=3, mult=is_valid * 1, args=[reads_aux__0__base__timestamp_lt_aux__lower_decomp__0_0, 17])
(id=3, mult=is_valid * 1, args=[reads_aux__0__base__timestamp_lt_aux__lower_decomp__1_0, 12])
(id=1, mult=is_valid * 2013265920, args=[1, 8, writes_aux__prev_data__0_0, writes_aux__prev_data__1_0, writes_aux__prev_data__2_0, writes_aux__prev_data__3_0, reads_aux__1__base__prev_timestamp_3 + reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3 + 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3 - (reads_aux__0__base__timestamp_lt_aux__lower_decomp__0_0 + 131072 * reads_aux__0__base__timestamp_lt_aux__lower_decomp__1_0 + 8)])
(id=0, mult=-is_valid, args=[from_state__pc_0, reads_aux__1__base__prev_timestamp_3 + reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3 + 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3 - 7])
(id=1, mult=is_valid * 1, args=[1, 8, rs1_data__0_1, rs1_data__1_1, rs1_data__2_1, rs1_data__3_1, reads_aux__1__base__prev_timestamp_3 + reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3 + 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3 - 4])
(id=3, mult=is_valid * 1, args=[-(503316480 * mem_ptr_limbs__0_1), 14])
(id=3, mult=is_valid * 1, args=[mem_ptr_limbs__1_1, 13])
(id=3, mult=is_valid * 1, args=[read_data_aux__base__timestamp_lt_aux__lower_decomp__0_1, 17])
(id=3, mult=is_valid * 1, args=[read_data_aux__base__timestamp_lt_aux__lower_decomp__1_1, 12])
(id=1, mult=is_valid * 2013265920, args=[1, 4, rd_aux_cols__prev_data__0_2, rd_aux_cols__prev_data__1_2, rd_aux_cols__prev_data__2_2, rd_aux_cols__prev_data__3_2, reads_aux__1__base__prev_timestamp_3 + reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3 + 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3 - (read_data_aux__base__timestamp_lt_aux__lower_decomp__0_1 + 131072 * read_data_aux__base__timestamp_lt_aux__lower_decomp__1_1 + 4)])
(id=3, mult=is_valid * 1, args=[write_base_aux__timestamp_lt_aux__lower_decomp__0_1, 17])
(id=3, mult=is_valid * 1, args=[write_base_aux__timestamp_lt_aux__lower_decomp__1_1, 12])
(id=1, mult=is_valid * 2013265920, args=[2, mem_ptr_limbs__0_1 + 65536 * mem_ptr_limbs__1_1, prev_data__0_1, prev_data__1_1, prev_data__2_1, prev_data__3_1, reads_aux__1__base__prev_timestamp_3 + reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3 + 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3 - (write_base_aux__timestamp_lt_aux__lower_decomp__0_1 + 131072 * write_base_aux__timestamp_lt_aux__lower_decomp__1_1 + 3)])
(id=1, mult=is_valid * 1, args=[2, mem_ptr_limbs__0_1 + 65536 * mem_ptr_limbs__1_1, rd_aux_cols__prev_data__0_2, rd_aux_cols__prev_data__1_2, rd_aux_cols__prev_data__2_2, rd_aux_cols__prev_data__3_2, reads_aux__1__base__prev_timestamp_3 + reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3 + 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3 - 2])
(id=6, mult=diff_marker__3_3 + diff_marker__2_3 + diff_marker__1_3 + diff_marker__0_3, args=[diff_val_3 - 1, 0, 0, 0])
(id=3, mult=is_valid * 1, args=[reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3, 17])
(id=3, mult=is_valid * 1, args=[reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3, 12])
(id=1, mult=is_valid * 1, args=[1, 4, a__0_3, a__1_3, a__2_3, b__3_3, reads_aux__1__base__prev_timestamp_3 + reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3 + 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3 + 1])
(id=2, mult=is_valid, args=[from_state__pc_0, 4351, 0, 0, 0, 0, 0, 0, 0])
(id=0, mult=is_valid, args=[from_state__pc_0 + 1776 * cmp_lt_3 + 16, reads_aux__1__base__prev_timestamp_3 + reads_aux__1__base__timestamp_lt_aux__lower_decomp__0_3 + 131072 * reads_aux__1__base__timestamp_lt_aux__lower_decomp__1_3 + 2])
(id=6, mult=is_valid * 1, args=[rs1_data__0_1, rs1_data__1_1, 0, 0])
(id=6, mult=is_valid * 1, args=[rs1_data__2_1, rs1_data__3_1, 0, 0])
(id=6, mult=is_valid * 1, args=[a__0_3, a__1_3, 0, 0])
(id=6, mult=is_valid * 1, args=[a__2_3, b__3_3, 0, 0])
(id=6, mult=is_valid * 1, args=[pc_limbs__0_2, pc_limbs__1_2, 0, 0])
(id=6, mult=is_valid * 1, args=[122880 * pc_limbs__0_2 + 31457280 * pc_limbs__1_2 + 480 * a__0_3 - (480 * from_state__pc_0 + 3840), a_msb_f_3 + 128, 0, 0])
(id=6, mult=is_valid * 1, args=[b_msb_f_3 + 128, 0, 0, 0])
"#;

    assert_eq!(machine.to_string(), expected);
}

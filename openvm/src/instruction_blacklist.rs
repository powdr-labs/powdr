use std::collections::HashSet;

use openvm_algebra_transpiler::{Fp2Opcode, Rv32ModularArithmeticOpcode};
use openvm_ecc_transpiler::Rv32WeierstrassOpcode;
use openvm_instructions::{LocalOpcode, SystemOpcode};
use openvm_keccak256_transpiler::Rv32KeccakOpcode;
use openvm_rv32im_transpiler::{Rv32HintStoreOpcode, Rv32LoadStoreOpcode};
use openvm_sha256_transpiler::Rv32Sha256Opcode;

/// The following opcodes shall never be accelerated and therefore always put in its own basic block.
/// Currently this contains OpenVm opcodes: Rv32HintStoreOpcode::HINT_STOREW (0x260) and Rv32HintStoreOpcode::HINT_BUFFER (0x261)
/// which are the only two opcodes from the Rv32HintStore, the air responsible for reading host states via stdin.
/// We don't want these opcodes because they create air constraints with next references, which powdr-openvm does not support yet.
pub fn instruction_blacklist() -> HashSet<usize> {
    [
        Rv32HintStoreOpcode::HINT_STOREW.global_opcode().as_usize(), // contain next references that don't work with apc
        Rv32HintStoreOpcode::HINT_BUFFER.global_opcode().as_usize(), // contain next references that don't work with apc
        Rv32LoadStoreOpcode::LOADB.global_opcode().as_usize(),
        Rv32LoadStoreOpcode::LOADH.global_opcode().as_usize(),
        Rv32WeierstrassOpcode::EC_ADD_NE.global_opcode().as_usize(),
        Rv32WeierstrassOpcode::SETUP_EC_ADD_NE
            .global_opcode()
            .as_usize(),
        Rv32WeierstrassOpcode::EC_DOUBLE.global_opcode().as_usize(),
        Rv32WeierstrassOpcode::SETUP_EC_DOUBLE
            .global_opcode()
            .as_usize(),
        Rv32WeierstrassOpcode::EC_ADD_NE.global_opcode().as_usize() + 4,
        Rv32WeierstrassOpcode::SETUP_EC_ADD_NE
            .global_opcode()
            .as_usize()
            + 4,
        Rv32WeierstrassOpcode::EC_DOUBLE.global_opcode().as_usize() + 4,
        Rv32WeierstrassOpcode::SETUP_EC_DOUBLE
            .global_opcode()
            .as_usize()
            + 4,
        Rv32KeccakOpcode::KECCAK256.global_opcode().as_usize(),
        Rv32Sha256Opcode::SHA256.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::ADD.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::SUB.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::SETUP_ADDSUB
            .global_opcode()
            .as_usize(),
        Rv32ModularArithmeticOpcode::MUL.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::DIV.global_opcode().as_usize(),
        Rv32ModularArithmeticOpcode::SETUP_MULDIV
            .global_opcode()
            .as_usize(),
        Rv32ModularArithmeticOpcode::IS_EQ
            .global_opcode()
            .as_usize(),
        Rv32ModularArithmeticOpcode::SETUP_ISEQ
            .global_opcode()
            .as_usize(),
        Fp2Opcode::ADD.global_opcode().as_usize(),
        Fp2Opcode::SUB.global_opcode().as_usize(),
        Fp2Opcode::SETUP_ADDSUB.global_opcode().as_usize(),
        Fp2Opcode::MUL.global_opcode().as_usize(),
        Fp2Opcode::DIV.global_opcode().as_usize(),
        Fp2Opcode::SETUP_MULDIV.global_opcode().as_usize(),
        SystemOpcode::PHANTOM.global_opcode().as_usize(),
        SystemOpcode::TERMINATE.global_opcode().as_usize(),
        // TODO clean this up
        0x510, // not sure yet what this is
        0x513, // not sure yet what this is
        0x516, // not sure yet what this is
        0x51c, // not sure yet what this is
        0x523, // not sure yet what this is
        0x526, // not sure yet what this is
        1024,
        1025,
        1028,
        1033,
        1104,
        1030,
        1033,
        1027,
        1029,
    ]
    .into_iter()
    .collect()
}

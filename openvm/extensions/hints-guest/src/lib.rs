#![no_std]
#[cfg(target_os = "zkvm")]
use openvm_custom_insn; // needed for the hint_store_u32 macro
use strum_macros::FromRepr;

/// This is custom-2 defined in RISC-V spec document
pub const OPCODE: u8 = 0x5b;
pub const HINTS_FUNCT3: u8 = 0b000;

#[derive(Debug, Copy, Clone, PartialEq, Eq, FromRepr)]
#[repr(u8)]
pub enum HintsFunct7 {
    ReverseBytes = 0,
    K256InverseField10x26,
}

#[cfg(target_os = "zkvm")]
#[inline(always)]
#[no_mangle]
extern "C" fn hint_reverse_bytes(bytes: *const u8) {
    openvm_platform::custom_insn_r!(
        opcode = OPCODE,
        funct3 = HINTS_FUNCT3,
        funct7 = HintsFunct7::ReverseBytes as u8,
        rd = Const "x0",
        rs1 = In bytes,
        rs2 = Const "x0"
    );
}

#[cfg(target_os = "zkvm")]
#[inline(always)]
#[no_mangle]
extern "C" fn hint_k256_inverse_field_10x26(bytes: *const u8) {
    openvm_platform::custom_insn_r!(
        opcode = OPCODE,
        funct3 = HINTS_FUNCT3,
        funct7 = HintsFunct7::K256InverseField10x26 as u8,
        rd = Const "x0",
        rs1 = In bytes,
        rs2 = Const "x0"
    );
}

// TODO: useless placeholder hint function
pub fn reverse_bytes(val: u32) -> u32 {
    #[cfg(target_os = "zkvm")]
    {
        let mut result = 0;
        hint_reverse_bytes(&val as *const u32 as *const u8);
        openvm_rv32im_guest::hint_store_u32!(&mut result);
        // TODO: zk assertion for hint result goes here
        return result;
    }
    #[cfg(not(target_os = "zkvm"))]
    {
        ((val & 0x000000FF) << 24) |
        ((val & 0x0000FF00) << 8)  |
        ((val & 0x00FF0000) >> 8)  |
        ((val & 0xFF000000) >> 24)
    }
}

// inverse of normalized field element in SECP256k1 modulus, as represented by k256 lib
pub fn k256_inverse_field_10x26(val: [u32; 10]) -> Option<[u32; 10]> {
    #[cfg(target_os = "zkvm")]
    {
        None
    }
    #[cfg(not(target_os = "zkvm"))]
    {
        None
    }
}

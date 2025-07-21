#![no_std]
#[cfg(target_os = "zkvm")]
use openvm_custom_insn; // needed for the hint_store_u32 macro

/// This is custom-2 defined in RISC-V spec document
pub const OPCODE: u8 = 0x5b;
pub const INVERSE_FUNCT3: u8 = 0b000;
pub const INVERSE_FUNCT7: u8 = 0x0;

/// k256 affine inverse hint
#[cfg(target_os = "zkvm")]
#[inline(always)]
#[no_mangle]
extern "C" fn hint_inverse(bytes: *const u8) {
    openvm_platform::custom_insn_r!(
        opcode = OPCODE,
        funct3 = INVERSE_FUNCT3,
        funct7 = INVERSE_FUNCT7,
        rd = Const "x0",
        rs1 = In bytes,
        rs2 = Const "x0"
    );
}

// TODO: actual interface/implementation, currently just for testing the ovm machinery
pub fn inverse(val: u32) -> u32 {
    #[cfg(target_os = "zkvm")]
    {
        let mut result = 0;
        hint_inverse(&val as *const u32 as *const u8);
        openvm_rv32im_guest::hint_store_u32!(&mut result);
        // TODO: zk assertion for hint result goes here
        return result;
    }
    #[cfg(not(target_os = "zkvm"))]
    return val;
}

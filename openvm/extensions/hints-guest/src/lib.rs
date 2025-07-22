#![no_std]
#[cfg(target_os = "zkvm")]
use openvm_custom_insn; // needed for the hint_store_u32 macro

/// This is custom-2 defined in RISC-V spec document
pub const OPCODE: u8 = 0x5b;
pub const HINTS_FUNCT3: u8 = 0b000;
pub const HINTS_FUNCT7: u8 = 0x0;

#[cfg(target_os = "zkvm")]
#[inline(always)]
#[no_mangle]
extern "C" fn hint_foo(bytes: *const u8) {
    openvm_platform::custom_insn_r!(
        opcode = OPCODE,
        funct3 = HINTS_FUNCT3,
        funct7 = HINTS_FUNCT7,
        rd = Const "x0",
        rs1 = In bytes,
        rs2 = Const "x0"
    );
}

// TODO: remove and add proper hints later, this just for checking it works
pub fn foo(val: u32) -> u32 {
    #[cfg(target_os = "zkvm")]
    {
        let mut result = 0;
        hint_foo(&val as *const u32 as *const u8);
        openvm_rv32im_guest::hint_store_u32!(&mut result);
        // TODO: zk assertion for hint result goes here
        return result;
    }
    #[cfg(not(target_os = "zkvm"))]
    return val;
}

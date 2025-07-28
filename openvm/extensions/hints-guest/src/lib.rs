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
    K256InverseField,
    K256InverseField10x26,
    K256SqrtField10x26,
}

#[cfg(target_os = "zkvm")]
#[inline(always)]
fn insn_reverse_bytes(bytes: *const u8) {
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
fn insn_k256_inverse_field(bytes: *const u8) {
    openvm_platform::custom_insn_r!(
        opcode = OPCODE,
        funct3 = HINTS_FUNCT3,
        funct7 = HintsFunct7::K256InverseField as u8,
        rd = Const "x0",
        rs1 = In bytes,
        rs2 = Const "x0"
    );
}

#[cfg(target_os = "zkvm")]
#[inline(always)]
fn insn_k256_inverse_field_10x26(bytes: *const u8) {
    openvm_platform::custom_insn_r!(
        opcode = OPCODE,
        funct3 = HINTS_FUNCT3,
        funct7 = HintsFunct7::K256InverseField10x26 as u8,
        rd = Const "x0",
        rs1 = In bytes,
        rs2 = Const "x0",
    );
}

#[cfg(target_os = "zkvm")]
#[inline(always)]
fn insn_k256_sqrt_field_10x26(bytes: *const u8) {
    openvm_platform::custom_insn_r!(
        opcode = OPCODE,
        funct3 = HINTS_FUNCT3,
        funct7 = HintsFunct7::K256SqrtField10x26 as u8,
        rd = Const "x0",
        rs1 = In bytes,
        rs2 = Const "x0",
    );
}

// TODO: this is just a silly hint example, remove
pub fn hint_reverse_bytes(val: u32) -> u32 {
    #[cfg(target_os = "zkvm")]
    {
        let result = core::mem::MaybeUninit::<u32>::uninit();
        insn_reverse_bytes(&val as *const u32 as *const u8);
        unsafe {
            openvm_rv32im_guest::hint_store_u32!(result.as_ptr() as *const u32);
            result.assume_init()
        }
    }
    #[cfg(not(target_os = "zkvm"))]
    {
        ((val & 0x000000FF) << 24) |
        ((val & 0x0000FF00) << 8)  |
        ((val & 0x00FF0000) >> 8)  |
        ((val & 0xFF000000) >> 24)
    }
}

// Inverse of field element in SECP256k1 modulus (if not zero).
#[cfg(target_os = "zkvm")]
pub fn hint_k256_inverse_field(sec1_bytes: &[u8]) -> [u8;32] {
    insn_k256_inverse_field(sec1_bytes.as_ptr() as *const u8);
    let inverse = core::mem::MaybeUninit::<[u8;32]>::uninit();
    unsafe {
        openvm_rv32im_guest::hint_buffer_u32!(inverse.as_ptr() as *const u8, 8);
        inverse.assume_init()
    }
}


// Inverse of field element in SECP256k1 modulus (if not zero).
// Takes in the raw 32-bit architecture representation of the field element from k256 (`FieldElement10x26`).
#[cfg(target_os = "zkvm")]
pub fn hint_k256_inverse_field_10x26(elem: [u32; 10]) -> [u32; 10] {
    insn_k256_inverse_field_10x26(elem.as_ptr() as *const u8);
    let inverse = core::mem::MaybeUninit::<[u32;10]>::uninit();
    unsafe {
        openvm_rv32im_guest::hint_buffer_u32!(inverse.as_ptr() as *const u8, 10);
        inverse.assume_init()
    }
}

// Square root of a field element in SECP256k1 modulus (if exists).
#[cfg(target_os = "zkvm")]
pub fn hint_k256_sqrt_field_10x26(elem: [u32; 10]) -> Option<[u32; 10]> {
    insn_k256_sqrt_field_10x26(elem.as_ptr() as *const u8);
    // read "boolean" result of whether the square root exists
    let has_sqrt = core::mem::MaybeUninit::<u32>::uninit();
    unsafe {
        openvm_rv32im_guest::hint_store_u32!(has_sqrt.as_ptr() as *const u32);
        if has_sqrt.assume_init() == 0 {
            return None;
        }
    }
    // read actual square root value
    let sqrt = core::mem::MaybeUninit::<[u32;10]>::uninit();
    unsafe {
        openvm_rv32im_guest::hint_buffer_u32!(sqrt.as_ptr() as *const u8, 10);
        Some(sqrt.assume_init())
    }
}

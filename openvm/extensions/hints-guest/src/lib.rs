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

/// Just an example hint that reverses the bytes of a u32 value.
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
        ((val & 0x000000FF) << 24)
            | ((val & 0x0000FF00) << 8)
            | ((val & 0x00FF0000) >> 8)
            | ((val & 0xFF000000) >> 24)
    }
}

/// Inverse of field element in SECP256k1 modulus (if not zero).
/// The caller is responsible for handling the zero input case, and the returned value is zero in that case.
#[cfg(target_os = "zkvm")]
pub fn hint_k256_inverse_field(sec1_bytes: &[u8]) -> [u8; 32] {
    insn_k256_inverse_field(sec1_bytes.as_ptr() as *const u8);
    let inverse = core::mem::MaybeUninit::<[u8; 32]>::uninit();
    unsafe {
        openvm_rv32im_guest::hint_buffer_u32!(inverse.as_ptr() as *const u8, 8);
        inverse.assume_init()
    }
}

/// Ensures that the 10 limbs are weakly normalized (i.e., the most significant limb is 22 bits and the others are 26 bits).
/// For an honest prover, this is a no-op.
#[cfg(target_os = "zkvm")]
fn ensure_weakly_normalized_10x26(limbs: [u32; 10]) -> [u32; 10] {
    [
        limbs[0] & 0x3ffffff,
        limbs[1] & 0x3ffffff,
        limbs[2] & 0x3ffffff,
        limbs[3] & 0x3ffffff,
        limbs[4] & 0x3ffffff,
        limbs[5] & 0x3ffffff,
        limbs[6] & 0x3ffffff,
        limbs[7] & 0x3ffffff,
        limbs[8] & 0x3ffffff,
        limbs[9] & 0x3fffff,
    ]
}

/// Inverse of field element in SECP256k1 modulus (if not zero).
/// Takes in the raw 32-bit architecture representation of the field element from k256 (`FieldElement10x26`).
/// It is guaranteed to be weakly normalized, i.e., the most significant limb is 22 bits and the other
/// limbs are 26 bits long.
/// The caller is responsible for handling the zero input case, and the returned value is undefined in that case.
#[cfg(target_os = "zkvm")]
pub fn hint_k256_inverse_field_10x26(elem: [u32; 10]) -> [u32; 10] {
    insn_k256_inverse_field_10x26(elem.as_ptr() as *const u8);
    let inverse = core::mem::MaybeUninit::<[u32; 10]>::uninit();
    let inverse = unsafe {
        openvm_rv32im_guest::hint_buffer_u32!(inverse.as_ptr() as *const u8, 10);
        inverse.assume_init()
    };
    ensure_weakly_normalized_10x26(inverse)
}

/// Pre-defined non-quadratic residue for k256.
/// The guest should use this value to prove the non-square case.
pub const K256_NON_QUADRATIC_RESIDUE: [u32; 10] = [3, 0, 0, 0, 0, 0, 0, 0, 0, 0];

/// If the input is square, returns true and the square root in the same representation.
/// It is guaranteed to be weakly normalized, i.e., the most significant limb is 22 bits and the other
/// limbs are 26 bits long.
/// If the input is non-square, returns false and the square root of the element times a pre-defined non-quadratic residue.
#[cfg(target_os = "zkvm")]
pub fn hint_k256_sqrt_field_10x26(elem: [u32; 10]) -> (bool, [u32; 10]) {
    insn_k256_sqrt_field_10x26(elem.as_ptr() as *const u8);
    // read the "boolean" result
    let has_sqrt = unsafe {
        let has_sqrt = core::mem::MaybeUninit::<u32>::uninit();
        openvm_rv32im_guest::hint_store_u32!(has_sqrt.as_ptr() as *const u32);
        has_sqrt.assume_init() != 0
    };
    // read the square root value
    let sqrt = unsafe {
        let sqrt = core::mem::MaybeUninit::<[u32; 10]>::uninit();
        openvm_rv32im_guest::hint_buffer_u32!(sqrt.as_ptr() as *const u8, 10);
        sqrt.assume_init()
    };
    let sqrt = ensure_weakly_normalized_10x26(sqrt);
    (has_sqrt, sqrt)
}

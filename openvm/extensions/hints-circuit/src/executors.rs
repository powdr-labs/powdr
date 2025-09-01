use openvm_circuit::arch::{PhantomSubExecutor, Streams};
use openvm_circuit::system::memory::MemoryController;
use openvm_instructions::riscv::RV32_MEMORY_AS;
use openvm_instructions::PhantomDiscriminant;
use openvm_rv32im_circuit::adapters::unsafe_read_rv32_register;
use openvm_stark_backend::p3_field::PrimeField32;

use crate::field10x26_k256;

/// Example hint implementation.
/// Takes a single u32 as input and sets the hint to be the bytes of the u32 in reverse order.
pub struct ReverseBytesSubEx;

impl<F: PrimeField32> PhantomSubExecutor<F> for ReverseBytesSubEx {
    fn phantom_execute(
        &mut self,
        memory: &MemoryController<F>,
        streams: &mut Streams<F>,
        _discriminant: PhantomDiscriminant,
        a: F,
        _b: F,
        c_upper: u16,
    ) -> eyre::Result<()> {
        assert_eq!(c_upper, 0);
        // read register
        let rs1 = unsafe_read_rv32_register(memory, a);
        // read memory
        let bytes = memory.unsafe_read::<4>(
            F::from_canonical_u32(RV32_MEMORY_AS),
            F::from_canonical_u32(rs1),
        );
        // write hint as bytes in reverse
        let hint_bytes = bytes.into_iter().rev().collect();
        streams.hint_stream = hint_bytes;
        Ok(())
    }
}

/// Takes as input a pointer to 32 bytes, the SEC1 encoding (i.e., big-endian) of a k256 coordinate field element.
/// Sets the hint to be the inverse of the field element in the same encoding (if not zero).
/// Sets the hint to zero when the input is zero.
pub struct K256InverseFieldSubEx;

use crypto_bigint::const_monty_form;
use crypto_bigint::impl_modulus;
use crypto_bigint::modular::ConstMontyParams;
use crypto_bigint::Encoding;
use crypto_bigint::Zero;
use crypto_bigint::U256;
impl_modulus!(
    K256Mod,
    U256,
    "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
);

impl<F: PrimeField32> PhantomSubExecutor<F> for K256InverseFieldSubEx {
    fn phantom_execute(
        &mut self,
        memory: &MemoryController<F>,
        streams: &mut Streams<F>,
        _: PhantomDiscriminant,
        a: F,
        _b: F,
        c_upper: u16,
    ) -> eyre::Result<()> {
        assert_eq!(c_upper, 0);
        // read register
        let rs1 = unsafe_read_rv32_register(memory, a);
        // read the field element
        let bytes: [u8; 32] = memory
            .unsafe_read::<32>(
                F::from_canonical_u32(RV32_MEMORY_AS),
                F::from_canonical_u32(rs1),
            )
            .into_iter()
            .map(|f| u8::try_from(f.as_canonical_u32()).expect("value not a byte"))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        let n = U256::from_be_bytes(bytes);

        // perform the inverse.
        let n_mod = const_monty_form!(n, K256Mod);
        let n_inv = if !(bool::from(n_mod.is_zero())) {
            n_mod.inv().unwrap().retrieve()
        } else {
            U256::ZERO
        };
        let inv_bytes = n_inv
            .to_be_bytes()
            .into_iter()
            .map(|b| F::from_canonical_u8(b))
            .collect();
        streams.hint_stream = inv_bytes;

        Ok(())
    }
}

/// Size in bytes of the k256 field element in 10x26 representation.
const FIELD10X26_BYTES: usize = 40; // [u32;10]

/// Takes as input a pointer to the inner representation of a k256 coordinate field element (in 32-bit architectures).
/// Sets the hint to be the inverse of the input (if not zero), in the same representation.
/// If the input is zero (normalized or not), the hint is also set, but undefined.
pub struct K256InverseField10x26SubEx;

impl<F: PrimeField32> PhantomSubExecutor<F> for K256InverseField10x26SubEx {
    fn phantom_execute(
        &mut self,
        memory: &MemoryController<F>,
        streams: &mut Streams<F>,
        _: PhantomDiscriminant,
        a: F,
        _b: F,
        c_upper: u16,
    ) -> eyre::Result<()> {
        assert_eq!(c_upper, 0);
        // read register
        let rs1 = unsafe_read_rv32_register(memory, a);
        // read the k256 field_10x26 as raw bytes
        let bytes: [u8; FIELD10X26_BYTES] = memory
            .unsafe_read::<{ FIELD10X26_BYTES }>(
                F::from_canonical_u32(RV32_MEMORY_AS),
                F::from_canonical_u32(rs1),
            )
            .into_iter()
            .map(|f| u8::try_from(f.as_canonical_u32()).expect("value not a byte"))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();
        // we just reinterpret the bytes as a k256 field element. We don't use mem::transmute to avoid alignment issues
        let mut elem = [0u32; 10];
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                elem.as_mut_ptr() as *mut u8,
                FIELD10X26_BYTES,
            );
        }
        let elem = field10x26_k256::FieldElement10x26(elem);
        let inv = elem.invert().normalize();
        // okay to transmute in the opposite direction
        let inv_bytes: [u8; FIELD10X26_BYTES] = unsafe { std::mem::transmute(inv.0) };
        streams.hint_stream = inv_bytes
            .into_iter()
            .map(|b| F::from_canonical_u8(b))
            .collect();

        Ok(())
    }
}

/// Pre-defined non-quadratic residue for k256.
/// The same value should be used by the guest to check the non-square case.
const K256_NON_QUADRATIC_RESIDUE: field10x26_k256::FieldElement10x26 =
    field10x26_k256::FieldElement10x26([3, 0, 0, 0, 0, 0, 0, 0, 0, 0]);

/// Takes as input a pointer to the inner representation of a k256 coordinate field element (in 32-bit architectures).
/// If the number is square, sets the hint an u32 of value one, followed by a square root in the same inner representation.
/// If the number is not square, sets the hint to an u32 of value zero.
pub struct K256SqrtField10x26SubEx;

impl<F: PrimeField32> PhantomSubExecutor<F> for K256SqrtField10x26SubEx {
    fn phantom_execute(
        &mut self,
        memory: &MemoryController<F>,
        streams: &mut Streams<F>,
        _: PhantomDiscriminant,
        a: F,
        _b: F,
        c_upper: u16,
    ) -> eyre::Result<()> {
        assert_eq!(c_upper, 0);
        // read register
        let rs1 = unsafe_read_rv32_register(memory, a);
        // read the k256 field_10x26 as raw bytes
        let bytes: [u8; FIELD10X26_BYTES] = memory
            .unsafe_read::<{ FIELD10X26_BYTES }>(
                F::from_canonical_u32(RV32_MEMORY_AS),
                F::from_canonical_u32(rs1),
            )
            .into_iter()
            .map(|f| u8::try_from(f.as_canonical_u32()).expect("value not a byte"))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();
        // we just reinterpret the bytes as a k256 field element. Can't use mem::transmute due to alighment requirements
        let mut elem = [0u32; 10];
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                elem.as_mut_ptr() as *mut u8,
                FIELD10X26_BYTES,
            );
        }
        let elem = field10x26_k256::FieldElement10x26(elem);
        let res = elem.sqrt();
        if res.is_some().into() {
            // return 1 followed by the result
            let bytes: [u8; FIELD10X26_BYTES] = unsafe {
                // safe to transmute into u8 array
                std::mem::transmute(res.unwrap().0)
            };
            streams.hint_stream = 1u32
                .to_le_bytes() // indicates that a square root exists
                .into_iter()
                .chain(bytes)
                .map(|b| F::from_canonical_u8(b))
                .collect();
        } else {
            // Number is not square.
            // Find the square root of the number times the predefined non-quadratic residue
            let res = (elem.mul(&K256_NON_QUADRATIC_RESIDUE)).sqrt().unwrap();
            let bytes: [u8; FIELD10X26_BYTES] = unsafe {
                // safe to transmute into u8 array
                std::mem::transmute(res.0)
            };
            streams.hint_stream = 0u32
                .to_le_bytes() // indicate number is not square
                .into_iter()
                .chain(bytes)
                .map(|b| F::from_canonical_u8(b))
                .collect();
        }

        Ok(())
    }
}

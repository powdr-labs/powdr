use core::arch::asm;

extern crate alloc;

use powdr_riscv_syscalls::Syscall;

use alloc::vec;
use alloc::vec::Vec;

pub fn get_data(channel: u32, data: &mut [u32]) {
    for (i, d) in data.iter_mut().enumerate() {
        unsafe {
            asm!("ecall", lateout("a0") *d, in("a0") channel, in("a1") (i+1) as u32, in("t0") Syscall::DataIdentifier as u32)
        };
    }
}

pub fn get_data_len(channel: u32) -> usize {
    let mut out: u32;
    unsafe {
        asm!("ecall", lateout("a0") out, in("a0") channel, in("a1") 0, in("t0") Syscall::DataIdentifier as u32)
    };
    out as usize
}

use serde::de::DeserializeOwned;

pub fn get_data_serde<T: DeserializeOwned>(channel: u32) -> T {
    let l = get_data_len(channel);
    let mut data = vec![0; l];
    get_data(channel, &mut data);

    // TODO this extra conversion can be removed if we change everything to be u8
    let data: Vec<u8> = data.into_iter().map(|x| x as u8).collect();

    serde_cbor::from_slice(&data.as_slice()).unwrap()
}

const GOLDILOCKS: u64 = 0xffffffff00000001;

/// Calls the low level Poseidon coprocessor in PIL, where
/// the last 4 elements are the "cap"
/// and the return value is placed in data[0:4].
/// The safe version below also checks that each u64 element
/// is less than the Goldilocks field.
/// The unsafe version does not perform such checks.
pub fn poseidon_gl(mut data: [u64; 12]) -> [u64; 4] {
    for &n in data.iter() {
        assert!(n < GOLDILOCKS);
    }

    unsafe {
        asm!("ecall", in("a0") &mut data as *mut [u64; 12], in("t0") Syscall::PoseidonGL as u32);
    };

    [data[0], data[1], data[2], data[3]]
}

pub fn poseidon_gl_unsafe(mut data: [u64; 12]) -> [u64; 4] {
    unsafe {
        asm!("ecall", in("a0") &mut data as *mut [u64; 12], in("t0") Syscall::PoseidonGL as u32);
    }

    [data[0], data[1], data[2], data[3]]
}

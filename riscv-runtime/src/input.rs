use core::arch::asm;

extern crate alloc;

use powdr_riscv_syscalls::Syscall;

use alloc::vec;
use alloc::vec::Vec;

pub fn get_prover_input(index: u32) -> u32 {
    let mut value: u32;
    unsafe {
        asm!("ecall", lateout("a0") value, in("a0") index, in("t0") u32::from(Syscall::Input));
    }
    value
}

pub fn get_data(channel: u32, data: &mut [u32]) {
    for (i, d) in data.iter_mut().enumerate() {
        unsafe {
            asm!("ecall", lateout("a0") *d, in("a0") channel, in("a1") (i+1) as u32, in("t0") u32::from(Syscall::DataIdentifier))
        };
    }
}

pub fn get_data_len(channel: u32) -> usize {
    let mut out: u32;
    unsafe {
        asm!("ecall", lateout("a0") out, in("a0") channel, in("a1") 0, in("t0") u32::from(Syscall::DataIdentifier))
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

use core::arch::asm;

extern crate alloc;

use powdr_riscv_syscalls::Syscall;

use alloc::vec;
use alloc::vec::Vec;

pub fn read_word(fd: u32) -> u32 {
    let mut value: u32;
    unsafe {
        asm!("ecall", lateout("a0") value, in("a0") fd, in("t0") u32::from(Syscall::Input));
    }
    value
}

pub fn read_slice(fd: u32, data: &mut [u32]) {
    for (i, d) in data.iter_mut().enumerate() {
        unsafe {
            asm!("ecall", lateout("a0") *d, in("a0") fd, in("a1") (i+1) as u32, in("t0") u32::from(Syscall::DataIdentifier))
        };
    }
}

pub fn read_data_len(fd: u32) -> usize {
    let mut out: u32;
    unsafe {
        asm!("ecall", lateout("a0") out, in("a0") fd, in("a1") 0, in("t0") u32::from(Syscall::DataIdentifier))
    };
    out as usize
}

pub fn write_word(fd: u32, w: u32) {
    unsafe {
        asm!("ecall", in("a0") fd, in("a1") w, in("t0") u32::from(Syscall::Output));
    }
}

pub fn write_slice(fd: u32, data: &[u32]) {
    for w in data {
        write_word(fd, *w);
    }
}

use serde::de::DeserializeOwned;
use serde::Serialize;

pub fn read<T: DeserializeOwned>(fd: u32) -> T {
    let l = read_data_len(fd);
    let mut data = vec![0; l];
    read_slice(fd, &mut data);

    // TODO this extra conversion can be removed if we change everything to be u8
    let data: Vec<u8> = data.into_iter().map(|x| x as u8).collect();

    serde_cbor::from_slice(&data.as_slice()).unwrap()
}

pub fn write<T: Serialize>(fd: u32, data: T) {
    let data = serde_cbor::to_vec(&data).unwrap();

    // TODO we should avoid this somehow
    let data = data.into_iter().map(|x| x as u32).collect::<Vec<u32>>();

    write_slice(fd, &data);
}

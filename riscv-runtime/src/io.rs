use core::arch::asm;

extern crate alloc;

use powdr_riscv_syscalls::Syscall;

use alloc::vec;
use alloc::vec::Vec;

/// A single u32 from input channel 0.
pub fn read_u32(idx: u32) -> u32 {
    let mut value: u32;
    unsafe {
        ecall!(Syscall::Input, lateout("a0") value, in("a0") 0, in("a1") idx + 1);
    }
    value
}

/// Reads data.len() u32s from the file descriptor fd into the data slice.
pub fn read_slice(fd: u32, data: &mut [u32]) {
    for (i, d) in data.iter_mut().enumerate() {
        unsafe {
            ecall!(Syscall::Input, lateout("a0") *d, in("a0") fd, in("a1") (i+1) as u32);
        };
    }
}

/// Reads the length of the data first at index 0, then the data itself.
pub fn read_data_len(fd: u32) -> usize {
    let mut out: u32;
    unsafe {
        ecall!(Syscall::Input, lateout("a0") out, in("a0") fd, in("a1") 0);
    };
    out as usize
}

/// Writes a single u8 to the file descriptor fd.
pub fn write_u8(fd: u32, byte: u8) {
    unsafe {
        ecall!(Syscall::Output, in("a0") fd, in("a1") byte);
    }
}

/// Writes data.len() u8s from the data slice to the file descriptor fd.
pub fn write_slice(fd: u32, data: &[u8]) {
    for byte in data {
        write_u8(fd, *byte);
    }
}

use serde::de::DeserializeOwned;
use serde::Serialize;

static mut STDOUT: Vec<Vec<u8>> = vec![];
static mut STDERR: Vec<Vec<u8>> = vec![];
const STDOUT_FD: u32 = 1;
const STDERR_FD: u32 = 2;

pub fn write_stdout<T: Serialize>(data: T) {
    let data = serde_cbor::to_vec(&data).unwrap();
    unsafe {
        STDOUT.push(data);
    }
}

pub fn write_stderr<T: Serialize>(data: T) {
    let data = serde_cbor::to_vec(&data).unwrap();
    unsafe {
        STDERR.push(data);
    }
}

pub fn finalize() {
    unsafe {
        let data_stdout = serde_cbor::to_vec(&STDOUT).unwrap();
        write_slice(STDOUT_FD, &data_stdout);
        let data_stderr = serde_cbor::to_vec(&STDERR).unwrap();
        write_slice(STDERR_FD, &data_stderr);
    }
}

/// Reads and deserializes a serialized value of type T from the file descriptor fd.
pub fn read<T: DeserializeOwned>(fd: u32) -> T {
    let l = read_data_len(fd);
    let mut data = vec![0; l];
    read_slice(fd, &mut data);

    // TODO this extra conversion can be removed if we change everything to be u8
    let data: Vec<u8> = data.into_iter().map(|x| x as u8).collect();

    serde_cbor::from_slice(data.as_slice()).unwrap()
}

/// Serializes and writes a value of type T to the file descriptor fd.
pub fn write<T: Serialize>(fd: u32, data: T) {
    let data = serde_cbor::to_vec(&data).unwrap();
    write_slice(fd, &data);
}

// The following functions are expected by `std` and `panic_abort` to exist for
// the zkvm target, and we must provide them. The list was taken from file
// "src/sys/pal/zkvm/abi.rs" in the `std` source code, filtered for functions
// that are actually used.

// We don't provide the sys_alloc_aligned(), despite it being used as a default
// allocator, because we already define a global allocator in the `allocator`
// module. It is used in both `std` and `no_std` modes.

use core::{alloc::Layout, arch::asm, slice};

use powdr_riscv_syscalls::Syscall;

use crate::io::write_slice;

#[no_mangle]
extern "C" fn sys_rand(buf: *mut u32, words: usize) {
    // This is only used by std to key the hash function of HashMap. Until we
    // figure a way to make random values available to user programs, let's just
    // have deterministic HashMaps.
    const VALUE: u32 = 4;

    let slice = unsafe { slice::from_raw_parts_mut(buf, words) };
    for v in slice.iter_mut() {
        *v = VALUE;
    }
}

#[no_mangle]
extern "C" fn sys_panic(msg_ptr: *const u8, len: usize) -> ! {
    let out = u32::from(Syscall::Output);
    unsafe {
        write_slice(out, "Panic: ".as_bytes());
        write_slice(out, slice::from_raw_parts(msg_ptr, len));
        write_slice(out, &[b'\n']);

        asm!("unimp");
        loop {}
    }
}

#[no_mangle]
extern "C" fn sys_read(_fd: u32, _buf: *mut u8, _nrequested: usize) -> usize {
    todo!()
}

#[no_mangle]
extern "C" fn sys_write(fd: u32, write_buf: *const u8, nbytes: usize) {
    unsafe {
        let byte_slice = slice::from_raw_parts(write_buf, nbytes);
        write_slice(fd, byte_slice);
    }
}

#[no_mangle]
extern "C" fn sys_getenv(
    _buf: *mut u32,
    _words: usize,
    _varname: *const u8,
    _varname_len: usize,
) -> usize {
    todo!()
}

#[no_mangle]
extern "C" fn sys_argc() -> usize {
    todo!()
}

#[no_mangle]
extern "C" fn sys_argv(_out_words: *mut u32, _out_nwords: usize, _arg_index: usize) -> usize {
    todo!()
}

#[no_mangle]
extern "C" fn sys_alloc_words(nwords: usize) -> *mut u32 {
    // Just call the global allocator, through the official interface.
    extern crate alloc;
    unsafe { alloc::alloc::alloc(Layout::from_size_align(nwords * 4, 4).unwrap()) as *mut u32 }
}

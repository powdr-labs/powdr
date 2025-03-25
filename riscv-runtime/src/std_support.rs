// The following functions are expected by `std` and `panic_abort` to exist for
// the zkvm target, and we must provide them. The list was taken from file
// "src/sys/pal/zkvm/abi.rs" in the `std` source code, filtered for functions
// that are actually used.

// We don't provide the sys_alloc_aligned(), despite it being used as a default
// allocator, because we already define a global allocator in the `allocator`
// module. It is used in both `std` and `no_std` modes.

use core::{alloc::Layout, arch::asm, slice};

use powdr_syscalls::Syscall;

use crate::io::write_slice;

/// The std interface to random number generation.
#[no_mangle]
extern "C" fn sys_rand(buf: *mut u32, words: usize) {
    let buf = buf as *mut u8;
    unsafe {
        let slice = slice::from_raw_parts_mut(buf, words * 4);
        crate::entropy_source::getrandom(slice);
    }
}

#[no_mangle]
extern "C" fn sys_panic(msg_ptr: *const u8, len: usize) -> ! {
    let out: u32 = u8::from(Syscall::Output).into();
    unsafe {
        write_slice(out, "Panic: ".as_bytes());
        write_slice(out, slice::from_raw_parts(msg_ptr, len));
        write_slice(out, b"\n");

        asm!("unimp");

        #[allow(clippy::empty_loop)]
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
    // For now we return the empty env without panicking.
    // TODO Implement properly.
    0
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

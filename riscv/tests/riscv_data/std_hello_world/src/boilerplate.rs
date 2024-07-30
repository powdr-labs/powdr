use powdr_riscv_syscalls::Syscall;
use std::{alloc::Layout, arch::asm, ptr::null_mut};
use talc::{ClaimOnOom, Span, Talc};

// The following two functions are needed when using the `riscv-rt` crate. We won't need these if
// we use our own startup code and linker script like in riscv-runtime.

#[no_mangle]
fn ExceptionHandler(trap_frame: &riscv_rt::TrapFrame) -> ! {
    panic!("hardware exception occurred: {:#?}", trap_frame);
}

#[no_mangle]
fn DefaultHandler() {
    panic!("hardware interrupt occurred");
}

// Allocator stuff:
const HEAP_SIZE: usize = 1024 * 1024 * 1024;
static mut ARENA: [u8; HEAP_SIZE] = [0; HEAP_SIZE];
static mut ALLOCATOR: Talc<ClaimOnOom> =
    Talc::new(unsafe { ClaimOnOom::new(Span::from_const_array(core::ptr::addr_of!(ARENA))) });

// The following functions are expected by `std` zkvm target. We must provide these functions.

/*
pub fn sys_halt();
pub fn sys_output(output_id: u32, output_value: u32);
pub fn sys_sha_compress(
    out_state: *mut [u32; DIGEST_WORDS],
    in_state: *const [u32; DIGEST_WORDS],
    block1_ptr: *const [u32; DIGEST_WORDS],
    block2_ptr: *const [u32; DIGEST_WORDS],
);
pub fn sys_sha_buffer(
    out_state: *mut [u32; DIGEST_WORDS],
    in_state: *const [u32; DIGEST_WORDS],
    buf: *const u8,
    count: u32,
);
pub fn sys_rand(recv_buf: *mut u32, words: usize);
pub fn sys_panic(msg_ptr: *const u8, len: usize) -> !;
pub fn sys_log(msg_ptr: *const u8, len: usize);
pub fn sys_cycle_count() -> usize;
pub fn sys_read(fd: u32, recv_buf: *mut u8, nrequested: usize) -> usize;
pub fn sys_write(fd: u32, write_buf: *const u8, nbytes: usize);
pub fn sys_getenv(
    recv_buf: *mut u32,
    words: usize,
    varname: *const u8,
    varname_len: usize,
) -> usize;
pub fn sys_argc() -> usize;
pub fn sys_argv(out_words: *mut u32, out_nwords: usize, arg_index: usize) -> usize;

// Allocate memory from global HEAP.
pub fn sys_alloc_words(nwords: usize) -> *mut u32;
pub fn sys_alloc_aligned(nwords: usize, align: usize) -> *mut u8;
*/

const DIGEST_WORDS: usize = 8;

#[no_mangle]
extern "C" fn sys_write(fd: u32, write_buf: *const u8, nbytes: usize) {
    unsafe {
        let byte_slice = std::slice::from_raw_parts(write_buf, nbytes);
        print_u8_slice(fd, byte_slice);
    }
}

#[no_mangle]
extern "C" fn sys_panic(msg_ptr: *const u8, len: usize) -> ! {
    let out = u32::from(Syscall::Output);
    unsafe {
        print_u8_slice(out, "Panic: ".as_bytes());
        print_u8_slice(out, std::slice::from_raw_parts(msg_ptr, len));
        print_u8_slice(out, &[b'\n']);

        asm!("unimp");
        loop {}
    }
}

unsafe fn print_u8_slice(fd: u32, slice: &[u8]) {
    for &byte in slice {
        asm!("ecall", in("a0") 1, in("a1") byte, in("t0") fd);
    }
}

#[no_mangle]
extern "C" fn sys_alloc_aligned(size: usize, align: usize) -> *mut u8 {
    let layout = Layout::from_size_align(size, align).unwrap();
    unsafe {
        ALLOCATOR
            .malloc(layout)
            .map_or(null_mut(), |nn| nn.as_ptr())
    }
}

#![no_main]
#![no_std]

extern crate powdr_riscv_runtime;

extern "C" {
    fn memset(s: *mut u8, c: core::ffi::c_int, n: usize) -> *mut u8;
    fn memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8;
    fn memcmp(s1: *const u8, s2: *const u8, n: usize) -> i32;
}

#[no_mangle]
pub fn main() {
    let input = [1u8; 100];
    let mut output = [0u8; 100];

    // Check memset across boundaries
    unsafe {
        memset(output.as_mut_ptr().offset(5), 2, 32);
    }
    for i in 0..5 {
        assert_eq!(output[i], 0,);
    }
    for i in 5..=36 {
        assert_eq!(output[i], 2,);
    }
    for i in 37..100 {
        assert_eq!(output[i], 0,);
    }

    // Check memcpy across boundaries
    unsafe {
        memcpy(output.as_mut_ptr().offset(5), input.as_ptr().offset(5), 32);
    }
    for i in 0..5 {
        assert_eq!(output[i], 0,);
    }
    for i in 5..=36 {
        assert_eq!(output[i], 1,);
    }
    for i in 37..100 {
        assert_eq!(output[i], 0,);
    }

    // Check memcmp across boundaries
    unsafe {
        assert_eq!(
            memcmp(output.as_ptr().offset(5), input.as_ptr().offset(5), 32),
            0,
        );
        assert_eq!(
            memcmp(output.as_ptr().offset(6), input.as_ptr().offset(5), 32),
            -1,
        );
        assert_eq!(
            memcmp(input.as_ptr().offset(5), output.as_ptr().offset(6), 32),
            1,
        );
    }
}

#![no_std]

use runtime::*;

#[no_mangle]
pub fn main() {
    let input = [1u8; 100];
    let mut output = [0u8; 100];

    // Check memset accross boundaries
    unsafe {
        __runtime_memset(output.as_mut_ptr().offset(5), 2, 32);
    }
    for i in 0..5 {
        assert_eq!(
            output[i],
            0,
        );
    }
    for i in 5..=36 {
        assert_eq!(
            output[i],
            2,
        );
    }
    for i in 37..100 {
        assert_eq!(
            output[i],
            0,
        );
    }

    // Check memcpy accross boundaries
    unsafe {
        __runtime_memcpy(output.as_mut_ptr().offset(5), input.as_ptr().offset(5), 32);
    }
    for i in 0..5 {
        assert_eq!(
            output[i],
            0,
        );
    }
    for i in 5..=36 {
        assert_eq!(
            output[i],
            1,
        );
    }
    for i in 37..100 {
        assert_eq!(
            output[i],
            0,
        );
    }

    // Check memcmp accross boundaries
    // This doesn't work because it compiles to the unsupported
    // assembly instruction "srai".
    // unsafe {
    //     assert_eq!(
    //         __runtime_memcmp(output.as_ptr().offset(5), input.as_ptr().offset(5), 32),
    //         0,
    //     );
    //     assert_eq!(
    //         __runtime_memcmp(output.as_ptr().offset(5), input.as_ptr().offset(6), 32),
    //         -1,
    //     );
    //     assert_eq!(
    //         __runtime_memcmp(output.as_ptr().offset(6), input.as_ptr().offset(5), 32),
    //         1,
    //     );
    // }
}
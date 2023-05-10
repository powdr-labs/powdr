#![no_std]
#![feature(start, alloc_error_handler)]

use core::arch::asm;
use core::panic::PanicInfo;

mod allocator;
mod print;
pub use print::print;

#[panic_handler]
fn panic(panic: &PanicInfo<'_>) -> ! {
    print(format_args!("{panic}"));
    unsafe {
        asm!("unimp");
    }
    loop {}
}

#[inline]
pub fn get_prover_input(index: u32) -> u32 {
    let mut value: u32;
    unsafe {
        asm!("ecall", lateout("a0") value, in("a0") index);
    }
    value
}

extern "Rust" {
    fn main();
}
#[no_mangle]
#[start]
pub unsafe extern "C" fn __runtime_start() -> ! {
    unsafe {
        main();
    }
    loop {}
}

#[no_mangle]
pub unsafe extern "C" fn __runtime_memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    if (src as u32) % 4 == 0 && (dest as u32) % 4 == 0 {
        memcpy_aligned(dest, src, n);
    } else {
        for i in 0..n {
            *dest.offset(i as isize) = *src.offset(i as isize);
        }
    }
    dest
}

pub unsafe fn memcpy_aligned(dest: *mut u8, src: *const u8, n: usize) {
    let mut i: isize = 0;
    while i + 3 < n as isize {
        *((dest.offset(i)) as *mut u32) = *((src.offset(i)) as *mut u32);
        i += 4;
    }
    if i < n as isize {
        let value = *((src.offset(i)) as *mut u32);
        let dest_value = (dest.offset(i)) as *mut u32;
        let mask = (1 << (((n as isize - i) * 8) as u32)) - 1;
        *dest_value = (*dest_value & !mask) | (value & mask);
    }
}

#[no_mangle]
pub unsafe extern "C" fn __runtime_memset(dest: *mut u8, val: u8, n: usize) -> *mut u8 {
    let mut i: isize = 0;
    let pattern = (val as u32) << 24 | (val as u32) << 16 | (val as u32) << 8 | (val as u32);

    // Prologue: set bytes in the first non-aligned word
    let first_word_offset = (dest as u32) % 4;
    if first_word_offset != 0 {
        let aligned_dest = dest.offset(-(first_word_offset as isize)) as *mut u32;
        let mask = (1 << ((first_word_offset * 8) as u32)) - 1;
        *aligned_dest = (*aligned_dest & mask) | (pattern & !mask);
        i += 4 - first_word_offset as isize;
    }

    // Set all full words
    while i + 3 < n as isize {
        *((dest.offset(i)) as *mut u32) = pattern;
        i += 4;
    }

    // last part: set the final word
    if i < n as isize {
        let dest_value = (dest.offset(i)) as *mut u32;
        let mask = (1 << (((n as isize - i) * 8) as u32)) - 1;
        *dest_value = (*dest_value & !mask) | (pattern & mask);
    }

    dest
}

#[no_mangle]
pub unsafe extern "C" fn __runtime_memcmp(dest: *const u8, src: *const u8, n: usize) -> i32 {
    // if (src as u32) % 4 == (dest as u32) % 4 {
    //     return memcmp_aligned(dest, src, n);
    // }

    for i in 0..n {
        if *dest.offset(i as isize) != *src.offset(i as isize) {
            return *dest.offset(i as isize) as i32 - *src.offset(i as isize) as i32;
        }
    }

    0i32
}

// pub unsafe fn memcmp_aligned(dest: *mut u8, src: *const u8, n: usize) -> i32 {
//     let mut i: isize = 0;

//     // Prologue: set bytes in the first non-aligned word
//     let first_word_offset = (dest as u32) % 4;
//     if first_word_offset != 0 {
//         let value = *((src.offset(-(first_word_offset as isize))) as *mut u32);
//         let aligned_dest = dest.offset(-(first_word_offset as isize)) as *mut u32;
//         let mask = (1 << ((first_word_offset * 8) as u32)) - 1;
//         if (*aligned_dest & !mask) != (value & !mask) {
//             return false;
//         }
//         i += 4 - first_word_offset as isize;
//     }

//     while i + 3 < n as isize {
//         if *((dest.offset(i)) as *mut u32) != *((src.offset(i)) as *mut u32) {
//             return false;
//         }
//         i += 4;
//     }

//     // last part: set the final word
//     if i < n as isize {
//         let value = *((src.offset(i)) as *mut u32);
//         let dest_value = (dest.offset(i)) as *mut u32;
//         let mask = (1 << (((n as isize - i) * 8) as u32)) - 1;
//         if (*dest_value & mask) != (value & mask) {
//             return false;
//         }
//     }
//     true
// }

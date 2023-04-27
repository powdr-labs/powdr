#![no_std]
#![feature(default_alloc_error_handler)]

use core::alloc::{GlobalAlloc, Layout};
use core::cell::UnsafeCell;
use core::ptr;

extern crate alloc;

use alloc::vec::Vec;

struct BumpPointerAlloc {
    head: UnsafeCell<usize>,
    end: usize,
}

unsafe impl Sync for BumpPointerAlloc {}

unsafe impl GlobalAlloc for BumpPointerAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let head = self.head.get();
        let size = layout.size();
        let align = layout.align();
        let align_mask = !(align - 1);

        // move start up to the next alignment boundary
        let start = (*head + align - 1) & align_mask;

        if start + size > self.end {
            // a null pointer signal an Out Of Memory condition
            ptr::null_mut()
        } else {
            *head = start + size;
            start as *mut u8
        }
    }

    unsafe fn dealloc(&self, _: *mut u8, _: Layout) {
        // this allocator never deallocates memory
    }
}

// TODO could get them from https://github.com/rust-lang/compiler-builtins
// or use compiler_builtins::mem::memcpy::memcpy

pub unsafe extern "C" fn memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    // We only access u32 because then we do not have to deal with
    // un-aligned memory access.
    // TODO this does not really enforce that the pointers are u32-aligned.
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
    dest
}

pub unsafe extern "C" fn memmove(dest: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    // TODO this does not work for overlapping areas.
    memcpy(dest, src, n)
}

// pub unsafe extern "C" fn memcmp(s1: *const u8, s2: *const u8, n: usize) -> i32 {
//     for i in 0..n as isize {
//         let a = *s1.offset(i);
//         let b = *s2.offset(i);
//         if a != b {
//             return a as i32 - b as i32;
//         }
//     }
//     0
// }

// pub unsafe extern "C" fn strlen(s: *const u8) -> usize {
//     let mut i = 0;
//     while *s.offset(i) != 0 {
//         i += 1;
//     }
//     i as usize
// }

// Declaration of the global memory allocator
// NOTE the user must ensure that the memory region `[0x2000_0100, 0x2000_0200]`
// is not used by other parts of the program
#[global_allocator]
static HEAP: BumpPointerAlloc = BumpPointerAlloc {
    head: UnsafeCell::new(0x2000_0100),
    end: 0x2000_0200,
};

static LONG_STR: &str = "aoeueeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee";

#[no_mangle]
pub fn main() -> ! {
    let mut xs = Vec::new();

    xs.push(42);
    assert!(xs.pop() == Some(42));

    assert!(LONG_STR.chars().next() == Some('a'));

    panic!(); //long_str);
}

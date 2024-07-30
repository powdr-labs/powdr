//! A very simple global allocator.
//!
//! Allocates on a global array and never deallocates.

use core::{
    alloc::{GlobalAlloc, Layout},
    cell::Cell,
    ptr::{self, addr_of},
};

// Force C representation so that the large buffer is at the end.
// This might avoid access to memory with large gaps.
#[repr(C)]
struct FixedMemoryAllocator<const SIZE: usize> {
    next_available: Cell<usize>,
    mem_buffer: [u8; SIZE],
}

impl<const SIZE: usize> FixedMemoryAllocator<SIZE> {
    const fn new() -> Self {
        Self {
            mem_buffer: [0; SIZE],
            next_available: Cell::new(0),
        }
    }
}

unsafe impl<const SIZE: usize> GlobalAlloc for FixedMemoryAllocator<SIZE> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.alloc_zeroed(layout)
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        // Start address of the allocation array:
        let array_start = addr_of!(self.mem_buffer) as usize;

        // Address of the next free space:
        let next_ptr = array_start + self.next_available.get();

        // Align the pointer.
        let aligned_ptr = (next_ptr + layout.align() - 1) & !(layout.align() - 1);

        // Where this allocated space ends:
        let end_of_allocation_ptr = aligned_ptr + layout.size();

        // Calculates where the next allocation will start:
        let new_next_available = end_of_allocation_ptr - array_start;

        if new_next_available <= SIZE {
            self.next_available.set(new_next_available);
            aligned_ptr as *mut u8
        } else {
            ptr::null_mut()
        }
    }

    unsafe fn dealloc(&self, _: *mut u8, _: Layout) {
        // Do nothing. This allocator never deallocates.
    }
}

#[global_allocator]
static mut GLOBAL: FixedMemoryAllocator<{ 1024 * 1024 * 1024 }> = FixedMemoryAllocator::new();

#[alloc_error_handler]
fn alloc_error(layout: Layout) -> ! {
    panic!(
        "memory allocation of {} bytes with alignment {} failed",
        layout.size(),
        layout.align()
    );
}

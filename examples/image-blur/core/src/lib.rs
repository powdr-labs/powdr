//! Box blur shared between the zkVM guest (proven) and the host (native
//! recomputation for the hash-equality check).
//!
//! The hot loop is deliberately **interior-only and branch-free**: we iterate
//! the strict interior `RADIUS..dim-RADIUS` and copy the border through
//! unchanged, so there are no per-pixel bounds checks or clamping branches.
//! As a result the executed instruction sequence (and thus powdr's execution
//! profile) is *independent of pixel values* — which is what lets the host
//! profile-guide APC selection on a cheap synthetic (all-black) image and
//! prove on the real one.
//!
//! The kernel size is a compile-time constant and the 25-pixel window sum is
//! fully unrolled (via `crunchy`) into one straight-line basic block per pixel,
//! which is the block powdr turns into an autoprecompile.
//!
//! All arithmetic is integer (`u32` accumulation, truncating `/ AREA`) so the
//! `no_std` guest and the `std` host produce identical bytes.

#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use crunchy::unroll;

/// Kernel radius. A radius of 2 is a 5x5 kernel.
pub const RADIUS: usize = 2;
/// Side length of the kernel (`2 * RADIUS + 1`).
pub const KERNEL: usize = 2 * RADIUS + 1;
/// Number of pixels averaged per output pixel (`KERNEL * KERNEL`).
pub const AREA: u32 = (KERNEL * KERNEL) as u32;

// The hot loop is hand-unrolled to a fixed 5x5 window below; keep the constants
// in sync with the unrolled ranges.
const _: () = assert!(KERNEL == 5, "the blur kernel is unrolled to 5x5; update the unroll! ranges if RADIUS changes");

/// One `KERNEL x KERNEL` box-blur pass over a `width * height` grayscale image
/// (row-major, one byte per pixel). Border pixels (within `RADIUS` of an edge)
/// are copied unchanged. Returns a new buffer of the same length.
pub fn blur(width: usize, height: usize, input: &[u8]) -> Vec<u8> {
    let mut out = input.to_vec();
    for y in RADIUS..height - RADIUS {
        for x in RADIUS..width - RADIUS {
            // Top-left corner of the kernel window centered on (y, x).
            let base = (y - RADIUS) * width + (x - RADIUS);
            // Sum the 25-pixel window. The accesses use `get_unchecked` so the
            // fully-unrolled body has *no* bounds-check branches and stays a
            // single straight-line basic block — the block powdr turns into an
            // autoprecompile.
            let mut sum: u32 = 0;
            unroll! {
                for dy in 0..5 {
                    unroll! {
                        for dx in 0..5 {
                            sum += unsafe { *input.get_unchecked(base + dy * width + dx) } as u32;
                        }
                    }
                }
            }
            unsafe {
                *out.get_unchecked_mut(y * width + x) = (sum / AREA) as u8;
            }
        }
    }
    out
}

#![cfg(feature = "cuda")]

use openvm_circuit_primitives::range_tuple::RangeTupleCheckerChipGPU;
use openvm_cuda_common::{d_buffer::DeviceBuffer, error::CudaError};

extern "C" {
    fn _range_tuple2_add_count(d_hist: *mut u32, sizes2: *const u32, values2: *const u32) -> i32;
}

// Mutates chip.count (device histogram) in place using __device__ RangeTupleChecker<2>::add_count
pub unsafe fn range_tuple2_add_count_on_chip(
    chip: &RangeTupleCheckerChipGPU<2>,
    values: [u32; 2],
) -> Result<(), CudaError> {
    CudaError::from_result(_range_tuple2_add_count(
        chip.count.as_mut_ptr() as *mut u32,
        chip.sizes.as_ptr(),
        values.as_ptr(),
    ))
}

// Optional raw variant
pub unsafe fn range_tuple2_add_count_raw(
    d_hist: &DeviceBuffer<u32>,
    sizes2: &[u32; 2],
    values2: &[u32; 2],
) -> Result<(), CudaError> {
    CudaError::from_result(_range_tuple2_add_count(
        d_hist.as_mut_ptr(),
        sizes2.as_ptr(),
        values2.as_ptr(),
    ))
}

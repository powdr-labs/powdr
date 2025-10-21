#![cfg(feature = "cuda")]

use openvm_circuit_primitives::range_tuple::RangeTupleCheckerChipGPU;
use openvm_cuda_backend::base::DeviceMatrix;
use openvm_cuda_common::{d_buffer::DeviceBuffer, error::CudaError};
use openvm_stark_backend::prover::hal::MatrixDimensions;
use openvm_stark_sdk::p3_baby_bear::BabyBear;

extern "C" {
    fn _range_tuple2_add_count(d_hist: *mut u32, sizes2: *const u32, values2: *const u32) -> i32;
    pub fn _apc_tracegen(
        d_output: *mut BabyBear, // column-major
        output_height: usize,    // H_out
        d_original_airs: *const OriginalAir, // device array, len = n_original_airs
        n_original_airs: usize,           // 
        d_subs: *const Subst,    // device array of all substitutions
        num_apc_calls: i32,      // number of APC calls
    ) -> i32;
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

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct OriginalAir {
    pub width: i32,                // number of columns
    pub height: i32,               // number of rows (Ha)
    pub buffer: *const BabyBear,   // column-major base: col*height + row (device ptr)
    pub row_block_size: i32,       // stride between used rows
    pub substitutions_offset: i32, // offset in d_subs
    pub substitutions_length: i32, // count in d_subs for this AIR
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]   
pub struct Subst {
    /// Source column within this AIR
    pub col: i32,     
    /// Base row offset within the row-block
    pub row: i32,     
    /// Destination APC column
    pub apc_col: i32, 
}

pub fn apc_tracegen(
    output: &mut DeviceMatrix<BabyBear>, // column-major
    original_airs: DeviceBuffer<OriginalAir>, // device array, len = n_airs
    substitutions: DeviceBuffer<Subst>,  // device array of all substitutions
    num_apc_calls: usize,
) -> Result<(), CudaError> {
    let output_height = output.height();
    let n_airs = original_airs.len();

    unsafe {
        CudaError::from_result(_apc_tracegen(
            output.buffer().as_mut_ptr(),
            output_height,
            original_airs.as_ptr(),
            n_airs,
            substitutions.as_ptr(),
            num_apc_calls as i32,
        ))
    }
}

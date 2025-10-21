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

    pub fn _apc_apply_bus(
        d_output: *const BabyBear, // column-major
        output_height: usize,
        d_interactions: *const DevInteraction,
        n_interactions: usize,
        d_arg_spans: *const DevArgSpan,
        n_arg_spans: usize,
        d_bytecode: *const u32,
        bytecode_len: usize,
        num_apc_calls: i32,
        // bus ids
        var_range_bus_id: u32,
        tuple2_bus_id: u32,
        bitwise_bus_id: u32,
        // histograms and params
        d_var_hist: *mut u32,
        var_num_bins: usize,
        d_tuple2_hist: *mut u32,
        tuple2_sz0: u32,
        tuple2_sz1: u32,
        d_bitwise_hist: *mut u32,
        bitwise_num_bits: u32,
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


// GPU Bus Compiler: Compile SymbolicBusInteraction<BabyBear> into device-friendly buffers
#[repr(u32)]
pub enum OpCode {
    PushApc = 0,
    PushConst = 1,
    Add = 2,
    Sub = 3,
    Mul = 4,
    Neg = 5,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DevInteraction {
    pub id: u32,
    pub num_args: u32,
    pub mult_off: u32,
    pub mult_len: u32,
    pub args_index_off: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DevArgSpan {
    pub off: u32,
    pub len: u32,
}

pub fn apc_apply_bus(
    output: &DeviceMatrix<BabyBear>, // column-major
    interactions: DeviceBuffer<DevInteraction>,
    arg_spans: DeviceBuffer<DevArgSpan>,
    bytecode: DeviceBuffer<u32>,
    // periphery-related data
    var_range_bus_id: u32,
    tuple2_bus_id: u32,
    bitwise_bus_id: u32,
    var_range_count: &DeviceBuffer<BabyBear>,
    tuple2_count: &DeviceBuffer<BabyBear>,
    tuple2_sizes: [u32; 2],
    bitwise_count: &DeviceBuffer<BabyBear>,
    bitwise_num_bits: u32,
    num_apc_calls: usize,
) -> Result<(), CudaError> {
    unsafe {
        CudaError::from_result(_apc_apply_bus(
            output.buffer().as_ptr(),
            output.height(),
            interactions.as_ptr(),
            interactions.len(),
            arg_spans.as_ptr(),
            arg_spans.len(),
            bytecode.as_ptr(),
            bytecode.len(),
            num_apc_calls as i32,
            var_range_bus_id,
            tuple2_bus_id,
            bitwise_bus_id,
            var_range_count.as_mut_ptr() as *mut u32,
            var_range_count.len(),
            tuple2_count.as_mut_ptr() as *mut u32,
            tuple2_sizes[0],
            tuple2_sizes[1],
            bitwise_count.as_mut_ptr() as *mut u32,
            bitwise_num_bits,
        ))
    }
}

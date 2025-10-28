#![cfg(feature = "cuda")]

use openvm_cuda_backend::base::DeviceMatrix;
use openvm_cuda_common::{d_buffer::DeviceBuffer, error::CudaError};
use openvm_stark_backend::prover::hal::MatrixDimensions;
use openvm_stark_sdk::p3_baby_bear::BabyBear;

extern "C" {
    /// Launches the GPU kernel that maps original AIR traces into the APC trace buffer.
    ///
    /// Safety: All pointers must be valid device pointers for the specified lengths.
    pub fn _apc_tracegen(
        d_output: *mut BabyBear,             // column-major
        output_height: usize,                // H_out
        d_original_airs: *const OriginalAir, // device array, len = n_original_airs
        n_original_airs: usize,              //
        d_subs: *const Subst,                // device array of all substitutions
        num_apc_calls: i32,                  // number of APC calls
    ) -> i32;

    /// Applies derived expression columns on the GPU.
    /// Each thread processes rows; for rows >= num_apc_calls, writes zeros.
    /// Safety: All device pointers must be valid for the specified lengths.
    pub fn _apc_apply_derived_expr(
        d_output: *mut BabyBear,         // APC trace matrix (column-major)
        output_height: usize,            // rows (height)
        num_apc_calls: i32,              // number of valid rows
        d_specs: *const DerivedExprSpec, // device array of derived expression specs
        n_cols: usize,                   // number of derived columns
        d_bytecode: *const u32,          // device bytecode buffer
    ) -> i32;

    /// Launches the GPU kernel that applies bus interactions to periphery histograms.
    ///
    /// Safety: All pointers must be valid device pointers for the specified lengths.
    pub fn _apc_apply_bus(
        // APC related
        d_output: *const BabyBear, // APC trace buffer (column-major), device pointer
        output_height: usize,      // APC trace height (rows)
        num_apc_calls: i32,        // number of APC calls (rows to process)

        // Interaction related
        d_bytecode: *const u32, // device bytecode buffer for stack-machine expressions
        bytecode_len: usize,    // length of bytecode buffer (u32 words)
        d_interactions: *const DevInteraction, // device array of interactions
        n_interactions: usize,  // number of interactions
        d_arg_spans: *const DevArgSpan, // device array of arg spans into `d_bytecode`
        n_arg_spans: usize,     // number of arg spans

        // Variable range checker related
        var_range_bus_id: u32, // bus id for the variable range checker
        d_var_hist: *mut u32,  // device histogram for variable range checker
        var_num_bins: usize,   // number of bins in variable range histogram

        // Tuple range checker related
        tuple2_bus_id: u32,      // bus id for the 2-tuple range checker
        d_tuple2_hist: *mut u32, // device histogram for tuple2 checker
        tuple2_sz0: u32,         // tuple2 dimension 0 size
        tuple2_sz1: u32,         // tuple2 dimension 1 size

        // Bitwise related
        bitwise_bus_id: u32,      // bus id for the bitwise lookup
        d_bitwise_hist: *mut u32, // device histogram for bitwise lookup
    ) -> i32;
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

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct DerivedColumn {
    /// Destination APC column index
    pub index: i32,
    /// Constant value to set for rows < num_apc_calls; zero beyond
    pub value: BabyBear,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DerivedExprSpec {
    /// Precomputed destination APC column base = (apc_col_index * H)
    pub col_base: u64,
    /// Expression span inside the shared bytecode buffer
    pub span: DevArgSpan,
}

pub fn apc_tracegen(
    output: &mut DeviceMatrix<BabyBear>,      // column-major
    original_airs: DeviceBuffer<OriginalAir>, // device array, len = n_airs
    substitutions: DeviceBuffer<Subst>,       // device array of all substitutions
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

/// High-level wrapper for `_apc_apply_derived_expr`.
/// Applies derived arbitrary expression columns using the GPU stack machine.
pub fn apc_apply_derived_expr(
    output: &mut DeviceMatrix<BabyBear>,
    specs: DeviceBuffer<DerivedExprSpec>,
    bytecode: DeviceBuffer<u32>,
    num_apc_calls: usize,
) -> Result<(), CudaError> {
    unsafe {
        CudaError::from_result(_apc_apply_derived_expr(
            output.buffer().as_mut_ptr(),
            output.height(),
            num_apc_calls as i32,
            specs.as_ptr(),
            specs.len(),
            bytecode.as_ptr(),
        ))
    }
}

/// OpCode enum for the GPU stack machine bus evaluator.
#[repr(u32)]
pub enum OpCode {
    PushApc = 0, // Push the APC value onto the stack. Must be followed by the index of the value in the APC device buffer.
    PushConst = 1, // Push a constant value onto the stack. Must be followed by the constant value.
    Add = 2,     // Add the top two values on the stack.
    Sub = 3,     // Subtract the top two values on the stack.
    Mul = 4,     // Multiply the top two values on the stack.
    Neg = 5,     // Negate the top value on the stack.
    InvOrZero = 6, // Pop a; push inv(a) if a != 0, else 0
}

/// GPU device representation of a bus interaction.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct DevInteraction {
    /// Bus id this interaction targets (matches periphery chip bus id)
    pub bus_id: u32,
    /// Number of argument expressions for this interaction
    pub num_args: u32,
    /// Starting index into the `DevArgSpan` array for this interaction's args
    /// Layout: [ multiplicity span, arg0, arg1, ... ]
    pub args_index_off: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DevArgSpan {
    /// Offset (in u32 words) into `bytecode` where this arg expression starts
    pub off: u32,
    /// Length (instruction count) of this arg expression
    pub len: u32,
}

/// High-level safe wrapper for `_apc_apply_bus`. Applies bus interactions on the GPU,
/// updating periphery histograms in-place.
#[allow(clippy::too_many_arguments)]
pub fn apc_apply_bus(
    // APC related
    output: &DeviceMatrix<BabyBear>, // APC trace matrix (column-major) on device
    num_apc_calls: usize,            // number of APC calls (rows to process)

    // Interaction related
    bytecode: DeviceBuffer<u32>,                // device bytecode buffer
    interactions: DeviceBuffer<DevInteraction>, // device array of interactions
    arg_spans: DeviceBuffer<DevArgSpan>,        // device array of arg spans

    // Variable range checker related
    var_range_bus_id: u32, // bus id for variable range checker
    var_range_count: &DeviceBuffer<BabyBear>, // device histogram for variable range

    // Tuple range checker related
    tuple2_bus_id: u32, // bus id for tuple range checker (2-ary)
    tuple2_count: &DeviceBuffer<BabyBear>, // device histogram for tuple2
    tuple2_sizes: [u32; 2], // tuple2 sizes (dim0, dim1)

    // Bitwise related
    bitwise_bus_id: u32,                    // bus id for bitwise lookup
    bitwise_count: &DeviceBuffer<BabyBear>, // device histogram for bitwise lookup
) -> Result<(), CudaError> {
    unsafe {
        CudaError::from_result(_apc_apply_bus(
            // APC related
            output.buffer().as_ptr(),
            output.height(),
            num_apc_calls as i32,
            // Interaction related
            bytecode.as_ptr(),
            bytecode.len(),
            interactions.as_ptr(),
            interactions.len(),
            arg_spans.as_ptr(),
            arg_spans.len(),
            // Variable range checker related
            var_range_bus_id,
            var_range_count.as_mut_ptr() as *mut u32,
            var_range_count.len(),
            // Tuple range checker related
            tuple2_bus_id,
            tuple2_count.as_mut_ptr() as *mut u32,
            tuple2_sizes[0],
            tuple2_sizes[1],
            // Bitwise related
            bitwise_bus_id,
            bitwise_count.as_mut_ptr() as *mut u32,
        ))
    }
}

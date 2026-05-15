#![cfg(feature = "cuda")]

use openvm_cuda_backend::base::DeviceMatrix;
use openvm_cuda_common::{
    d_buffer::DeviceBuffer,
    error::CudaError,
    stream::{cudaStream_t, CudaStream},
};
use openvm_stark_backend::prover::MatrixDimensions;
use openvm_stark_sdk::p3_baby_bear::BabyBear;

extern "C" {
    /// Launches the GPU kernel that maps original AIR traces into the APC trace buffer.
    ///
    /// Safety: All pointers must be valid device pointers for the specified lengths.
    pub fn _apc_tracegen(
        d_output: *mut BabyBear,             // column-major
        output_height: usize,                // H_out
        d_original_airs: *const OriginalAir, // device array of AIR metadata
        d_subs: *const Subst,                // device array of all substitutions
        n_subs: usize,                       // number of substitutions
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
        num_apc_calls: i32,        // number of APC calls (rows to process)

        // Interaction related
        d_bytecode: *const u32, // device bytecode buffer for stack-machine expressions
        bytecode_len: usize,    // length of bytecode buffer (u32 words)
        d_interactions: *const DevInteraction, // device array of interactions
        n_interactions: usize,  // number of interactions
        d_arg_spans: *const ExprSpan, // device array of arg spans into `d_bytecode`
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

    /// Launches the GKR-DAG variant of the bus kernel.
    ///
    /// Safety: All pointers must be valid device pointers for the specified lengths.
    pub fn _apc_apply_bus_dag(
        // APC trace
        d_output: *const BabyBear,
        num_apc_calls: i32,
        apc_height: u32,

        // Rules: encoded 128-bit `Rule { low, high }` array
        d_rules: *const DevRule,
        n_rules: u32,

        // Interaction-output dispatch table
        d_interactions: *const DevInteractionDag,
        n_interactions: u32,
        d_output_descs: *const OutputDesc,

        // Global intermediates buffer: size = total_threads * buffer_size Fps,
        // slot-major coalesced layout.
        d_intermediates: *mut BabyBear,

        // Histograms
        var_range_bus_id: u32,
        d_var_hist: *mut u32,
        var_num_bins: u32,
        tuple2_bus_id: u32,
        d_tuple2_hist: *mut u32,
        tuple2_sz0: u32,
        tuple2_sz1: u32,
        bitwise_bus_id: u32,
        d_bitwise_hist: *mut u32,

        // CUDA stream to launch the kernel on. Pass cudaStreamPerThread or a
        // pool-allocated CudaStream for concurrent execution.
        stream: cudaStream_t,
    ) -> i32;
}

/// Device-side encoded rule. Matches `Rule { uint64_t low; uint64_t high; }`
/// in `codec.cuh` and the `u128` produced by stark-backend's
/// `RuleWithFlag::encode()`.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct DevRule {
    pub low: u64,
    pub high: u64,
}

/// GPU-side bus interaction descriptor for the DAG kernel.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct DevInteractionDag {
    pub bus_id: u32,
    pub num_args: u32,
    pub outputs_off: u32,
    pub flags: u32,
}

/// Per-output dispatch descriptor. The kernel reads `kind` to choose between
/// (Inter: read inter\[value\]), (Col: read d_output\[value + r\]), or
/// (Const: emit Fp(value)).
#[repr(C)]
#[derive(Clone, Copy)]
pub struct OutputDesc {
    pub kind: u32,
    pub value: u32,
}

pub const OUTPUT_KIND_INTER: u32 = 0;
pub const OUTPUT_KIND_COL: u32 = 1;
pub const OUTPUT_KIND_CONST: u32 = 2;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct OriginalAir {
    pub width: i32,              // number of columns
    pub height: i32,             // number of rows (Ha)
    pub buffer: *const BabyBear, // column-major base: col*height + row (device ptr)
    pub row_block_size: i32,     // stride between used rows
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Subst {
    /// Index of the source AIR in `d_original_airs`
    pub air_index: i32,
    /// Source column within this AIR
    pub col: i32,
    /// Base row offset within the row-block
    pub row: i32,
    /// Destination APC column
    pub apc_col: i32,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct DerivedExprSpec {
    /// Precomputed destination APC column base = (apc_col_index * H)
    pub col_base: u64,
    /// Expression span inside the shared bytecode buffer
    pub span: ExprSpan,
}

pub fn apc_tracegen(
    output: &mut DeviceMatrix<BabyBear>,      // column-major
    original_airs: DeviceBuffer<OriginalAir>, // device array of AIR metadata
    substitutions: DeviceBuffer<Subst>,       // device array of all substitutions
    num_apc_calls: usize,
) -> Result<(), CudaError> {
    let output_height = output.height();

    unsafe {
        CudaError::from_result(_apc_tracegen(
            output.buffer().as_mut_ptr(),
            output_height,
            original_airs.as_ptr(),
            substitutions.as_ptr(),
            substitutions.len(),
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
    InvOrZero = 6, // Invert the top value on the stack if it is not zero, otherwise pop and push zero.
}

/// GPU device representation of a bus interaction.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct DevInteraction {
    /// Bus id this interaction targets (matches periphery chip bus id)
    pub bus_id: u32,
    /// Number of argument expressions for this interaction
    pub num_args: u32,
    /// Starting index into the `ExprSpan` array for this interaction's args
    /// Layout: [ multiplicity span, arg0, arg1, ... ]. The multiplicity span
    /// is unused (and unallocated) when `flags & INTR_FLAG_STATIC_MULT_1`.
    pub args_index_off: u32,
    /// Bitfield. See `INTR_FLAG_*`.
    pub flags: u32,
}

/// Multiplicity is provably 1 for every processed row — the kernel skips the
/// mult bytecode walk and does a single histogram add. Set by host codegen
/// when the (peephole-folded) mult AST is `Number(1)` or `Reference(is_valid)`.
pub const INTR_FLAG_STATIC_MULT_1: u32 = 1;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ExprSpan {
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
    arg_spans: DeviceBuffer<ExprSpan>,          // device array of arg spans

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

/// High-level safe wrapper for `_apc_apply_bus_dag`.
#[allow(clippy::too_many_arguments)]
#[allow(clippy::too_many_arguments)]
pub fn apc_apply_bus_dag(
    output: &DeviceMatrix<BabyBear>,
    num_apc_calls: usize,
    apc_height: usize,
    rules: &DeviceBuffer<DevRule>,
    interactions: &DeviceBuffer<DevInteractionDag>,
    output_descs: &DeviceBuffer<OutputDesc>,
    intermediates: &DeviceBuffer<BabyBear>,
    var_range_bus_id: u32,
    var_range_count: &DeviceBuffer<BabyBear>,
    tuple2_bus_id: u32,
    tuple2_count: &DeviceBuffer<BabyBear>,
    tuple2_sizes: [u32; 2],
    bitwise_bus_id: u32,
    bitwise_count: &DeviceBuffer<BabyBear>,
    stream: &CudaStream,
) -> Result<(), CudaError> {
    unsafe {
        CudaError::from_result(_apc_apply_bus_dag(
            output.buffer().as_ptr(),
            num_apc_calls as i32,
            apc_height as u32,
            rules.as_ptr(),
            rules.len() as u32,
            interactions.as_ptr(),
            interactions.len() as u32,
            output_descs.as_ptr(),
            intermediates.as_mut_ptr(),
            var_range_bus_id,
            var_range_count.as_mut_ptr() as *mut u32,
            var_range_count.len() as u32,
            tuple2_bus_id,
            tuple2_count.as_mut_ptr() as *mut u32,
            tuple2_sizes[0],
            tuple2_sizes[1],
            bitwise_bus_id,
            bitwise_count.as_mut_ptr() as *mut u32,
            stream.as_raw(),
        ))
    }
}

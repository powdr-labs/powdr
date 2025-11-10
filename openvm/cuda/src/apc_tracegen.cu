#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"
#include "expr_eval.cuh"

// ============================================================================================
// Types
// ============================================================================================

struct OriginalAir {
    int width;               // number of columns
    int height;              // number of rows (Ha)
    const Fp* buffer;        // column-major base: col*height + row
    int row_block_size;      // stride between used rows
};

struct Subst {
    int air_index; // index into d_original_airs
    int col;      // source column within this AIR
    int row;      // base row offset within the row-block
    int apc_col;  // destination APC column
};

extern "C" {
  typedef struct {
    uint64_t col_base; // precomputed destination base offset = apc_col_index * H
    ExprSpan span;   // expression span encoding this column's value
  } DerivedExprSpec;
}

// ============================================================================================
// Kernel: each thread iterates rows and processes all substitutions.
// ============================================================================================

__global__ void apc_tracegen_kernel(
    Fp* __restrict__ d_output,                         // column-major
    size_t H,                                          // height of the output
    const OriginalAir* __restrict__ d_original_airs,   // metadata per AIR
    size_t n_airs,                                     // number of AIR entries
    const Subst* __restrict__ d_subs,                  // all substitutions
    size_t n_subs,                                     // number of substitutions
    int num_apc_calls                                  // number of APC calls
) {
    const size_t total_threads = (size_t)gridDim.x * (size_t)blockDim.x;
    const size_t tid = (size_t)blockIdx.x * (size_t)blockDim.x + (size_t)threadIdx.x;

    for (size_t r = tid; r < H; r += total_threads) {
        const bool row_in_range = r < (size_t)num_apc_calls;

        for (size_t i = 0; i < n_subs; ++i) {
            const Subst sub = d_subs[i];
            const size_t dst_idx = (size_t)sub.apc_col * H + r;

            if (!row_in_range) {
                d_output[dst_idx] = Fp(0);
                continue;
            }

            const size_t air_idx = (size_t)sub.air_index;
            // if (air_idx >= n_airs) {
            //     d_output[dst_idx] = Fp(0);
            //     continue;
            // }
            const OriginalAir air = d_original_airs[air_idx];
            const Fp* __restrict__ src_base = air.buffer;
            const size_t src_col_base = (size_t)sub.col * (size_t)air.height;
            const size_t src_r = (size_t)sub.row + r * (size_t)air.row_block_size;

            // if (src_r < (size_t)air.height) {
            d_output[dst_idx] = src_base[src_col_base + src_r];
            // } else {
            //     d_output[dst_idx] = Fp(0);
            // }
        }
    }
}

// ============================================================================================
// Derived expressions: lane-per-row evaluator, sequential over derived columns per row
// ============================================================================================

__global__ void apc_apply_derived_expr_kernel(
    Fp* __restrict__ d_output,   // APC trace (column-major)
    size_t H,                    // rows (height)
    int num_apc_calls,           // number of valid rows
    const DerivedExprSpec* __restrict__ d_specs, // derived expression specs
    size_t n_cols,               // number of derived columns
    const uint32_t* __restrict__ d_bytecode // shared bytecode buffer
) {
    const size_t total_threads = (size_t)gridDim.x * (size_t)blockDim.x;
    const size_t tid = (size_t)blockIdx.x * (size_t)blockDim.x + (size_t)threadIdx.x;

    for (size_t r = tid; r < H; r += total_threads) {
        if (r < (size_t)num_apc_calls) {
            // Compute and write each derived column for this row
            for (size_t i = 0; i < n_cols; ++i) {
                const DerivedExprSpec spec = d_specs[i];
                const size_t col_base = (size_t)spec.col_base;
                const Fp v = eval_arg(spec.span, d_bytecode, d_output, r);
                d_output[col_base + r] = v;
            }
        } else {
            // Zero-fill non-APC rows
            for (size_t i = 0; i < n_cols; ++i) {
                const size_t col_base = (size_t)d_specs[i].col_base;
                d_output[col_base + r] = Fp(0);
            }
        }
    }
}

// ============================================================================================
// Host launcher wrappers — callable from Rust FFI or cudarc
// ============================================================================================

extern "C" int _apc_apply_derived_expr(
    Fp*                d_output,
    size_t             H,
    int                num_apc_calls,
    const DerivedExprSpec* d_specs,
    size_t             n_cols,
    const uint32_t*    d_bytecode
) {
    if (n_cols == 0) return 0;
    const int block_x = 256; // more lanes to cover rows
    const dim3 block(block_x, 1, 1);
    unsigned g = (unsigned)((H + block_x - 1) / block_x);
    if (g == 0u) g = 1u;
    const dim3 grid(g, 1, 1);
    apc_apply_derived_expr_kernel<<<grid, block>>>(
        d_output, H, num_apc_calls, d_specs, n_cols, d_bytecode
    );
    return (int)cudaGetLastError();
}

extern "C" int _apc_tracegen(
    Fp*                      d_output,          // [output_height * output_width], column-major
    size_t                   output_height,     // H_out
    const OriginalAir*       d_original_airs,   // device array, length = n_airs
    size_t                   n_airs,            // metadata array length
    const Subst*             d_subs,            // device array of all substitutions
    size_t                   n_subs,            // number of substitutions
    int                      num_apc_calls      // number of APC calls
) {
    assert((output_height & (output_height - 1)) == 0);  // power-of-two height check

    const int block_x = 256;
    const dim3 block(block_x, 1, 1);
    unsigned g = (unsigned)((output_height + block_x - 1) / block_x);
    if (g == 0u) g = 1u;
    const dim3 grid(g, 1, 1);

    apc_tracegen_kernel<<<grid, block>>>(
        d_output, output_height, d_original_airs, n_airs, d_subs, n_subs, num_apc_calls
    );
    return (int)cudaGetLastError();
}

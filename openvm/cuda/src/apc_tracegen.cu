#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"

// ============================================================================================
// Types
// ============================================================================================

struct OriginalAir {
    int width;               // number of columns
    int height;              // number of rows (Ha)
    const Fp* buffer;        // column-major base: col*height + row
    int row_block_size;      // stride between used rows
    int substitutions_offset;// offset in d_subs
    int substitutions_length;// count in d_subs for this AIR
};

struct Subst {
    int col;      // source column within this AIR
    int row;      // base row offset within the row-block
    int apc_col;  // destination APC column
};

// ============================================================================================
// Kernel: one block per OriginalAir; each warp handles one substitution (APC column).
// ============================================================================================

__global__ void apc_tracegen_kernel(
    Fp* __restrict__ d_output,                         // [output_height * output_width], column-major
    const OriginalAir* __restrict__ d_original_airs,   // metadata per AIR
    const Subst* __restrict__ d_subs,                  // all substitutions
    size_t output_height,                              // H_out
    int num_apc_calls                                  // number of APC calls
) {
    const int air_id = blockIdx.x;
    const OriginalAir air = d_original_airs[air_id];

    const Fp* __restrict__ src_base = air.buffer;
    const int Ha  = air.height;
    const int RBS = air.row_block_size;

    const int lane  = threadIdx.x & 31;     // 0..31
    const int warp  = threadIdx.x >> 5;     // warp index in block
    const int warps_per_block = blockDim.x >> 5;

    // Process this AIR's substitutions in batches of warps_per_block
    for (int base = 0; base < air.substitutions_length; base += warps_per_block) {
        const int rel = base + warp;
        if (rel >= air.substitutions_length) break;

        const Subst sub = d_subs[air.substitutions_offset + rel];

        // Column bases (column-major)
        const size_t dst_col_base = (size_t)sub.apc_col * (size_t)output_height;
        const size_t src_col_base = (size_t)sub.col     * (size_t)Ha;

        // Each lane writes rows lane, lane+32, lane+64, ... (coalesced per warp)
        // Loop over full output height; zero-pad rows beyond `num_apc_calls`.
        for (size_t r = (size_t)lane; r < (size_t)output_height; r += 32) {
            if (r < (size_t)num_apc_calls) {
                const size_t src_r = (size_t)sub.row + r * (size_t)RBS;
                if (src_r < (size_t)Ha) {
                    d_output[dst_col_base + r] = src_base[src_col_base + src_r];
                }
            } else {
                d_output[dst_col_base + r] = Fp(0);
            }
        }
        // Warps are independent for different substitutions; no syncthreads needed here.
    }
}

// ============================================================================================
// Derived columns application: warp-per-derived-column, lane-per-row
// ============================================================================================

__global__ void apc_apply_derived_kernel(
    Fp* __restrict__ d_output,   // APC trace (column-major)
    size_t H,                    // rows (height)
    int num_apc_calls,           // number of valid rows
    const int* __restrict__ d_cols,  // APC column indices for derived constants
    const Fp* __restrict__ d_vals,   // corresponding constant values
    size_t n_cols
) {
    const int warps_per_block = (blockDim.x >> 5);
    const int warp = (threadIdx.x >> 5);
    const int lane = (threadIdx.x & 31);

    for (int base = (int)blockIdx.x * warps_per_block; base < (int)n_cols; base += (int)gridDim.x * warps_per_block) {
        const int i = base + warp;
        if (i >= (int)n_cols) return;
        const int col = d_cols[i];
        const Fp val = d_vals[i];
        const size_t col_base = (size_t)col * H;
        for (size_t r = (size_t)lane; r < H; r += 32) {
            d_output[col_base + r] = (r < (size_t)num_apc_calls) ? val : Fp(0);
        }
    }
}

extern "C" int _apc_apply_derived(
    Fp*                d_output,
    size_t             output_height,
    int                num_apc_calls,
    const int*         d_cols,
    const Fp*          d_values,
    size_t             n_cols
) {
    if (n_cols == 0) return 0;
    const int block_x = 128; // 4 warps per block
    const dim3 block(block_x, 1, 1);
    unsigned g = (unsigned)((n_cols + 3) / 4);
    if (g == 0u) g = 1u;
    const dim3 grid(g, 1, 1);
    apc_apply_derived_kernel<<<grid, block>>>(
        d_output, output_height, num_apc_calls, d_cols, d_values, n_cols
    );
    return (int)cudaGetLastError();
}

// ============================================================================================
// Host launcher wrapper — callable from Rust FFI or cudarc
// ============================================================================================

extern "C" int _apc_tracegen(
    Fp*                      d_output,          // [output_height * output_width], column-major
    size_t                   output_height,     // H_out
    const OriginalAir*       d_original_airs,   // device array, length = n_airs
    size_t                   n_airs,            // one block per AIR
    const Subst*             d_subs,            // device array of all substitutions
    int                      num_apc_calls      // number of APC calls
) {
    assert((output_height & (output_height - 1)) == 0);  // power-of-two height check

    const int block_x = 32;
    const dim3 block(block_x, 1, 1);
    const dim3 grid((unsigned int)n_airs, 1, 1);

    apc_tracegen_kernel<<<grid, block>>>(
        d_output, d_original_airs, d_subs, output_height, num_apc_calls
    );
    return (int)cudaGetLastError();
}
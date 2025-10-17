#include <cuda_runtime.h>
#include <stdint.h>
#include "primitives/histogram.cuh"  // Histogram (in lookup::), RangeTupleChecker (global)

template <uint32_t N>
__global__ void rt_add_count_kernel(uint32_t* global_hist,
                                    const uint32_t* sizes,
                                    const uint32_t* values) {
    if (threadIdx.x == 0 && blockIdx.x == 0) {
        uint32_t sz[N];
        #pragma unroll
        for (uint32_t i = 0; i < N; ++i) sz[i] = sizes[i];

        // RangeTupleChecker is in the global namespace (not lookup::)
        RangeTupleChecker<N> rtc(global_hist, sz);

        uint32_t vals[N];
        #pragma unroll
        for (uint32_t i = 0; i < N; ++i) vals[i] = values[i];

        rtc.add_count(vals);
    }
}

extern "C" int _range_tuple2_add_count(uint32_t* d_hist,
                                       const uint32_t* sizes2,
                                       const uint32_t* values2) {
    rt_add_count_kernel<2><<<1, 1>>>(d_hist, sizes2, values2);
    return cudaGetLastError();
}
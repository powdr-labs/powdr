// Reusable NVRTC + Driver-API shim for per-APC trace-gen kernels.
//
// All extern "C" entry points return 0 on success and a nonzero CUDA / NVRTC
// error code on failure. Caller-owned buffers are documented per function.

#include <cuda.h>
#include <nvrtc.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>

namespace {

bool ensure_primary_context() {
    // cuCtxSetCurrent is per-thread, so each rayon worker thread must call
    // it (cuInit + cuDevicePrimaryCtxRetain are process-global and idempotent).
    // Track init per-thread to avoid the syscall on every launch.
    static thread_local bool initialized = false;
    if (initialized) return true;
    if (cuInit(0) != CUDA_SUCCESS) return false;
    CUdevice dev = 0;
    if (cuDeviceGet(&dev, 0) != CUDA_SUCCESS) return false;
    CUcontext ctx = nullptr;
    if (cuDevicePrimaryCtxRetain(&ctx, dev) != CUDA_SUCCESS) return false;
    if (cuCtxSetCurrent(ctx) != CUDA_SUCCESS) return false;
    initialized = true;
    return true;
}

int device_arch_flag(char* out, size_t out_size) {
    CUdevice dev = 0;
    if (cuDeviceGet(&dev, 0) != CUDA_SUCCESS) return -1;
    int major = 0, minor = 0;
    if (cuDeviceGetAttribute(&major, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR, dev)
            != CUDA_SUCCESS) return -1;
    if (cuDeviceGetAttribute(&minor, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR, dev)
            != CUDA_SUCCESS) return -1;
    std::snprintf(out, out_size, "--gpu-architecture=sm_%d%d", major, minor);
    return 0;
}

}  // namespace

extern "C" {

// Compile a CUDA C++ source string with NVRTC. On success, returns 0 and
// writes a malloc'd PTX buffer into *ptx_out (caller must free with
// powdr_nvrtc_free) and its byte size into *ptx_size_out. On failure, returns
// the (negated) NVRTC result code; if a compilation log is available it is
// written to *log_out (caller must free with powdr_nvrtc_free).
int powdr_nvrtc_compile(
    const char*  src,
    const char*  src_name,
    char**       ptx_out,
    size_t*      ptx_size_out,
    char**       log_out
) {
    if (!ensure_primary_context()) return -1;
    if (ptx_out)      *ptx_out = nullptr;
    if (ptx_size_out) *ptx_size_out = 0;
    if (log_out)      *log_out = nullptr;

    char arch_flag[64];
    if (device_arch_flag(arch_flag, sizeof(arch_flag)) != 0) return -2;

    nvrtcProgram prog = nullptr;
    nvrtcResult cr = nvrtcCreateProgram(&prog, src, src_name, 0, nullptr, nullptr);
    if (cr != NVRTC_SUCCESS) return -1000 - (int)cr;

    const char* opts[] = {
        arch_flag,
        "--use_fast_math",
        "-default-device",
        "--std=c++17",
    };
    cr = nvrtcCompileProgram(prog, (int)(sizeof(opts) / sizeof(opts[0])), opts);

    // Fetch the log either way so caller can debug compile errors.
    size_t log_size = 0;
    nvrtcGetProgramLogSize(prog, &log_size);
    if (log_size > 1 && log_out) {
        char* log = (char*)std::malloc(log_size);
        if (log != nullptr) {
            if (nvrtcGetProgramLog(prog, log) == NVRTC_SUCCESS) {
                *log_out = log;
            } else {
                std::free(log);
            }
        }
    }

    if (cr != NVRTC_SUCCESS) {
        nvrtcDestroyProgram(&prog);
        return -1000 - (int)cr;
    }

    size_t ptx_size = 0;
    cr = nvrtcGetPTXSize(prog, &ptx_size);
    if (cr != NVRTC_SUCCESS) { nvrtcDestroyProgram(&prog); return -1000 - (int)cr; }
    char* ptx = (char*)std::malloc(ptx_size);
    if (ptx == nullptr) { nvrtcDestroyProgram(&prog); return -3; }
    cr = nvrtcGetPTX(prog, ptx);
    if (cr != NVRTC_SUCCESS) {
        std::free(ptx);
        nvrtcDestroyProgram(&prog);
        return -1000 - (int)cr;
    }
    nvrtcDestroyProgram(&prog);

    if (ptx_out)      *ptx_out = ptx;       else std::free(ptx);
    if (ptx_size_out) *ptx_size_out = ptx_size;
    return 0;
}

// Free a buffer allocated by powdr_nvrtc_compile (PTX or log).
void powdr_nvrtc_free(char* p) { std::free(p); }

// Load a PTX blob into a CUDA module. *module_out holds an opaque CUmodule
// handle on success (cast through void*). Caller must call
// powdr_nvrtc_unload_module to release it.
int powdr_nvrtc_load_module(const void* ptx, size_t /*ptx_size*/, void** module_out) {
    if (!ensure_primary_context()) return -1;
    if (module_out) *module_out = nullptr;
    CUmodule mod = nullptr;
    CUresult r = cuModuleLoadData(&mod, ptx);
    if (r != CUDA_SUCCESS) return -2000 - (int)r;
    if (module_out) *module_out = (void*)mod;
    return 0;
}

int powdr_nvrtc_get_function(void* module, const char* name, void** fn_out) {
    if (fn_out) *fn_out = nullptr;
    CUfunction fn = nullptr;
    CUresult r = cuModuleGetFunction(&fn, (CUmodule)module, name);
    if (r != CUDA_SUCCESS) return -2000 - (int)r;
    if (fn_out) *fn_out = (void*)fn;
    return 0;
}

int powdr_nvrtc_unload_module(void* module) {
    CUresult r = cuModuleUnload((CUmodule)module);
    if (r != CUDA_SUCCESS) return -2000 - (int)r;
    return 0;
}

// Launch a JIT-compiled trace-gen kernel that conforms to the v1 signature:
//
//   __global__ void <kernel>(uint32_t* d_output,
//                            size_t H,
//                            int N,
//                            const uint8_t* d_arena,
//                            uint32_t range_max_bits);
//
// Synchronizes before returning so any kernel error surfaces to the caller.
int powdr_nvrtc_launch_jit_v1(
    void*           fn,
    unsigned int*   d_output,
    size_t          H,
    int             num_apc_calls,
    const unsigned char* d_arena,
    unsigned int    range_max_bits,
    unsigned int    grid_x,
    unsigned int    block_x
) {
    if (!ensure_primary_context()) return -1;
    if (fn == nullptr) return -2;
    if (grid_x == 0)  grid_x = 1;
    if (block_x == 0) block_x = 256;

    void* args[] = {
        (void*)&d_output,
        (void*)&H,
        (void*)&num_apc_calls,
        (void*)&d_arena,
        (void*)&range_max_bits,
    };

    CUresult r = cuLaunchKernel(
        (CUfunction)fn,
        grid_x, 1, 1,
        block_x, 1, 1,
        0,
        nullptr,
        args,
        nullptr);
    if (r != CUDA_SUCCESS) return -2000 - (int)r;

    r = cuCtxSynchronize();
    if (r != CUDA_SUCCESS) return -2000 - (int)r;
    return 0;
}

// Launch a JIT-compiled bus kernel that conforms to the bus_v1 signature:
//
//   __global__ void <kernel>(
//       const unsigned int* d_output, int N, unsigned long long H,
//       unsigned int var_range_bus_id, unsigned int* d_var_hist, unsigned int var_num_bins,
//       unsigned int tuple2_bus_id, unsigned int* d_tuple2_hist,
//       unsigned int tuple2_sz0, unsigned int tuple2_sz1,
//       unsigned int bitwise_bus_id, unsigned int* d_bitwise_hist);
//
// Synchronizes before returning so any kernel error surfaces to the caller.
int powdr_nvrtc_launch_bus_v1(
    void*                  fn,
    const unsigned int*    d_output,
    int                    num_apc_calls,
    unsigned long long     H,
    unsigned int           var_range_bus_id,
    unsigned int*          d_var_hist,
    unsigned int           var_num_bins,
    unsigned int           tuple2_bus_id,
    unsigned int*          d_tuple2_hist,
    unsigned int           tuple2_sz0,
    unsigned int           tuple2_sz1,
    unsigned int           bitwise_bus_id,
    unsigned int*          d_bitwise_hist,
    unsigned int           grid_x,
    unsigned int           block_x
) {
    if (!ensure_primary_context()) return -1;
    if (fn == nullptr) return -2;
    if (grid_x == 0)  grid_x = 1;
    if (block_x == 0) block_x = 256;

    void* args[] = {
        (void*)&d_output,
        (void*)&num_apc_calls,
        (void*)&H,
        (void*)&var_range_bus_id,
        (void*)&d_var_hist,
        (void*)&var_num_bins,
        (void*)&tuple2_bus_id,
        (void*)&d_tuple2_hist,
        (void*)&tuple2_sz0,
        (void*)&tuple2_sz1,
        (void*)&bitwise_bus_id,
        (void*)&d_bitwise_hist,
    };

    CUresult r = cuLaunchKernel(
        (CUfunction)fn,
        grid_x, 1, 1,
        block_x, 1, 1,
        0,
        nullptr,
        args,
        nullptr);
    if (r != CUDA_SUCCESS) return -2000 - (int)r;

    r = cuCtxSynchronize();
    if (r != CUDA_SUCCESS) return -2000 - (int)r;
    return 0;
}

}  // extern "C"

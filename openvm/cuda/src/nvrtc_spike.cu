// Phase 0 spike: prove that NVRTC-compiled kernels can launch against
// DeviceBuffer-allocated memory (cudaMallocAsync / VPMM) using the Driver API.
//
// Exposes one extern "C" function:
//     powdr_nvrtc_spike_run_noop(d_ptr, n)
// which compiles a trivial kernel that writes 0x12345678 into the first `n`
// elements of `d_ptr`, loads the resulting PTX as a module, launches the
// kernel, and synchronizes. Returns 0 on success; nonzero on any error.

#include <cuda.h>
#include <nvrtc.h>
#include <cstdio>
#include <cstring>

namespace {

const char* k_noop_src = R"CUDA_SRC(
extern "C" __global__ void noop(unsigned int* p, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) p[i] = 0x12345678u;
}
)CUDA_SRC";

#define NVRTC_CHECK(call)                                                       \
    do {                                                                        \
        nvrtcResult _r = (call);                                                \
        if (_r != NVRTC_SUCCESS) {                                              \
            std::fprintf(stderr, "NVRTC error %d at %s:%d (%s)\n",              \
                         (int)_r, __FILE__, __LINE__, nvrtcGetErrorString(_r)); \
            return -((int)_r + 1000);                                           \
        }                                                                       \
    } while (0)

#define CU_CHECK(call)                                                          \
    do {                                                                        \
        CUresult _r = (call);                                                   \
        if (_r != CUDA_SUCCESS) {                                               \
            const char* _msg = nullptr;                                         \
            cuGetErrorString(_r, &_msg);                                        \
            std::fprintf(stderr, "CUDA driver error %d at %s:%d (%s)\n",        \
                         (int)_r, __FILE__, __LINE__,                           \
                         _msg ? _msg : "unknown");                              \
            return -((int)_r + 2000);                                           \
        }                                                                       \
    } while (0)

}

extern "C" int powdr_nvrtc_spike_run_noop(unsigned int* d_ptr, int n) {
    if (d_ptr == nullptr || n <= 0) return -1;

    // Ensure a CUDA context exists. cuInit + cuDevicePrimaryCtxRetain attaches
    // to the same primary context the Runtime API uses, so the device pointer
    // allocated by cudaMallocAsync / VPMM is valid here.
    CU_CHECK(cuInit(0));
    CUdevice dev = 0;
    CU_CHECK(cuDeviceGet(&dev, 0));
    CUcontext ctx = nullptr;
    CU_CHECK(cuDevicePrimaryCtxRetain(&ctx, dev));
    CU_CHECK(cuCtxSetCurrent(ctx));

    // Determine arch for nvrtc: compute capability of device 0.
    int major = 0, minor = 0;
    CU_CHECK(cuDeviceGetAttribute(&major, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR, dev));
    CU_CHECK(cuDeviceGetAttribute(&minor, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR, dev));
    char arch_flag[64];
    std::snprintf(arch_flag, sizeof(arch_flag), "--gpu-architecture=sm_%d%d", major, minor);

    nvrtcProgram prog = nullptr;
    NVRTC_CHECK(nvrtcCreateProgram(&prog, k_noop_src, "noop.cu", 0, nullptr, nullptr));

    const char* opts[] = {
        arch_flag,
        "--use_fast_math",
        "-default-device",
        "--std=c++17",
    };
    nvrtcResult comp_r = nvrtcCompileProgram(prog, (int)(sizeof(opts) / sizeof(opts[0])), opts);
    if (comp_r != NVRTC_SUCCESS) {
        size_t log_size = 0;
        nvrtcGetProgramLogSize(prog, &log_size);
        if (log_size > 1) {
            char* log = (char*)std::malloc(log_size);
            nvrtcGetProgramLog(prog, log);
            std::fprintf(stderr, "NVRTC compile log:\n%s\n", log);
            std::free(log);
        }
        nvrtcDestroyProgram(&prog);
        return -((int)comp_r + 1000);
    }

    size_t ptx_size = 0;
    NVRTC_CHECK(nvrtcGetPTXSize(prog, &ptx_size));
    char* ptx = (char*)std::malloc(ptx_size);
    NVRTC_CHECK(nvrtcGetPTX(prog, ptx));
    NVRTC_CHECK(nvrtcDestroyProgram(&prog));

    CUmodule mod = nullptr;
    CU_CHECK(cuModuleLoadData(&mod, ptx));
    std::free(ptx);

    CUfunction fn = nullptr;
    CU_CHECK(cuModuleGetFunction(&fn, mod, "noop"));

    int block_x = 256;
    int grid_x = (n + block_x - 1) / block_x;

    void* args[] = { (void*)&d_ptr, (void*)&n };
    CU_CHECK(cuLaunchKernel(fn,
                            grid_x, 1, 1,
                            block_x, 1, 1,
                            0,
                            nullptr,        // default stream (driver API => null = legacy default stream)
                            args,
                            nullptr));

    CU_CHECK(cuCtxSynchronize());
    CU_CHECK(cuModuleUnload(mod));
    return 0;
}

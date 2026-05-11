//! Common NVRTC emitter types shared by NVRTC kernel emitters (e.g. the
//! per-APC bus-codegen path in `nvrtc_bus_emit`). Concrete emitters live
//! in their own modules and produce `EmittedKernel`s consumed by
//! `nvrtc_cache::NvrtcKernelCache`.

/// A piece of CUDA source ready to be handed to NVRTC. `source_hash` is the
/// cache key — bumped by emitters whenever their output format changes.
#[derive(Clone, Debug)]
pub struct EmittedKernel {
    pub source: String,
    pub name: String,
    pub source_hash: u64,
}

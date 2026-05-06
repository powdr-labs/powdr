//! Phase 1 NVRTC kernel cache: dedupes compilation by source hash and
//! holds the resulting `CUmodule` / `CUfunction` for reuse across launches.
//!
//! No disk persistence, no warm-from-disk path — everything lives in
//! process memory. Replace with a richer cache in a later phase.

use std::collections::HashMap;
use std::ffi::{c_void, CString};
use std::sync::{Arc, Mutex, OnceLock};

use rayon::prelude::*;

use crate::cuda_abi;

use super::nvrtc_emit::EmittedKernel;

/// A compiled NVRTC kernel held by the cache. Owns the loaded `CUmodule`
/// and exposes the `CUfunction` to launch.
#[derive(Debug)]
pub struct CompiledKernel {
    /// Opaque `CUmodule` handle. Released on drop.
    module: *mut c_void,
    /// Opaque `CUfunction` handle into `module`.
    function: *mut c_void,
    /// Symbol name we looked up.
    pub name: String,
}

// CUDA driver handles are thread-safe to share once obtained.
unsafe impl Send for CompiledKernel {}
unsafe impl Sync for CompiledKernel {}

impl CompiledKernel {
    /// Raw `CUfunction` for use with `powdr_nvrtc_launch_jit_v1`.
    pub fn function(&self) -> *mut c_void {
        self.function
    }

    /// Raw `CUmodule`. Used to look up `__constant__` symbols on the
    /// kernel's module via `cuModuleGetGlobal`.
    pub fn module(&self) -> *mut c_void {
        self.module
    }
}

impl Drop for CompiledKernel {
    fn drop(&mut self) {
        if !self.module.is_null() {
            // Best-effort unload; ignore any error during shutdown.
            unsafe {
                let _ = cuda_abi::powdr_nvrtc_unload_module(self.module);
            }
            self.module = std::ptr::null_mut();
            self.function = std::ptr::null_mut();
        }
    }
}

/// Errors from compiling and loading an NVRTC kernel.
#[derive(Debug)]
pub enum NvrtcError {
    Compile { code: i32, log: Option<String> },
    Load { code: i32 },
    GetFunction { code: i32 },
    InvalidSource,
}

impl std::fmt::Display for NvrtcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NvrtcError::Compile { code, log } => {
                write!(f, "NVRTC compile failed (code {})", code)?;
                if let Some(l) = log {
                    write!(f, ": {}", l)?;
                }
                Ok(())
            }
            NvrtcError::Load { code } => write!(f, "cuModuleLoadData failed (code {})", code),
            NvrtcError::GetFunction { code } => {
                write!(f, "cuModuleGetFunction failed (code {})", code)
            }
            NvrtcError::InvalidSource => write!(f, "invalid kernel source (interior NUL)"),
        }
    }
}

impl std::error::Error for NvrtcError {}

/// Process-wide cache of compiled NVRTC kernels.
#[derive(Default)]
pub struct NvrtcKernelCache {
    by_hash: Mutex<HashMap<u64, Arc<CompiledKernel>>>,
}

impl NvrtcKernelCache {
    pub fn global() -> &'static Self {
        static CACHE: OnceLock<NvrtcKernelCache> = OnceLock::new();
        CACHE.get_or_init(NvrtcKernelCache::default)
    }

    /// Compile `kernel` if not already cached and return a handle that can be
    /// launched many times. Identical sources (matched by `source_hash`) share
    /// one compiled module.
    pub fn get_or_compile(&self, kernel: &EmittedKernel) -> Result<Arc<CompiledKernel>, NvrtcError> {
        if let Some(existing) = self.by_hash.lock().unwrap().get(&kernel.source_hash) {
            return Ok(Arc::clone(existing));
        }
        let compiled = compile_one(kernel)?;
        let arc = Arc::new(compiled);
        self.by_hash
            .lock()
            .unwrap()
            .entry(kernel.source_hash)
            .or_insert_with(|| Arc::clone(&arc));
        Ok(arc)
    }

    /// Bulk parallel compile: maps `kernels[i]` → result `Arc<CompiledKernel>`,
    /// running cache misses concurrently across the rayon thread pool. Cache
    /// hits return the existing `Arc` without recompiling. Order is preserved.
    /// Duplicate hashes within `kernels` compile only once and share an `Arc`.
    ///
    /// Both the in-memory cache (this struct's `by_hash`) and the on-disk PTX
    /// cache (in `compile_one`) are populated as a side effect, so subsequent
    /// `get_or_compile` calls for the same hash return immediately.
    ///
    /// Useful when many APCs need first-time compilation: an APC=30 prove
    /// needs ~30 NVRTC compiles cold; serially that's seconds, in parallel
    /// (NVRTC + nvcc are thread-safe per-program) it's near `max(per-kernel)`.
    pub fn get_or_compile_many(
        &self,
        kernels: &[EmittedKernel],
    ) -> Vec<Result<Arc<CompiledKernel>, NvrtcError>> {
        // Collect unique kernels (by source_hash) that aren't yet cached. The
        // first occurrence of each hash carries the source we'll compile.
        let unique_misses: Vec<&EmittedKernel> = {
            let by_hash = self.by_hash.lock().unwrap();
            let mut seen: std::collections::HashSet<u64> = std::collections::HashSet::new();
            let mut keep = Vec::new();
            for k in kernels {
                if by_hash.contains_key(&k.source_hash) {
                    continue;
                }
                if seen.insert(k.source_hash) {
                    keep.push(k);
                }
            }
            keep
        };

        // Compile the unique misses in parallel.
        let compiled: Vec<(u64, Result<Arc<CompiledKernel>, NvrtcError>)> = unique_misses
            .par_iter()
            .map(|k| (k.source_hash, compile_one(k).map(Arc::new)))
            .collect();

        // Insert successful compiles into the cache.
        {
            let mut by_hash = self.by_hash.lock().unwrap();
            for (hash, result) in &compiled {
                if let Ok(arc) = result {
                    by_hash.entry(*hash).or_insert_with(|| Arc::clone(arc));
                }
            }
        }

        // Map each input kernel to its result by re-reading the (now-populated)
        // cache, falling back to per-hash compile errors that didn't make it
        // into the cache.
        let by_hash = self.by_hash.lock().unwrap();
        let mut errors: std::collections::HashMap<u64, NvrtcError> = std::collections::HashMap::new();
        for (hash, result) in compiled {
            if let Err(e) = result {
                errors.insert(hash, e);
            }
        }
        kernels
            .iter()
            .map(|k| {
                if let Some(existing) = by_hash.get(&k.source_hash) {
                    Ok(Arc::clone(existing))
                } else if let Some(e) = errors.remove(&k.source_hash) {
                    Err(e)
                } else {
                    // Hash missing from both cache and error map: a duplicate
                    // entry whose error we already returned above. Reconstruct
                    // an InvalidSource so callers see *some* error.
                    Err(NvrtcError::InvalidSource)
                }
            })
            .collect()
    }
}

/// Disk PTX cache directory. Honors `POWDR_NVRTC_CACHE_DIR` if set; otherwise
/// falls back to `~/.cache/powdr/nvrtc_kernels`. Returns `None` if neither
/// can be resolved (in which case the on-disk cache is silently skipped).
fn disk_cache_dir() -> Option<std::path::PathBuf> {
    if let Ok(d) = std::env::var("POWDR_NVRTC_CACHE_DIR") {
        return Some(std::path::PathBuf::from(d));
    }
    let home = std::env::var("HOME").ok()?;
    Some(std::path::PathBuf::from(home).join(".cache/powdr/nvrtc_kernels"))
}

fn disk_cache_path(kernel: &EmittedKernel) -> Option<std::path::PathBuf> {
    Some(disk_cache_dir()?.join(format!("{:016x}.ptx", kernel.source_hash)))
}

fn compile_one(kernel: &EmittedKernel) -> Result<CompiledKernel, NvrtcError> {
    if let Ok(dir) = std::env::var("POWDR_NVRTC_DUMP_DIR") {
        let path = format!("{}/{}.cu", dir, kernel.name);
        if let Err(e) = std::fs::write(&path, kernel.source.as_bytes()) {
            tracing::warn!("POWDR_NVRTC_DUMP_DIR: failed to write {}: {}", path, e);
        } else {
            tracing::info!(
                "Dumped NVRTC source ({} bytes, {} lines) to {}",
                kernel.source.len(),
                kernel.source.lines().count(),
                path
            );
        }
    }

    // Disk cache fast path: load PTX bytes and skip NVRTC entirely.
    if std::env::var("POWDR_NVRTC_NO_DISK_CACHE").is_err() {
        if let Some(p) = disk_cache_path(kernel) {
            if let Ok(ptx) = std::fs::read(&p) {
                let load_start = std::time::Instant::now();
                match load_module_from_ptx(&ptx, &kernel.name) {
                    Ok(handle) => {
                        tracing::info!(
                            "NVRTC PTX cache hit: loaded {} from {} in {:.2}ms ({} bytes)",
                            kernel.name,
                            p.display(),
                            load_start.elapsed().as_secs_f64() * 1000.0,
                            ptx.len()
                        );
                        return Ok(handle);
                    }
                    Err(e) => {
                        tracing::warn!(
                            "NVRTC PTX cache hit but load failed ({}); recompiling",
                            e
                        );
                        // Fall through to NVRTC compile.
                    }
                }
            }
        }
    }

    let compile_start = std::time::Instant::now();
    let src = CString::new(kernel.source.as_str()).map_err(|_| NvrtcError::InvalidSource)?;
    let src_name = CString::new("apc_kernel.cu").unwrap();

    let mut ptx_ptr: *mut std::ffi::c_char = std::ptr::null_mut();
    let mut ptx_size: usize = 0;
    let mut log: *mut std::ffi::c_char = std::ptr::null_mut();

    let rc = unsafe {
        cuda_abi::powdr_nvrtc_compile(
            src.as_ptr(),
            src_name.as_ptr(),
            &mut ptx_ptr,
            &mut ptx_size,
            &mut log,
        )
    };
    if rc != 0 {
        let log_str = unsafe { take_c_str(log) };
        return Err(NvrtcError::Compile { code: rc, log: log_str });
    }
    debug_assert!(!ptx_ptr.is_null() && ptx_size > 0, "NVRTC returned empty PTX");
    if !log.is_null() {
        unsafe { cuda_abi::powdr_nvrtc_free(log) };
    }

    // Copy PTX bytes out of the NVRTC-allocated buffer so we can both load
    // the module and persist to disk after the NVRTC buffer is freed.
    let ptx_bytes: Vec<u8> = unsafe {
        std::slice::from_raw_parts(ptx_ptr as *const u8, ptx_size).to_vec()
    };
    unsafe { cuda_abi::powdr_nvrtc_free(ptx_ptr) };

    let elapsed = compile_start.elapsed();
    tracing::info!(
        "NVRTC compiled {} in {:.2}s (source {} bytes, ptx {} bytes)",
        kernel.name,
        elapsed.as_secs_f64(),
        kernel.source.len(),
        ptx_bytes.len()
    );

    let handle = load_module_from_ptx(&ptx_bytes, &kernel.name)?;

    // Best-effort persist to disk for future runs.
    if std::env::var("POWDR_NVRTC_NO_DISK_CACHE").is_err() {
        if let Some(p) = disk_cache_path(kernel) {
            if let Some(parent) = p.parent() {
                let _ = std::fs::create_dir_all(parent);
            }
            // Atomic write: write to a temp file in the same dir, then rename.
            let tmp = p.with_extension("ptx.tmp");
            if std::fs::write(&tmp, &ptx_bytes)
                .and_then(|_| std::fs::rename(&tmp, &p))
                .is_err()
            {
                let _ = std::fs::remove_file(&tmp);
                tracing::debug!(
                    "Failed to persist PTX to {}; future runs will recompile",
                    p.display()
                );
            }
        }
    }

    Ok(handle)
}

/// Load PTX bytes into a CUDA module and look up the kernel function.
fn load_module_from_ptx(ptx: &[u8], kernel_name: &str) -> Result<CompiledKernel, NvrtcError> {
    let name_c = CString::new(kernel_name).map_err(|_| NvrtcError::InvalidSource)?;

    let mut module: *mut c_void = std::ptr::null_mut();
    let load_rc = unsafe {
        cuda_abi::powdr_nvrtc_load_module(
            ptx.as_ptr() as *const c_void,
            ptx.len(),
            &mut module,
        )
    };
    if load_rc != 0 {
        return Err(NvrtcError::Load { code: load_rc });
    }

    let mut function: *mut c_void = std::ptr::null_mut();
    let fn_rc = unsafe {
        cuda_abi::powdr_nvrtc_get_function(module, name_c.as_ptr(), &mut function)
    };
    if fn_rc != 0 {
        unsafe { cuda_abi::powdr_nvrtc_unload_module(module) };
        return Err(NvrtcError::GetFunction { code: fn_rc });
    }

    Ok(CompiledKernel {
        module,
        function,
        name: kernel_name.to_string(),
    })
}

/// Take ownership of a `*mut c_char` produced by `powdr_nvrtc_compile`,
/// returning a Rust string and freeing the underlying buffer.
unsafe fn take_c_str(p: *mut std::ffi::c_char) -> Option<String> {
    if p.is_null() {
        return None;
    }
    let s = std::ffi::CStr::from_ptr(p).to_string_lossy().into_owned();
    cuda_abi::powdr_nvrtc_free(p);
    Some(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::powdr_extension::trace_generator::cuda::nvrtc_emit::{
        emit_jit_kernel_source, EmitterColumn, EmitterInput, EmitterInstruction,
    };

    fn sample() -> EmittedKernel {
        emit_jit_kernel_source(&EmitterInput {
            instructions: vec![EmitterInstruction {
                arena_offset: 0,
                record_stride: 64,
                record_offset: 0,
                columns: vec![EmitterColumn::DirectU32 { apc_col: 0, off: 0 }],
            }],
        })
    }

    #[test]
    fn cache_compiles_and_dedupes() {
        let cache = NvrtcKernelCache::default();
        let k1 = sample();
        let k2 = sample(); // identical source
        let a = cache.get_or_compile(&k1).expect("first compile");
        let b = cache.get_or_compile(&k2).expect("second compile");
        // Same Arc => same compiled module.
        assert!(Arc::ptr_eq(&a, &b), "expected dedupe by source hash");
        assert!(!a.function().is_null());
    }

    /// `get_or_compile_many` parallel-compiles distinct sources and dedupes
    /// duplicates, populating the cache so a subsequent `get_or_compile`
    /// returns the same `Arc` without recompiling.
    #[test]
    fn cache_parallel_compile_dedupes_and_populates() {
        // Build N distinct sources (different stride values produce different
        // hashes). Repeat one of them to test in-batch dedupe.
        let mut kernels: Vec<EmittedKernel> = (0..4)
            .map(|i| {
                emit_jit_kernel_source(&EmitterInput {
                    instructions: vec![EmitterInstruction {
                        arena_offset: 0,
                        record_stride: 64 + i as u32,
                        record_offset: 0,
                        columns: vec![EmitterColumn::DirectU32 { apc_col: 0, off: 0 }],
                    }],
                })
            })
            .collect();
        // Duplicate kernels[1] so the batch contains two entries with the
        // same source hash.
        kernels.push(kernels[1].clone());

        let cache = NvrtcKernelCache::default();
        let results = cache.get_or_compile_many(&kernels);
        assert_eq!(results.len(), kernels.len());
        let arcs: Vec<Arc<CompiledKernel>> = results
            .into_iter()
            .map(|r| r.expect("each compile must succeed"))
            .collect();

        // Duplicate kernel must dedupe to the same Arc as the original.
        assert!(
            Arc::ptr_eq(&arcs[1], &arcs[4]),
            "expected duplicate kernel to dedupe within batch"
        );

        // Cache must be populated: get_or_compile returns the SAME Arc.
        for (k, expected) in kernels.iter().zip(arcs.iter()) {
            let got = cache.get_or_compile(k).expect("post-batch get");
            assert!(Arc::ptr_eq(&got, expected), "expected cache hit");
        }
    }

    /// End-to-end Phase 1 vertical slice: build a synthetic arena with
    /// known values, emit a DirectU32-only kernel, compile via NVRTC, launch
    /// it against a fresh trace buffer, copy back, and assert each cell
    /// matches the host-side Montgomery encoding of the synthetic value.
    #[test]
    fn launch_direct_u32_writes_montgomery_words() {
        use openvm_cuda_common::{
            copy::{MemCopyD2H, MemCopyH2D},
            d_buffer::DeviceBuffer,
        };

        use crate::powdr_extension::trace_generator::cuda::nvrtc_emit::host_to_monty;

        // 16 valid rows, width = 3 cols. Trace is column-major.
        const N: usize = 16;
        const H: usize = 16; // already a power of two
        const WIDTH: usize = 3;
        const RECORD_STRIDE: usize = 16; // bytes per APC call

        // Build synthetic arena: row r has u32(r+1) at offset 0, u32(0xCAFE0000+r)
        // at offset 4, and u32((P-1) - r) at offset 8 (last is canonical-form
        // BabyBear which is what records carry).
        let p: u32 = 0x7800_0001;
        let mut arena_bytes = vec![0u8; N * RECORD_STRIDE];
        for r in 0..N {
            let row = &mut arena_bytes[r * RECORD_STRIDE..(r + 1) * RECORD_STRIDE];
            row[0..4].copy_from_slice(&((r as u32) + 1).to_le_bytes());
            row[4..8].copy_from_slice(&(0xCAFE_0000u32 + r as u32).to_le_bytes());
            row[8..12].copy_from_slice(&((p - 1) - r as u32).to_le_bytes());
            // 12..16 left as zero
        }
        let d_arena = arena_bytes.to_device().expect("arena H2D");

        // Allocate column-major trace buffer of u32 Montgomery words. Pre-zero
        // so non-written cells stay 0 (Montgomery(0) = 0).
        let d_trace: DeviceBuffer<u32> = DeviceBuffer::with_capacity(H * WIDTH);
        d_trace.fill_zero().expect("zero trace");

        // Emit DirectU32 kernel: col 0 <- offset 0, col 1 <- offset 4,
        // col 2 <- offset 8.
        let kernel = crate::powdr_extension::trace_generator::cuda::nvrtc_emit::emit_jit_kernel_source(
            &EmitterInput {
                instructions: vec![EmitterInstruction {
                    arena_offset: 0,
                    record_stride: RECORD_STRIDE as u32,
                    record_offset: 0,
                    columns: vec![
                        EmitterColumn::DirectU32 { apc_col: 0, off: 0 },
                        EmitterColumn::DirectU32 { apc_col: 1, off: 4 },
                        EmitterColumn::DirectU32 { apc_col: 2, off: 8 },
                    ],
                }],
            },
        );

        let cache = NvrtcKernelCache::default();
        let compiled = cache.get_or_compile(&kernel).expect("compile kernel");

        // Launch: 1 block of 256 threads is plenty for 16 rows.
        let rc = unsafe {
            crate::cuda_abi::powdr_nvrtc_launch_jit_v1(
                compiled.function(),
                d_trace.as_mut_ptr(),
                H,
                N as i32,
                d_arena.as_ptr(),
                /* range_max_bits */ 0,
                /* grid_x */ 1,
                /* block_x */ 256,
            )
        };
        assert_eq!(rc, 0, "launch failed: {}", rc);

        let host: Vec<u32> = d_trace.to_host().expect("trace D2H");
        assert_eq!(host.len(), H * WIDTH);

        // Verify column-major layout: cell (col, row) lives at host[col*H + row].
        for r in 0..N {
            let v0 = (r as u32) + 1;
            let v1 = 0xCAFE_0000u32 + r as u32;
            let v2 = (p - 1) - r as u32;
            assert_eq!(host[0 * H + r], host_to_monty(v0), "col0 row{}", r);
            assert_eq!(host[1 * H + r], host_to_monty(v1), "col1 row{}", r);
            assert_eq!(host[2 * H + r], host_to_monty(v2), "col2 row{}", r);
        }
        // Padding rows (none here since N==H) and unused col bytes stay zero.
    }
}

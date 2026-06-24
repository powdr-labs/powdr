//! On-disk artifact cache used by the staged APC pipeline.
//!
//! Each call to [`cached`] is keyed by `(stage, hash)` and writes a single
//! `artifact.cbor` blob under `<artifacts_dir>/<stage>/<hash>/`. The caller
//! supplies the hash; [`stage_hash`] is the convention pipelines use to mix
//! a per-stage args fingerprint with a guest fingerprint.

use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use serde::de::DeserializeOwned;
use serde::Serialize;

/// Hash `args` together with `prelude` (typically a guest fingerprint).
///
/// Uses `DefaultHasher`, which is intentionally not stable across Rust
/// releases — accepted, since stale entries are silently ignored.
pub fn stage_hash<H: Hash + ?Sized>(args: &H, prelude: &str) -> String {
    let mut hasher = DefaultHasher::new();
    prelude.hash(&mut hasher);
    args.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}

fn cache_path(dir: &Path, stage: &str, hash: &str) -> PathBuf {
    dir.join(stage).join(hash).join("artifact.cbor")
}

pub fn load_cached<T: DeserializeOwned>(
    artifacts_dir: Option<&Path>,
    stage: &str,
    hash: &str,
) -> Option<T> {
    let dir = artifacts_dir?;
    let path = cache_path(dir, stage, hash);
    let file = fs::File::open(&path).ok()?;
    match serde_cbor::from_reader(file) {
        Ok(v) => Some(v),
        Err(err) => {
            tracing::warn!("ignoring corrupt cache entry {}: {err}", path.display());
            None
        }
    }
}

pub fn save_cached<T: Serialize>(artifacts_dir: Option<&Path>, stage: &str, hash: &str, value: &T) {
    let Some(dir) = artifacts_dir else { return };
    let path = cache_path(dir, stage, hash);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    let file = fs::File::create(&path).unwrap();
    serde_cbor::to_writer(file, value).unwrap();
}

/// Return the cached `(stage, hash)` blob if present, else run `compute`,
/// persist its result, and return it. `compute` is only invoked on miss.
pub fn cached<T, F>(artifacts_dir: Option<&Path>, stage: &str, hash: &str, compute: F) -> T
where
    T: Serialize + DeserializeOwned,
    F: FnOnce() -> T,
{
    if let Some(value) = load_cached::<T>(artifacts_dir, stage, hash) {
        tracing::info!("cache hit: {stage}/{hash}");
        return value;
    }
    let value = compute();
    save_cached(artifacts_dir, stage, hash, &value);
    value
}

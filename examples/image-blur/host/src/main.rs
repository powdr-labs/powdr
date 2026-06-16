//! Host driver for the image-blur autoprecompile demo.
//!
//! Decodes an image to grayscale (the private input), compiles the zkVM guest
//! and optionally synthesizes autoprecompiles for its hot blur loop, then proves
//! and verifies the run. The guest reveals a single 32-byte commitment to
//! keccak(input) and keccak(blurred-output); the host recomputes the blur with
//! the same `image_blur_core::blur` and checks the commitment matches.
//!
//! Drives powdr as a library; the proving path mirrors `powdr_openvm_riscv::prove`.

use std::error::Error;
use std::path::{Path, PathBuf};

use clap::Parser;
use metrics_tracing_context::{MetricsLayer, TracingContextLayer};
use metrics_util::{debugging::DebuggingRecorder, layers::Layer};
use openvm_sdk::config::{AggregationSystemParams, AppConfig};
use openvm_sdk::StdIn;
use openvm_stark_sdk::bench::serialize_metric_snapshot;
use openvm_stark_sdk::config::{app_params_with_100_bits_security, MAX_APP_LOG_STACKED_HEIGHT};
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::{PgoType, SelectConfig};
use powdr_openvm::{
    default_generate_config, execution_profile_from_guest, make_default_empirical_constraints,
    PowdrSdkCpu, StagedPipeline,
};
use powdr_openvm_riscv::{
    compile_openvm, GuestOptions, OriginalCompiledProgram, PgoConfig, RiscvISA,
};
use tiny_keccak::{Hasher, Keccak};
use tracing_forest::ForestLayer;
use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};

#[cfg(feature = "metrics")]
use openvm_stark_sdk::metrics_tracing::TimingMetricsLayer;

/// Where the blurred image is written.
const OUT_PATH: &str = "blur.png";
/// powdr's staged APC artifact cache (reused across runs).
const ARTIFACTS_DIR: &str = "apc-cache";
/// Where `apc_candidates.json` is dumped (for plot_effectiveness.py).
const CANDIDATES_DIR: &str = "apc-cache/candidates";
/// Where the proving metrics snapshot is written (for basic_metrics.py).
const METRICS_PATH: &str = "metrics.json";

#[derive(Parser)]
#[command(
    name = "image-blur-host",
    about = "Prove a 5x5 box blur of a private image with powdr autoprecompiles"
)]
struct Args {
    /// Input image (any format the `image` crate decodes; converted to grayscale).
    #[arg(long)]
    image: PathBuf,

    /// Number of autoprecompiles to synthesize for the blur loop. 0 disables APC/PGO.
    #[arg(long, default_value_t = 0)]
    apc: usize,

    /// Skip the STARK proof: just build APCs and run interpreted execution.
    #[arg(long, default_value_t = false)]
    execute_only: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    setup_tracing();
    run(Args::parse())
}

fn run(args: Args) -> Result<(), Box<dyn Error>> {
    // Decode the private input image to grayscale.
    let img = image::open(&args.image)?.to_luma8();
    let (width, height) = img.dimensions();
    let pixels: Vec<u8> = img.into_raw();
    tracing::info!(
        "Loaded {width}x{height} grayscale image ({} bytes)",
        pixels.len()
    );

    // Native reference computation (the guest proves the same thing): blur the
    // image, hash input and output, and commit to both with a single digest.
    let blurred = image_blur_core::blur(width as usize, height as usize, &pixels);
    let h_in = keccak256(&pixels);
    let h_out = keccak256(&blurred);
    let commitment = commit(&h_in, &h_out);

    // Compile the guest. The guest reveals a single 32-byte commitment, which
    // fits the default public-values width — no config override needed.
    let guest_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .ok_or("host crate has no parent dir")?
        .join("guest");
    let original = compile_openvm(
        guest_dir.to_str().ok_or("non-utf8 guest path")?,
        GuestOptions::default(),
    )?;

    // Synthesize autoprecompiles (cached under ARTIFACTS_DIR).
    let pgo_type = if args.apc > 0 {
        PgoType::Cell
    } else {
        PgoType::None
    };
    let select = SelectConfig::new(args.apc as u64, 0);
    std::fs::create_dir_all(CANDIDATES_DIR)?;
    let generate = default_generate_config()
        .with_select_defaults(pgo_type, select)
        .with_apc_candidates_dir(CANDIDATES_DIR);
    // The PGO profile is materialized lazily (only on a cache miss) from this
    // tiny, deterministic descriptor — keeping the cache key stable.
    let pgo_inputs = serde_cbor::to_vec(&(width, height))?;
    let pgo_config = PgoConfig::new(pgo_type, None, pgo_inputs);

    let make_pgo_profile = |guest: &OriginalCompiledProgram<'static, RiscvISA>, inputs: &[u8]| {
        let (w, h): (u32, u32) = serde_cbor::from_slice(inputs).expect("decode pgo descriptor");
        // The blur is branch-free, so an all-black synthetic image of the same
        // dimensions yields the same execution profile as any real image.
        let synthetic = vec![0u8; (w as usize) * (h as usize)];
        let mut stdin = StdIn::default();
        stdin.write(&w);
        stdin.write(&h);
        stdin.write(&synthetic);
        execution_profile_from_guest(guest, stdin)
    };

    tracing::info!(
        "Compiling guest with {} autoprecompiles (pgo={pgo_type:?})",
        args.apc
    );
    let program = StagedPipeline::new(original, Some(PathBuf::from(ARTIFACTS_DIR))).setup(
        &generate,
        &pgo_config,
        select,
        make_pgo_profile,
        make_default_empirical_constraints,
    );

    // Surface the APC candidate report in the working directory (it lives under
    // the cache dir, and is only (re)written on a generate-stage cache miss).
    if args.apc > 0 {
        let candidates = Path::new(CANDIDATES_DIR).join("apc_candidates.json");
        if candidates.exists() {
            std::fs::copy(&candidates, "apc_candidates.json")?;
            tracing::info!("Wrote apc_candidates.json");
        }
    }

    // Build the SDK and the real (private-image) input.
    let app_config = AppConfig::new(
        program.vm_config.clone(),
        app_params_with_100_bits_security(MAX_APP_LOG_STACKED_HEIGHT),
    );
    let sdk = PowdrSdkCpu::<RiscvISA>::new(app_config, AggregationSystemParams::default())
        .map_err(|e| format!("failed to build SDK: {e:?}"))?;
    let mut stdin = StdIn::default();
    stdin.write(&width);
    stdin.write(&height);
    stdin.write(&pixels);

    // Interpreted execution: fast check that the public hashes match, then write
    // the blurred image (independent of proving).
    let public_values = sdk
        .execute(program.exe.clone(), stdin.clone())
        .map_err(|e| format!("execute failed: {e:?}"))?;
    check_public_values(&public_values, &commitment)?;
    write_image(Path::new(OUT_PATH), width, height, &blurred)?;
    tracing::info!(
        "Execution OK — public commitment matches. Wrote {OUT_PATH} (in={}, out={})",
        hex::encode(h_in),
        hex::encode(h_out),
    );

    if args.execute_only {
        return Ok(());
    }

    // Full STARK proof + verification (app + aggregation). Trace-cell metrics
    // emitted during proving are captured to METRICS_PATH.
    tracing::info!("Generating STARK proof (app + aggregation)...");
    let start = std::time::Instant::now();
    let metrics_file = std::fs::File::create(METRICS_PATH)?;
    let proof_pv = run_with_metric_collection_to_file(
        metrics_file,
        move || -> Result<Vec<u8>, Box<dyn Error>> {
            let mut prover = sdk
                .prover(program.exe.clone())
                .map_err(|e| format!("prover init failed: {e:?}"))?;
            let (proof, _) = prover
                .prove(stdin, &[])
                .map_err(|e| format!("prove failed: {e:?}"))?;
            let baseline = prover.generate_baseline();
            PowdrSdkCpu::<RiscvISA>::verify_proof((*sdk.agg_vk()).clone(), baseline, &proof)
                .map_err(|e| format!("verification failed: {e:?}"))?;
            // The verified proof commits to these public values.
            Ok(proof
                .user_pvs_proof
                .public_values
                .iter()
                .map(|f| f.as_canonical_u32() as u8)
                .collect())
        },
    )?;

    // Confirm the proof commits to the value we expect.
    check_public_values(&proof_pv, &commitment)?;
    println!(
        "Proof verified. Proving + verification took {:.2?}. Metrics written to {METRICS_PATH}.",
        start.elapsed()
    );

    Ok(())
}

fn keccak256(data: &[u8]) -> [u8; 32] {
    let mut hasher = Keccak::v256();
    hasher.update(data);
    let mut out = [0u8; 32];
    hasher.finalize(&mut out);
    out
}

/// Single public commitment to both digests: keccak(h_in ‖ h_out). Must match
/// the guest's `reveal_bytes32`.
fn commit(h_in: &[u8; 32], h_out: &[u8; 32]) -> [u8; 32] {
    let mut pair = [0u8; 64];
    pair[..32].copy_from_slice(h_in);
    pair[32..].copy_from_slice(h_out);
    keccak256(&pair)
}

fn check_public_values(pv: &[u8], commitment: &[u8; 32]) -> Result<(), Box<dyn Error>> {
    if pv.len() < 32 || &pv[0..32] != commitment {
        return Err(format!(
            "public commitment mismatch: proof {} vs host {}",
            hex::encode(pv.get(0..32).unwrap_or(pv)),
            hex::encode(commitment)
        )
        .into());
    }
    Ok(())
}

fn write_image(path: &Path, width: u32, height: u32, pixels: &[u8]) -> Result<(), Box<dyn Error>> {
    image::GrayImage::from_raw(width, height, pixels.to_vec())
        .ok_or("pixel buffer does not match dimensions")?
        .save(path)?;
    Ok(())
}

fn setup_tracing() {
    let filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info,p3_=warn"));
    let subscriber = Registry::default()
        .with(filter)
        .with(ForestLayer::default())
        .with(MetricsLayer::new());
    #[cfg(feature = "metrics")]
    let subscriber = subscriber.with(TimingMetricsLayer::new());
    tracing::subscriber::set_global_default(subscriber).unwrap();
}

/// Export the stark-backend metrics emitted during `f` to `file` (same format
/// the CLI uses, so `basic_metrics.py` / `plot_trace_cells.py` work unchanged).
fn run_with_metric_collection_to_file<R>(file: std::fs::File, f: impl FnOnce() -> R) -> R {
    let recorder = DebuggingRecorder::new();
    let snapshotter = recorder.snapshotter();
    let recorder = TracingContextLayer::all().layer(recorder);
    metrics::set_global_recorder(recorder).unwrap();
    let res = f();
    serde_json::to_writer_pretty(&file, &serialize_metric_snapshot(snapshotter.snapshot()))
        .unwrap();
    res
}

pub use powdr_ast as ast;
pub use powdr_backend as backend;
pub use powdr_executor as executor;
pub use powdr_number as number;
pub use powdr_pil_analyzer as pil_analyzer;
pub use powdr_pipeline as pipeline;
pub use powdr_riscv as riscv;
pub use powdr_riscv_executor as riscv_executor;

pub use powdr_pipeline::Pipeline;

pub use powdr_number::Bn254Field;
pub use powdr_number::FieldElement;
pub use powdr_number::GoldilocksField;

use riscv::CompilerOptions;

use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;
use std::time::Instant;

pub struct Session {
    pipeline: Pipeline<GoldilocksField>,
    out_path: String,
}

const DEFAULT_PKEY: &str = "pkey.bin";
const DEFAULT_VKEY: &str = "vkey.bin";

// Minimum and maximum log of number of rows for the RISCV machine.
const DEFAULT_LOG_MIN_DEGREE: u32 = 5;
const DEFAULT_LOG_MAX_DEGREE: u32 = 20;
// Minimum acceptable max degree.
const DEFAULT_MIN_LOG_MAX_DEGREE: u32 = 16;

impl Session {
    pub fn new(guest_path: &str, out_path: &str) -> Self {
        Session {
            pipeline: pipeline_from_guest(
                guest_path,
                Path::new(out_path),
                DEFAULT_LOG_MIN_DEGREE,
                DEFAULT_LOG_MAX_DEGREE,
            ),
            out_path: out_path.into(),
        }
        .with_backend(powdr_backend::BackendType::Plonky3)
    }

    pub fn new_with_chunk_size(guest_path: &str, out_path: &str, chunk_size_log: u32) -> Self {
        assert!(chunk_size_log >= DEFAULT_MIN_LOG_MAX_DEGREE);
        Session {
            pipeline: pipeline_from_guest(
                guest_path,
                Path::new(out_path),
                DEFAULT_LOG_MIN_DEGREE,
                chunk_size_log,
            ),
            out_path: out_path.into(),
        }
        .with_backend(powdr_backend::BackendType::Plonky3)
    }

    pub fn into_pipeline(self) -> Pipeline<GoldilocksField> {
        self.pipeline
    }

    pub fn pipeline(&self) -> &Pipeline<GoldilocksField> {
        &self.pipeline
    }

    pub fn with_backend(self, backend: backend::BackendType) -> Self {
        Session {
            pipeline: self.pipeline.with_backend(backend, None),
            ..self
        }
    }

    pub fn write<S: serde::Serialize>(self, channel: u32, data: &S) -> Self {
        Session {
            pipeline: self.pipeline.add_data(channel, data),
            ..self
        }
    }

    pub fn run(&mut self) {
        run(&mut self.pipeline);
    }

    pub fn prove(&mut self) {
        let asm_name = self.pipeline.asm_string().unwrap().0.clone().unwrap();
        let pil_file = create_pil_file_path(&asm_name);

        let generate_artifacts = if let Some(existing_pil) = read_file_if_exists(&pil_file) {
            let computed_pil = self.pipeline.compute_optimized_pil().unwrap().to_string();
            if existing_pil != computed_pil {
                log::info!("Compiled PIL changed, invalidating artifacts...");
                true
            } else {
                log::info!("Compiled PIL did not change, will try to reuse artifacts...");
                false
            }
        } else {
            log::info!("PIL file not found, will generate artifacts...");
            true
        };

        let pkey = Path::new(&self.out_path).join(DEFAULT_PKEY);
        let vkey = Path::new(&self.out_path).join(DEFAULT_VKEY);

        if generate_artifacts {
            self.pipeline.compute_fixed_cols().unwrap();
            self.pipeline.setup_backend().unwrap();
            self.export_setup();
            self.pipeline.set_pkey_file(pkey.clone());
            self.pipeline.set_vkey_file(vkey.clone());
        } else {
            if self
                .pipeline
                .read_constants_mut(Path::new(&self.out_path))
                .is_ok()
            {
                log::info!("Read constants from file...");
            } else {
                self.pipeline.compute_fixed_cols().unwrap();
            }

            if pkey.exists() && vkey.exists() {
                log::info!("Re-using proving and verification keys...");
                self.pipeline.set_pkey_file(pkey.clone());
                self.pipeline.set_vkey_file(vkey.clone());
                self.pipeline.setup_backend().unwrap();
            } else {
                self.export_setup();
                self.pipeline.set_pkey_file(pkey.clone());
                self.pipeline.set_vkey_file(vkey.clone());
            }
        }

        prove(&mut self.pipeline);
    }

    pub fn export_setup(&mut self) {
        let mut path = PathBuf::from(self.out_path.clone());
        path.push(DEFAULT_PKEY);
        let file = File::create(path).unwrap();

        self.pipeline.export_proving_key(file).unwrap();

        let mut path = PathBuf::from(self.out_path.clone());
        path.push(DEFAULT_VKEY);
        let file = File::create(path).unwrap();

        self.pipeline.export_verification_key(file).unwrap();
    }
}

fn create_pil_file_path(asm_name: &Path) -> PathBuf {
    let file_stem = asm_name.file_stem().unwrap().to_str().unwrap();
    let opt_file_stem = format!("{file_stem}_opt");
    asm_name.with_file_name(opt_file_stem).with_extension("pil")
}

fn read_file_if_exists(path: &Path) -> Option<String> {
    if path.exists() {
        let mut file = File::open(path).unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        Some(contents)
    } else {
        None
    }
}

pub fn build_guest(
    guest_path: &str,
    out_path: &Path,
    log_min_degree: u32,
    log_max_degree: u32,
) -> (PathBuf, String) {
    riscv::compile_rust(
        guest_path,
        CompilerOptions::new_gl()
            .with_poseidon()
            .with_continuations()
            .with_log_min_degree(log_min_degree)
            .with_log_max_degree(log_max_degree),
        out_path,
        true,
        None,
    )
    .ok_or_else(|| vec!["could not compile rust".to_string()])
    .unwrap()
}

pub fn pipeline_from_guest(
    guest_path: &str,
    out_path: &Path,
    log_min_degree: u32,
    log_max_degree: u32,
) -> Pipeline<GoldilocksField> {
    log::info!("Compiling guest program...");

    let (asm_file_path, asm_contents) =
        build_guest(guest_path, out_path, log_min_degree, log_max_degree);

    // Create a pipeline from the asm program
    Pipeline::<GoldilocksField>::default()
        .from_asm_string(asm_contents.clone(), Some(asm_file_path.clone()))
        .with_output(out_path.into(), true)
}

pub fn run(pipeline: &mut Pipeline<GoldilocksField>) {
    log::info!("Running powdr-riscv executor in fast mode...");
    let start = Instant::now();

    let program = pipeline.compute_analyzed_asm().unwrap().clone();
    let initial_memory = riscv::continuations::load_initial_memory(&program);
    let (trace, _mem, _reg_mem) = riscv_executor::execute_ast(
        &program,
        initial_memory,
        pipeline.data_callback().unwrap(),
        &riscv::continuations::bootloader::default_input(&[]),
        usize::MAX,
        riscv_executor::ExecMode::Fast,
        None,
    );

    let duration = start.elapsed();
    log::info!("Fast executor took: {:?}", duration);
    log::info!("Trace length: {}", trace.len);
}

pub fn prove(pipeline: &mut Pipeline<GoldilocksField>) {
    log::info!("Running powdr-riscv executor in trace mode for continuations...");
    let start = Instant::now();

    let bootloader_inputs =
        riscv::continuations::rust_continuations_dry_run(&mut pipeline.clone(), None);

    let duration = start.elapsed();
    log::info!("Trace executor took: {:?}", duration);

    // TODO how do we skip PIL compilation and fixed column generation if not needed?
    // We can check whether they exist and not generate it, but what if the asm changed?
    // Maybe one solution is to at least compile asm to PIL and see if that changed.
    let generate_proof = |pipeline: &mut Pipeline<GoldilocksField>| -> Result<(), Vec<String>> {
        let start = Instant::now();
        log::info!("Generating witness...");
        pipeline.compute_witness()?;
        let duration = start.elapsed();
        log::info!("Generating witness took: {duration:?}");

        log::info!("Generating proof...");
        let start = Instant::now();

        pipeline.compute_proof().unwrap();

        let duration = start.elapsed();
        log::info!("Proof generation took: {:?}", duration);

        Ok(())
    };

    pipeline.rollback_from_witness();

    log::info!("Running witness and proof generation for all chunks...");
    let start = Instant::now();
    riscv::continuations::rust_continuations(pipeline, generate_proof, bootloader_inputs).unwrap();
    let duration = start.elapsed();
    log::info!("Proof generation for all chunks took: {:?}", duration);
}

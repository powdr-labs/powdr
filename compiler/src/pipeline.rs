use std::{
    fmt::Display,
    fs,
    io::{BufWriter, Read, Write},
    path::{Path, PathBuf},
    time::Instant,
};

use analysis::convert_analyzed_to_pil_constraints;
use ast::{
    analyzed::Analyzed,
    asm_analysis::AnalysisASMFile,
    object::PILGraph,
    parsed::{asm::ASMProgram, PILFile},
    DiffMonitor,
};
use backend::{BackendType, Proof};
use executor::{
    constant_evaluator,
    witgen::{chain_callbacks, QueryCallback},
};
use log::Level;
use mktemp::Temp;
use number::{write_polys_csv_file, write_polys_file, CsvRenderMode, FieldElement};

use crate::{
    inputs_to_query_callback,
    util::{read_poly_set, FixedPolySet, WitnessPolySet},
};

pub struct GeneratedWitness<T: FieldElement> {
    pub pil: Analyzed<T>,
    pub constants: Vec<(String, Vec<T>)>,
    pub witness: Option<Vec<(String, Vec<T>)>>,
}

pub struct PilWithConstants<T: FieldElement> {
    pub pil: Analyzed<T>,
    pub constants: Vec<(String, Vec<T>)>,
}

pub struct ProofResult<T: FieldElement> {
    /// Constant columns, potentially incomplete (if success is false)
    pub constants: Vec<(String, Vec<T>)>,
    /// Witness columns, potentially None (if success is false)
    pub witness: Option<Vec<(String, Vec<T>)>>,
    /// Proof, potentially None (if success is false)
    pub proof: Option<Proof>,
    /// Serialized low level constraints, potentially None (if success is false)
    pub constraints_serialization: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Stage {
    AsmFile,
    AsmString,
    Parsed,
    Resolved,
    AnalyzedAsm,
    PilConstraints,
    Graph,
    Linked,
    PilFile,
    PilString,
    AnalyzedPil,
    OptimizedPil,
    PilWithConstants,
    GeneratedWitness,
    Proof,
}

enum Artifact<T: FieldElement> {
    /// The path to the .asm file.
    AsmFile(PathBuf),
    /// The contents of the .asm file, with an optional Path (for error messages).
    AsmString(Option<PathBuf>, String),
    /// The parsed .asm file, with an optional Path (for error messages).
    Parsed(Option<PathBuf>, ASMProgram<T>),
    /// The resolved .asm file.
    Resolved(ASMProgram<T>),
    /// The analyzed .asm file.
    AnalyzedAsm(AnalysisASMFile<T>),
    /// The pil constraints.
    PilConstraints(AnalysisASMFile<T>),
    /// The airgen graph.
    Graph(PILGraph<T>),
    /// The linked pil.
    Linked(PILFile<T>),
    /// The path to the .pil file.
    PilFile(PathBuf),
    /// The contents of the .pil file.
    PilString(String),
    /// The analyzed .pil file.
    AnaylyzedPil(Analyzed<T>),
    /// The optimized .pil file.
    OptimzedPil(Analyzed<T>),
    /// The optimized .pil file with constants.
    PilWithConstants(PilWithConstants<T>),
    /// The generated witness.
    GeneratedWitness(GeneratedWitness<T>),
    /// The proof (if successful)
    Proof(ProofResult<T>),
}

/// Optional Arguments for various stages of the pipeline.
#[derive(Default)]
struct Arguments<T: FieldElement> {
    /// Externally computed witness values for witness generation.
    external_witness_values: Vec<(String, Vec<T>)>,
    /// Callback for queries for witness generation.
    query_callback: Option<Box<dyn QueryCallback<T>>>,
    /// Backend to use for proving. If None, proving will fail.
    backend: Option<BackendType>,
    /// CSV render mode for witness generation.
    csv_render_mode: CsvRenderMode,
    /// Whether to export the witness as a CSV file.
    export_witness_csv: bool,
    /// The optional setup file to use for proving.
    setup_file: Option<PathBuf>,
    /// The optional existing proof file to use for aggregation.
    existing_proof_file: Option<PathBuf>,
}

pub struct Pipeline<T: FieldElement> {
    /// The current artifact. It is never None in practice, making it an Option is
    /// only necessary so that we can take ownership of it in advance().
    artifact: Option<Artifact<T>>,
    /// The diff monitor is used to track changes between pipeline stages.
    diff_monitor: DiffMonitor,
    /// Output directory for intermediate files. If None, no files are written.
    output_dir: Option<PathBuf>,
    /// The name of the pipeline. Used to name output files.
    name: Option<String>,
    /// Whether to overwrite existing files. If false, an error is returned if a file
    /// already exists.
    force_overwrite: bool,
    /// The log level to use for this pipeline.
    log_level: Level,
    // The output directory if `Pipeline::with_tmp_output` was called.
    // Note that there is some redundancy with `output_dir`, but the Temp
    // object has to live for the lifetime of the pipeline, so we keep it here.
    tmp_dir: Option<Temp>,
    /// Optional arguments for various stages of the pipeline.
    arguments: Arguments<T>,
}

impl<T> Default for Pipeline<T>
where
    T: FieldElement,
{
    fn default() -> Self {
        Pipeline {
            artifact: None,
            diff_monitor: DiffMonitor::default(),
            output_dir: None,
            log_level: Level::Debug,
            name: None,
            force_overwrite: false,
            tmp_dir: None,
            arguments: Arguments::default(),
        }
    }
}

/// A Powdr pipeline, going from a PIL or ASM file to a proof.
///
/// The pipeline steps are:
/// ```mermaid
///  graph TD
///      AsmFile --> AsmString
///      AsmString --> Parsed
///      Parsed --> Resolved
///      Resolved --> AnalyzedAsm
///      AnalyzedAsm --> PilConstraints
///      PilConstraints --> Graph
///      Graph --> Linked
///      Linked --> AnaylyzedPil
///      PilFile --> AnaylyzedPil
///      PilString --> AnaylyzedPil
///      AnaylyzedPil --> OptimzedPil
///      OptimzedPil --> PilWithConstants
///      PilWithConstants --> GeneratedWitness
///      GeneratedWitness --> Proof
/// ```
///
/// # Example
/// ```rust
/// use compiler::{pipeline::Pipeline, pipeline::Stage, verify, BackendType, test_util::resolve_test_file};
/// use std::path::PathBuf;
/// use number::GoldilocksField;
///
/// let mut pipeline = Pipeline::<GoldilocksField>::default()
///   .from_file(resolve_test_file("pil/fibonacci.pil"))
///   .with_backend(BackendType::PilStarkCli);
///
/// // Advance to some stage (which might have side effects)
/// pipeline.advance_to(Stage::OptimizedPil).unwrap();
///
/// // Get the result
/// let proof = pipeline.proof().unwrap();
/// ```
impl<T: FieldElement> Pipeline<T> {
    pub fn with_tmp_output(self) -> Self {
        let tmp_dir = mktemp::Temp::new_dir().unwrap();
        Pipeline {
            output_dir: Some(tmp_dir.to_path_buf()),
            tmp_dir: Some(tmp_dir),
            ..self
        }
    }

    pub fn with_output(self, output_dir: PathBuf, force_overwrite: bool) -> Self {
        Pipeline {
            output_dir: Some(output_dir),
            force_overwrite,
            ..self
        }
    }

    pub fn with_external_witness_values(
        mut self,
        external_witness_values: Vec<(String, Vec<T>)>,
    ) -> Self {
        self.arguments.external_witness_values = external_witness_values;
        self
    }
    pub fn with_witness_csv_settings(
        mut self,
        export_witness_csv: bool,
        csv_render_mode: CsvRenderMode,
    ) -> Self {
        self.arguments.export_witness_csv = export_witness_csv;
        self.arguments.csv_render_mode = csv_render_mode;
        self
    }

    pub fn add_query_callback(mut self, query_callback: Box<dyn QueryCallback<T>>) -> Self {
        let query_callback = match self.arguments.query_callback {
            Some(old_callback) => Box::new(chain_callbacks(old_callback, query_callback)),
            None => query_callback,
        };
        self.arguments.query_callback = Some(query_callback);
        self
    }

    pub fn with_prover_inputs(self, inputs: Vec<T>) -> Self {
        self.add_query_callback(Box::new(inputs_to_query_callback(inputs)))
    }

    pub fn with_backend(mut self, backend: BackendType) -> Self {
        self.arguments.backend = Some(backend);
        self
    }

    pub fn with_setup_file(mut self, setup_file: Option<PathBuf>) -> Self {
        self.arguments.setup_file = setup_file;
        self
    }

    pub fn with_existing_proof_file(mut self, existing_proof_file: Option<PathBuf>) -> Self {
        self.arguments.existing_proof_file = existing_proof_file;
        self
    }

    pub fn with_name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    pub fn from_file(self, asm_file: PathBuf) -> Self {
        if asm_file.extension().unwrap() == "asm" {
            self.from_asm_file(asm_file)
        } else {
            self.from_pil_file(asm_file)
        }
    }

    pub fn from_asm_file(self, asm_file: PathBuf) -> Self {
        let name = self.name.or(Some(Self::name_from_path(&asm_file)));
        Pipeline {
            artifact: Some(Artifact::AsmFile(asm_file)),
            name,
            ..self
        }
    }

    pub fn from_asm_string(self, asm_string: String, path: Option<PathBuf>) -> Self {
        let name = self.name.or(path.as_ref().map(|p| Self::name_from_path(p)));
        Pipeline {
            artifact: Some(Artifact::AsmString(path, asm_string)),
            name,
            ..self
        }
    }

    pub fn from_pil_file(self, pil_file: PathBuf) -> Self {
        let name = self.name.or(Some(Self::name_from_path(&pil_file)));
        Pipeline {
            artifact: Some(Artifact::PilFile(pil_file)),
            name,
            ..self
        }
    }

    pub fn from_pil_string(self, pil_string: String) -> Self {
        Pipeline {
            artifact: Some(Artifact::PilString(pil_string)),
            ..self
        }
    }

    /// Reads a previously generated witness from the provided directory and
    /// advances the pipeline to the `GeneratedWitness` stage.
    pub fn read_generated_witness(mut self, directory: &Path) -> Self {
        self.advance_to(Stage::OptimizedPil).unwrap();

        let pil = match self.artifact.unwrap() {
            Artifact::OptimzedPil(pil) => pil,
            _ => panic!(),
        };

        // Can't use self.name() because self is partially moved...
        let name = self.name.as_ref().expect("name must be set!");
        let (fixed, degree_fixed) = read_poly_set::<FixedPolySet, T>(&pil, directory, name);
        let (witness, degree_witness) = read_poly_set::<WitnessPolySet, T>(&pil, directory, name);
        assert_eq!(degree_fixed, degree_witness);

        Pipeline {
            artifact: Some(Artifact::GeneratedWitness(GeneratedWitness {
                pil,
                constants: fixed,
                witness: Some(witness),
            })),
            ..self
        }
    }

    fn name_from_path(path: &Path) -> String {
        path.file_stem().unwrap().to_str().unwrap().to_string()
    }

    fn log(&self, msg: &str) {
        log::log!(self.log_level, "{}", msg);
    }

    #[allow(clippy::print_stderr)]
    fn advance(&mut self) -> Result<(), Vec<String>> {
        let artifact = std::mem::take(&mut self.artifact).unwrap();
        self.artifact = Some(match artifact {
            Artifact::AsmFile(path) => Artifact::AsmString(
                Some(path.clone()),
                fs::read_to_string(&path).map_err(|e| {
                    vec![format!("Error reading .asm file: {}\n{e}", path.display())]
                })?,
            ),
            Artifact::AsmString(path, asm_string) => {
                let parsed_asm = parser::parse_asm(None, &asm_string).unwrap_or_else(|err| {
                    match path.as_ref() {
                        Some(path) => eprintln!("Error parsing .asm file: {}", path.display()),
                        None => eprintln!("Error parsing .asm file:"),
                    }
                    err.output_to_stderr();
                    panic!();
                });
                self.diff_monitor.push(&parsed_asm);
                Artifact::Parsed(path, parsed_asm)
            }
            Artifact::Parsed(path, parsed) => {
                self.log("Resolve imports");
                let resolved = importer::resolve(path, parsed).map_err(|e| vec![e])?;
                self.diff_monitor.push(&resolved);
                Artifact::Resolved(resolved)
            }
            Artifact::Resolved(resolved) => {
                self.log("Run analysis");
                self.log("Analysis done");
                let analyzed_asm = analysis::analyze(resolved, &mut self.diff_monitor)?;
                log::trace!("{analyzed_asm}");
                Artifact::AnalyzedAsm(analyzed_asm)
            }
            Artifact::AnalyzedAsm(analyzed_asm) => Artifact::PilConstraints(
                convert_analyzed_to_pil_constraints(analyzed_asm, &mut self.diff_monitor),
            ),
            Artifact::PilConstraints(analyzed_asm) => {
                self.log("Run airgen");
                let graph = airgen::compile(analyzed_asm);
                self.diff_monitor.push(&graph);
                self.log("Airgen done");
                log::trace!("{graph}");
                Artifact::Graph(graph)
            }
            Artifact::Graph(graph) => {
                self.log("Run linker");
                self.log("Linker done");
                let linked = linker::link(graph)?;
                self.diff_monitor.push(&linked);
                log::trace!("{linked}");
                self.maybe_write_pil(&linked, "")?;
                Artifact::Linked(linked)
            }
            Artifact::Linked(linked) => {
                Artifact::AnaylyzedPil(pil_analyzer::analyze_string(&format!("{linked}")))
            }
            Artifact::PilFile(pil_file) => Artifact::AnaylyzedPil(pil_analyzer::analyze(&pil_file)),
            Artifact::PilString(pil_string) => {
                Artifact::AnaylyzedPil(pil_analyzer::analyze_string(&pil_string))
            }
            Artifact::AnaylyzedPil(analyzed_pil) => {
                self.log("Optimizing pil...");
                let optimized = pilopt::optimize(analyzed_pil);
                self.maybe_write_pil(&optimized, "_opt")?;
                Artifact::OptimzedPil(optimized)
            }
            Artifact::OptimzedPil(pil) => {
                self.log("Evaluating fixed columns...");
                let start = Instant::now();
                let constants = constant_evaluator::generate(&pil);
                let constants = constants
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), v))
                    .collect::<Vec<_>>();
                self.maybe_write_constants(&constants)?;
                self.log(&format!("Took {}", start.elapsed().as_secs_f32()));
                Artifact::PilWithConstants(PilWithConstants { pil, constants })
            }
            Artifact::PilWithConstants(PilWithConstants { pil, constants }) => {
                let witness = (pil.constant_count() == constants.len()).then(|| {
                    self.log("Deducing witness columns...");
                    let start = Instant::now();
                    let external_witness_values =
                        std::mem::take(&mut self.arguments.external_witness_values);
                    let query_callback = self
                        .arguments
                        .query_callback
                        .take()
                        .unwrap_or_else(|| Box::new(executor::witgen::unused_query_callback()));
                    let witness =
                        executor::witgen::WitnessGenerator::new(&pil, &constants, query_callback)
                            .with_external_witness_values(external_witness_values)
                            .generate();

                    self.log(&format!("Took {}", start.elapsed().as_secs_f32()));
                    witness
                        .into_iter()
                        .map(|(name, c)| (name.to_string(), c))
                        .collect::<Vec<_>>()
                });

                self.maybe_write_witness(&constants, &witness)?;
                Artifact::GeneratedWitness(GeneratedWitness {
                    pil,
                    constants,
                    witness,
                })
            }
            Artifact::GeneratedWitness(GeneratedWitness {
                pil,
                constants,
                witness,
            }) => {
                let backend = self
                    .arguments
                    .backend
                    .expect("backend must be set before calling proving!");
                let factory = backend.factory::<T>();
                let backend = if let Some(path) = self.arguments.setup_file.as_ref() {
                    let mut file = fs::File::open(path).unwrap();
                    factory.create_from_setup(&mut file).unwrap()
                } else {
                    factory.create(pil.degree())
                };

                let existing_proof = self.arguments.existing_proof_file.as_ref().map(|path| {
                    let mut buf = Vec::new();
                    fs::File::open(path).unwrap().read_to_end(&mut buf).unwrap();
                    buf
                });

                // Even if we don't have all constants and witnesses, some backends will
                // still output the constraint serialization.
                let (proof, constraints_serialization) = backend.prove(
                    &pil,
                    &constants,
                    witness.as_deref().unwrap_or_default(),
                    existing_proof,
                );

                let proof_result = ProofResult {
                    constants,
                    witness,
                    proof,
                    constraints_serialization,
                };

                self.maybe_wite_proof(&proof_result)?;

                Artifact::Proof(proof_result)
            }
            Artifact::Proof(_) => panic!("Last pipeline step!"),
        });
        Ok(())
    }

    /// Returns the path to the output file if the output directory is set.
    /// Fails if the file already exists and `force_overwrite` is false.
    fn path_if_should_write<F: FnOnce(&str) -> String>(
        &self,
        file_name_from_pipeline_name: F,
    ) -> Result<Option<PathBuf>, Vec<String>> {
        self.output_dir
            .as_ref()
            .map(|output_dir| {
                let name = self
                    .name
                    .as_ref()
                    .expect("name must be set if output_dir is set");
                let path = output_dir.join(file_name_from_pipeline_name(name));
                if path.exists() && !self.force_overwrite {
                    Err(vec![format!(
                        "{} already exists! Use --force to overwrite.",
                        path.to_str().unwrap()
                    )])?;
                }
                log::info!("Writing {}.", path.to_str().unwrap());
                Ok(path)
            })
            .transpose()
    }

    fn maybe_write_pil<C: Display>(&self, content: &C, suffix: &str) -> Result<(), Vec<String>> {
        if let Some(path) = self.path_if_should_write(|name| format!("{name}{suffix}.pil"))? {
            fs::write(&path, format!("{content}"))
                .map_err(|e| vec![format!("Error writing {}: {e}", path.to_str().unwrap())])?;
        }
        Ok(())
    }

    fn maybe_write_constants(&self, constants: &[(String, Vec<T>)]) -> Result<(), Vec<String>> {
        if let Some(path) = self.path_if_should_write(|name| format!("{name}_constants.bin"))? {
            write_polys_file(
                &mut BufWriter::new(&mut fs::File::create(path).unwrap()),
                constants,
            );
        }
        Ok(())
    }

    fn maybe_write_witness(
        &self,
        fixed: &[(String, Vec<T>)],
        witness: &Option<Vec<(String, Vec<T>)>>,
    ) -> Result<(), Vec<String>> {
        if let Some(witness) = witness.as_ref() {
            if let Some(path) = self.path_if_should_write(|name| format!("{name}_commits.bin"))? {
                write_polys_file(
                    &mut BufWriter::new(&mut fs::File::create(path).unwrap()),
                    witness,
                );
            }
        }

        if self.arguments.export_witness_csv {
            if let Some(path) = self.path_if_should_write(|name| format!("{name}_columns.csv"))? {
                let columns = fixed
                    .iter()
                    .chain(match witness.as_ref() {
                        Some(witness) => witness.iter(),
                        None => [].iter(),
                    })
                    .collect::<Vec<_>>();

                let mut csv_file = fs::File::create(path).map_err(|e| vec![format!("{}", e)])?;
                let mut csv_writer = BufWriter::new(&mut csv_file);

                write_polys_csv_file(&mut csv_writer, self.arguments.csv_render_mode, &columns);
            }
        }

        Ok(())
    }

    fn maybe_wite_proof(&self, proof_result: &ProofResult<T>) -> Result<(), Vec<String>> {
        if let Some(constraints_serialization) = &proof_result.constraints_serialization {
            if let Some(path) =
                self.path_if_should_write(|name| format!("{name}_constraints.json"))?
            {
                let mut file = fs::File::create(path).unwrap();
                file.write_all(constraints_serialization.as_bytes())
                    .unwrap();
            }
        }
        if let Some(proof) = &proof_result.proof {
            let fname = if self.arguments.existing_proof_file.is_some() {
                "proof_aggr.bin"
            } else {
                "proof.bin"
            };
            if let Some(path) = self.path_if_should_write(|name| format!("{name}_{fname}"))? {
                let mut proof_file = fs::File::create(path).unwrap();
                proof_file.write_all(proof).unwrap();
            }
        }

        Ok(())
    }

    fn stage(&self) -> Stage {
        match self.artifact.as_ref().unwrap() {
            Artifact::AsmFile(_) => Stage::AsmFile,
            Artifact::AsmString(_, _) => Stage::AsmString,
            Artifact::Parsed(_, _) => Stage::Parsed,
            Artifact::Resolved(_) => Stage::Resolved,
            Artifact::AnalyzedAsm(_) => Stage::AnalyzedAsm,
            Artifact::PilConstraints(_) => Stage::PilConstraints,
            Artifact::Graph(_) => Stage::Graph,
            Artifact::Linked(_) => Stage::Linked,
            Artifact::PilFile(_) => Stage::PilFile,
            Artifact::PilString(_) => Stage::PilString,
            Artifact::AnaylyzedPil(_) => Stage::AnalyzedPil,
            Artifact::OptimzedPil(_) => Stage::OptimizedPil,
            Artifact::PilWithConstants(_) => Stage::PilWithConstants,
            Artifact::GeneratedWitness(_) => Stage::GeneratedWitness,
            Artifact::Proof(_) => Stage::Proof,
        }
    }

    pub fn advance_to(&mut self, target_stage: Stage) -> Result<(), Vec<String>> {
        while self.stage() != target_stage {
            self.advance()?;
        }
        Ok(())
    }

    pub fn asm_string(mut self) -> Result<String, Vec<String>> {
        self.advance_to(Stage::AsmString)?;
        match self.artifact.unwrap() {
            Artifact::AsmString(_, asm_string) => Ok(asm_string),
            _ => panic!(),
        }
    }

    pub fn analyzed_asm(mut self) -> Result<AnalysisASMFile<T>, Vec<String>> {
        self.advance_to(Stage::AnalyzedAsm)?;
        let Artifact::AnalyzedAsm(analyzed_asm) = self.artifact.unwrap() else {
            panic!()
        };
        Ok(analyzed_asm)
    }

    pub fn analyzed_asm_ref(&mut self) -> Result<&AnalysisASMFile<T>, Vec<String>> {
        self.advance_to(Stage::AnalyzedAsm)?;
        match self.artifact.as_ref().unwrap() {
            Artifact::AnalyzedAsm(analyzed_asm) => Ok(analyzed_asm),
            _ => panic!(),
        }
    }

    pub fn optimized_pil(mut self) -> Result<Analyzed<T>, Vec<String>> {
        self.advance_to(Stage::OptimizedPil)?;
        let Artifact::OptimzedPil(optimized_pil) = self.artifact.unwrap() else {
            panic!()
        };
        Ok(optimized_pil)
    }

    pub fn optimized_pil_with_constants(mut self) -> Result<PilWithConstants<T>, Vec<String>> {
        self.advance_to(Stage::PilWithConstants)?;
        let Artifact::PilWithConstants(pil_with_constants) = self.artifact.unwrap() else {
            panic!()
        };
        Ok(pil_with_constants)
    }

    pub fn generated_witness(mut self) -> Result<GeneratedWitness<T>, Vec<String>> {
        self.advance_to(Stage::GeneratedWitness)?;
        let Artifact::GeneratedWitness(generated_witness) = self.artifact.unwrap() else {
            panic!()
        };
        Ok(generated_witness)
    }

    pub fn proof(mut self) -> Result<ProofResult<T>, Vec<String>> {
        self.advance_to(Stage::Proof)?;
        let Artifact::Proof(proof) = self.artifact.unwrap() else {
            panic!()
        };
        Ok(proof)
    }

    pub fn tmp_dir(&self) -> &Path {
        self.tmp_dir.as_ref().unwrap()
    }

    pub fn name(&self) -> &str {
        self.name.as_ref().unwrap()
    }
}

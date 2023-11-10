//! The main powdr lib, used to compile from assembly to PIL

#![deny(clippy::print_stdout)]

use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::time::Instant;

use analysis::analyze;
use analysis::convert_analyzed_to_pil_constraints;
use ast::analyzed::Analyzed;
use ast::DiffMonitor;

pub mod util;
mod verify;

use ast::asm_analysis::AnalysisASMFile;
pub use backend::{BackendType, Proof};
use executor::witgen::QueryCallback;
pub use verify::{
    verify, verify_asm_string, write_commits_to_fs, write_constants_to_fs, write_constraints_to_fs,
};

use ast::parsed::PILFile;
use executor::constant_evaluator;
use number::FieldElement;

pub fn no_callback<T>() -> Option<fn(&str) -> Option<T>> {
    None
}

/// Compiles a .pil or .asm file and runs witness generation.
/// If the file ends in .asm, converts it to .pil first.
/// Returns the compilation result if any compilation took place.
pub fn compile_pil_or_asm<T: FieldElement>(
    file_name: &str,
    inputs: Vec<T>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
    external_witness_values: Vec<(&str, Vec<T>)>,
) -> Result<Option<CompilationResult<T>>, Vec<String>> {
    if file_name.ends_with(".asm") {
        compile_asm(
            file_name,
            inputs,
            output_dir,
            force_overwrite,
            prove_with,
            external_witness_values,
        )
    } else {
        Ok(Some(compile_pil(
            Path::new(file_name),
            output_dir,
            inputs_to_query_callback(&inputs),
            prove_with,
            external_witness_values,
        )))
    }
}

pub fn analyze_pil<T: FieldElement>(pil_file: &Path) -> Analyzed<T> {
    pil_analyzer::analyze(pil_file)
}

/// Compiles a .pil file to its json form and also tries to generate
/// constants and committed polynomials.
/// @returns a compilation result, containing witness and fixed columns
/// if they could be successfully generated.
pub fn compile_pil<T: FieldElement, Q: QueryCallback<T>>(
    pil_file: &Path,
    output_dir: &Path,
    query_callback: Q,
    prove_with: Option<BackendType>,
    external_witness_values: Vec<(&str, Vec<T>)>,
) -> CompilationResult<T> {
    compile(
        pil_analyzer::analyze(pil_file),
        pil_file.file_name().unwrap(),
        output_dir,
        query_callback,
        prove_with,
        external_witness_values,
    )
}

/// Compiles a given PIL and tries to generate fixed and witness columns.
/// @returns a compilation result, containing witness and fixed columns
pub fn compile_pil_ast<T: FieldElement, Q: QueryCallback<T>>(
    pil: &PILFile<T>,
    file_name: &OsStr,
    output_dir: &Path,
    query_callback: Q,
    prove_with: Option<BackendType>,
    external_witness_values: Vec<(&str, Vec<T>)>,
) -> CompilationResult<T> {
    // TODO exporting this to string as a hack because the parser
    // is tied into the analyzer due to imports.
    compile(
        pil_analyzer::analyze_string(&format!("{pil}")),
        file_name,
        output_dir,
        query_callback,
        prove_with,
        external_witness_values,
    )
}

/// Compiles a .asm file, outputs the PIL on stdout and tries to generate
/// fixed and witness columns.
/// @returns a compilation result if any compilation was done.
pub fn compile_asm<T: FieldElement>(
    file_name: &str,
    inputs: Vec<T>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
    external_witness_values: Vec<(&str, Vec<T>)>,
) -> Result<Option<CompilationResult<T>>, Vec<String>> {
    let contents = fs::read_to_string(file_name).unwrap();
    Ok(compile_asm_string(
        file_name,
        &contents,
        &inputs,
        None,
        output_dir,
        force_overwrite,
        prove_with,
        external_witness_values,
    )?
    .1)
}

#[allow(clippy::print_stderr)]
pub fn compile_asm_string_to_analyzed_ast<T: FieldElement>(
    file_name: &str,
    contents: &str,
    monitor: &mut DiffMonitor,
) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let parsed = parser::parse_asm(Some(file_name), contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .asm file:");
        err.output_to_stderr();
        panic!();
    });
    log::debug!("Resolve imports");
    let resolved =
        importer::resolve(Some(PathBuf::from(file_name)), parsed).map_err(|e| vec![e])?;
    log::debug!("Run analysis");
    let analyzed = analyze(resolved, monitor)?;
    log::debug!("Analysis done");
    log::trace!("{analyzed}");

    Ok(analyzed)
}

#[allow(clippy::too_many_arguments)]
pub fn convert_analyzed_to_pil<T: FieldElement>(
    file_name: &str,
    monitor: &mut DiffMonitor,
    analyzed: AnalysisASMFile<T>,
    inputs: &[T],
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
    external_witness_values: Vec<(&str, Vec<T>)>,
) -> Result<(PathBuf, Option<CompilationResult<T>>), Vec<String>> {
    let constraints = convert_analyzed_to_pil_constraints(analyzed, monitor);
    log::debug!("Run airgen");
    let graph = airgen::compile(constraints);
    log::debug!("Airgen done");
    log::trace!("{graph}");
    log::debug!("Run linker");
    let pil = linker::link(graph)?;
    log::debug!("Linker done");
    log::trace!("{pil}");

    let pil_file_name = format!(
        "{}.pil",
        Path::new(file_name).file_stem().unwrap().to_str().unwrap()
    );

    let pil_file_path = output_dir.join(pil_file_name);
    if pil_file_path.exists() && !force_overwrite {
        eprintln!(
            "Target file {} already exists. Not overwriting.",
            pil_file_path.to_str().unwrap()
        );
        return Ok((pil_file_path, None));
    }

    fs::write(&pil_file_path, format!("{pil}")).unwrap();

    let pil_file_name = pil_file_path.file_name().unwrap();
    Ok((
        pil_file_path.clone(),
        Some(compile_pil_ast(
            &pil,
            pil_file_name,
            output_dir,
            inputs_to_query_callback(inputs),
            prove_with,
            external_witness_values,
        )),
    ))
}

pub type AnalyzedASTHook<'a, T> = &'a mut dyn FnMut(&AnalysisASMFile<T>);

/// Compiles the contents of a .asm file, outputs the PIL on stdout and tries to generate
/// fixed and witness columns.
///
/// Returns the relative pil file name and the compilation result if any compilation was done.
#[allow(clippy::too_many_arguments)]
pub fn compile_asm_string<T: FieldElement>(
    file_name: &str,
    contents: &str,
    inputs: &[T],
    analyzed_hook: Option<AnalyzedASTHook<T>>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
    external_witness_values: Vec<(&str, Vec<T>)>,
) -> Result<(PathBuf, Option<CompilationResult<T>>), Vec<String>> {
    let mut monitor = DiffMonitor::default();
    let analyzed = compile_asm_string_to_analyzed_ast(file_name, contents, &mut monitor)?;
    if let Some(hook) = analyzed_hook {
        hook(&analyzed);
    };
    convert_analyzed_to_pil(
        file_name,
        &mut monitor,
        analyzed,
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
        external_witness_values,
    )
}

pub struct CompilationResult<T: FieldElement> {
    /// Constant columns, potentially incomplete (if success is false)
    pub constants: Vec<(String, Vec<T>)>,
    /// Witness columns, potentially None (if success is false)
    pub witness: Option<Vec<(String, Vec<T>)>>,
    /// Proof, potentially None (if success is false)
    pub proof: Option<Proof>,
    /// Serialized low level constraints, potentially None (if success is false)
    pub constraints_serialization: Option<String>,
}

/// Optimizes a given pil and tries to generate constants and committed polynomials.
/// @returns a compilation result, containing witness and fixed columns, if successful.
fn compile<T: FieldElement, Q: QueryCallback<T>>(
    analyzed: Analyzed<T>,
    file_name: &OsStr,
    output_dir: &Path,
    query_callback: Q,
    prove_with: Option<BackendType>,
    external_witness_values: Vec<(&str, Vec<T>)>,
) -> CompilationResult<T> {
    log::info!("Optimizing pil...");
    let analyzed = pilopt::optimize(analyzed);
    let optimized_pil_file_name = output_dir.join(format!(
        "{}_opt.pil",
        Path::new(file_name).file_stem().unwrap().to_str().unwrap()
    ));
    fs::write(optimized_pil_file_name.clone(), format!("{analyzed}")).unwrap();
    log::info!("Wrote {}.", optimized_pil_file_name.to_str().unwrap());
    let start = Instant::now();
    log::info!("Evaluating fixed columns...");
    let constants = constant_evaluator::generate(&analyzed);
    log::info!("Took {}", start.elapsed().as_secs_f32());

    let witness = (analyzed.constant_count() == constants.len()).then(|| {
        log::info!("Deducing witness columns...");
        let commits =
            executor::witgen::WitnessGenerator::new(&analyzed, &constants, query_callback)
                .with_external_witness_values(external_witness_values)
                .generate();

        commits
            .into_iter()
            .map(|(name, c)| (name.to_string(), c))
            .collect::<Vec<_>>()
    });

    let constants = constants
        .into_iter()
        .map(|(name, c)| (name.to_string(), c))
        .collect::<Vec<_>>();

    // Even if we don't have all constants and witnesses, some backends will
    // still output the constraint serialization.
    let (proof, constraints_serialization) = if let Some(backend) = prove_with {
        let factory = backend.factory::<T>();
        let backend = factory.create(analyzed.degree());

        backend.prove(
            &analyzed,
            &constants,
            witness.as_deref().unwrap_or_default(),
            None,
        )
    } else {
        (None, None)
    };

    let constants = constants
        .into_iter()
        .map(|(name, c)| (name.to_owned(), c))
        .collect();

    let witness = witness.map(|v| {
        v.into_iter()
            .map(|(name, c)| (name.to_owned(), c))
            .collect()
    });

    CompilationResult {
        constants,
        witness,
        proof,
        constraints_serialization,
    }
}

#[allow(clippy::print_stdout)]
pub fn inputs_to_query_callback<T: FieldElement>(inputs: &[T]) -> impl Fn(&str) -> Option<T> + '_ {
    move |query: &str| -> Option<T> {
        let items = query.split(',').map(|s| s.trim()).collect::<Vec<_>>();
        match items[0] {
            "\"input\"" => {
                assert_eq!(items.len(), 2);
                let index = items[1].parse::<usize>().unwrap();
                let value = inputs.get(index).cloned();
                if let Some(value) = value {
                    log::trace!("Input query: Index {index} -> {value}");
                } else {
                    log::warn!("Not enough inputs provided! Index {index} out of bounds")
                }
                value
            }
            "\"print_char\"" => {
                assert_eq!(items.len(), 2);
                print!("{}", items[1].parse::<u8>().unwrap() as char);
                Some(0.into())
            }
            "\"hint\"" => {
                assert_eq!(items.len(), 2);
                Some(T::from_str(items[1]))
            }
            _ => None,
        }
    }
}

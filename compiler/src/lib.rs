//! The main powdr lib, used to compile from assembly to PIL

use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::time::Instant;

use ast::analyzed::Analyzed;

pub mod util;
mod verify;

use analysis::analyze;
pub use backend::{BackendType, Proof};
pub use verify::{verify, verify_asm_string};

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
) -> Option<CompilationResult<T>> {
    if file_name.ends_with(".asm") {
        compile_asm(file_name, inputs, output_dir, force_overwrite, prove_with)
    } else {
        Some(compile_pil(
            Path::new(file_name),
            output_dir,
            Some(inputs_to_query_callback(inputs)),
            prove_with,
        ))
    }
}

pub fn analyze_pil<T: FieldElement>(pil_file: &Path) -> Analyzed<T> {
    pil_analyzer::analyze(pil_file)
}

/// Compiles a .pil file to its json form and also tries to generate
/// constants and committed polynomials.
/// @returns a compilation result, containing witness and fixed columns
/// if they could be successfully generated.
pub fn compile_pil<T: FieldElement, QueryCallback>(
    pil_file: &Path,
    output_dir: &Path,
    query_callback: Option<QueryCallback>,
    prove_with: Option<BackendType>,
) -> CompilationResult<T>
where
    QueryCallback: FnMut(&str) -> Option<T> + Sync + Send,
{
    compile(
        pil_analyzer::analyze(pil_file),
        pil_file.file_name().unwrap(),
        output_dir,
        query_callback,
        prove_with,
    )
}

/// Compiles a given PIL and tries to generate fixed and witness columns.
/// @returns a compilation result, containing witness and fixed columns
pub fn compile_pil_ast<T: FieldElement, QueryCallback>(
    pil: &PILFile<T>,
    file_name: &OsStr,
    output_dir: &Path,
    query_callback: Option<QueryCallback>,
    prove_with: Option<BackendType>,
) -> CompilationResult<T>
where
    QueryCallback: FnMut(&str) -> Option<T> + Sync + Send,
{
    // TODO exporting this to string as a hack because the parser
    // is tied into the analyzer due to imports.
    compile(
        pil_analyzer::analyze_string(&format!("{pil}")),
        file_name,
        output_dir,
        query_callback,
        prove_with,
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
) -> Option<CompilationResult<T>> {
    let contents = fs::read_to_string(file_name).unwrap();
    compile_asm_string(
        file_name,
        &contents,
        inputs,
        output_dir,
        force_overwrite,
        prove_with,
    )
    .1
}

/// Compiles the contents of a .asm file, outputs the PIL on stdout and tries to generate
/// fixed and witness columns.
///
/// Returns the relative pil file name and the compilation result if any compilation was done.
pub fn compile_asm_string<T: FieldElement>(
    file_name: &str,
    contents: &str,
    inputs: Vec<T>,
    output_dir: &Path,
    force_overwrite: bool,
    prove_with: Option<BackendType>,
) -> (PathBuf, Option<CompilationResult<T>>) {
    let parsed = parser::parse_asm(Some(file_name), contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .asm file:");
        err.output_to_stderr();
        panic!();
    });
    let analysed = analyze(parsed).unwrap();
    let graph = asm_to_pil::compile(analysed);
    let pil = linker::link(graph);

    let pil_file_name = format!(
        "{}.pil",
        Path::new(file_name).file_stem().unwrap().to_str().unwrap()
    );

    let pil_file_path = output_dir.join(pil_file_name);
    if pil_file_path.exists() && !force_overwrite {
        eprint!(
            "Target file {} already exists. Not overwriting.",
            pil_file_path.to_str().unwrap()
        );
        return (pil_file_path, None);
    }
    fs::write(pil_file_path.clone(), format!("{pil}")).unwrap();

    let pil_file_name = pil_file_path.file_name().unwrap();
    (
        pil_file_path.clone(),
        Some(compile_pil_ast(
            &pil,
            pil_file_name,
            output_dir,
            Some(inputs_to_query_callback(inputs)),
            prove_with,
        )),
    )
}

pub struct CompilationResult<T: FieldElement> {
    /// Constant columns, potentially incomplete (if success is false)
    pub constants: Vec<(String, Vec<T>)>,
    /// Witness columns, potentially None (if success is false)
    pub witness: Option<Vec<(String, Vec<T>)>>,
}

/// Optimizes a given pil and tries to generate constants and committed polynomials.
/// @returns a compilation result, containing witness and fixed columns, if successful.
fn compile<T: FieldElement, QueryCallback>(
    analyzed: Analyzed<T>,
    file_name: &OsStr,
    output_dir: &Path,
    query_callback: Option<QueryCallback>,
    prove_with: Option<BackendType>,
) -> CompilationResult<T>
where
    QueryCallback: FnMut(&str) -> Option<T> + Send + Sync,
{
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
    let (constants, degree) = constant_evaluator::generate(&analyzed);
    log::info!("Took {}", start.elapsed().as_secs_f32());

    let witness = (analyzed.constant_count() == constants.len()).then(|| {
        log::info!("Deducing witness columns...");
        let commits = executor::witgen::generate(&analyzed, degree, &constants, query_callback);

        commits
            .into_iter()
            .map(|(name, c)| (name, c))
            .collect::<Vec<_>>()
    });

    // Even if we don't have all constants and witnesses, some backends will
    // still output useful stuff.
    if let Some(backend) = prove_with {
        let factory = backend.factory::<T>();
        let backend = factory.create(degree);
        if let Err(error) = backend.prove(
            &analyzed,
            &constants,
            witness.as_deref().unwrap_or_default(),
            None,
            Some(output_dir),
        ) {
            log::warn!("Proof generation failed: {error}");
        }
    }

    let constants = constants
        .into_iter()
        .map(|(name, c)| (name.to_owned(), c))
        .collect();

    let witness = witness.map(|v| {
        v.into_iter()
            .map(|(name, c)| (name.to_owned(), c))
            .collect()
    });

    CompilationResult { constants, witness }
}

pub fn inputs_to_query_callback<T: FieldElement>(inputs: Vec<T>) -> impl Fn(&str) -> Option<T> {
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
            _ => None,
        }
    }
}

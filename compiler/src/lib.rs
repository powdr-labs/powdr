//! The main powdr lib, used to compile from assembly to PIL

#![deny(clippy::print_stdout)]

use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::time::Instant;

use ast::analyzed::Analyzed;

pub use backend::{BackendType, Proof};
use executor::witgen::QueryCallback;

use pil_analyzer::pil_analyzer::inline_intermediate_polynomials;

use ast::parsed::PILFile;
use executor::constant_evaluator;
use number::FieldElement;

pub fn no_callback<T>() -> Option<fn(&str) -> Option<T>> {
    None
}

/// Compiles a .pil or .asm file and runs witness generation.
/// If the file ends in .asm, converts it to .pil first.
/// Returns the compilation result if any compilation took place.
pub fn compile_pil_root<T: FieldElement>(
    file_name: &str,
    inputs: Vec<T>,
    output_dir: &Path,
    _force_overwrite: bool,
    prove_with: Option<BackendType>,
    bname: Option<String>,
) -> Result<Option<CompilationResult<T>>, Vec<String>> {
    Ok(Some(compile_pil(
        Path::new(file_name),
        output_dir,
        inputs_to_query_callback(inputs),
        prove_with,
        bname,
    )))
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
    bname: Option<String>,
) -> CompilationResult<T> {
    compile(
        pil_analyzer::analyze(pil_file),
        pil_file.file_name().unwrap(),
        output_dir,
        query_callback,
        prove_with,
        bname,
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
    bname: Option<String>,
) -> CompilationResult<T> {
    // TODO exporting this to string as a hack because the parser
    // is tied into the analyzer due to imports.
    compile(
        pil_analyzer::analyze_string(&format!("{pil}")),
        file_name,
        output_dir,
        query_callback,
        prove_with,
        bname,
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
fn compile<T: FieldElement, Q: QueryCallback<T>>(
    analyzed: Analyzed<T>,
    file_name: &OsStr,
    output_dir: &Path,
    _query_callback: Q,
    prove_with: Option<BackendType>,
    bname: Option<String>,
) -> CompilationResult<T> {
    log::info!("Optimizing pil...");
    // let analyzed = pilopt::optimize(analyzed);

    // md: we inline intermediate polynomials here, as honk does not have a notion of an intermediate
    let mut mut_analyzed = analyzed;
    mut_analyzed.identities = inline_intermediate_polynomials(&mut_analyzed);

    let optimized_pil_file_name = output_dir.join(format!(
        "{}_opt.pil",
        Path::new(file_name).file_stem().unwrap().to_str().unwrap()
    ));
    fs::write(optimized_pil_file_name.clone(), format!("{mut_analyzed}")).unwrap();
    log::info!("Wrote {}.", optimized_pil_file_name.to_str().unwrap());
    let start = Instant::now();
    log::info!("Evaluating fixed columns...");
    let (constants, degree) = constant_evaluator::generate(&mut_analyzed);
    log::info!("Took {}", start.elapsed().as_secs_f32());

    let witness_names = mut_analyzed
        .committed_polys_in_source_order()
        .into_iter()
        .map(|(sym, _)| sym.absolute_name.clone())
        .collect::<Vec<_>>();

    // NOTE: temporarily just append a vector to the end such that it is in the expected form for the backend
    let witness_in_powdr_form: Vec<(&str, Vec<T>)> = witness_names
        .iter()
        .map(|name| (name.as_str(), vec![]))
        .collect();

    // Even if we don't have all constants and witnesses, some backends will
    // still output the constraint serialization.
    if let Some(backend) = prove_with {
        let factory = backend.factory::<T>();
        let backend = factory.create(degree);

        backend.prove(
            &mut_analyzed,
            &constants,
            &witness_in_powdr_form,
            None,
            bname,
        );
    }

    let constants = constants
        .into_iter()
        .map(|(name, c)| (name.to_owned(), c))
        .collect();

    CompilationResult {
        constants,
        witness: None,
    }
}

#[allow(clippy::print_stdout)]
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
            "\"hint\"" => {
                assert_eq!(items.len(), 2);
                Some(T::from_str(items[1]))
            }
            _ => None,
        }
    }
}

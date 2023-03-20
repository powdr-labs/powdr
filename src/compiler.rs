use std::fs;
use std::io::{BufWriter, Write};
use std::path::Path;

use itertools::Itertools;
use num_bigint::Sign;

use crate::number::{abstract_to_degree, AbstractNumberType, DegreeType};
use crate::parser::ast::PILFile;
use crate::{analyzer, asm_compiler, commit_evaluator, constant_evaluator, json_exporter};

pub fn no_callback() -> Option<fn(&str) -> Option<AbstractNumberType>> {
    None
}

/// Compiles a .pil file to its json form and also tries to generate
/// constants and committed polynomials.
/// @returns true if all committed/witness and constant/fixed polynomials
/// could be generated.
pub fn compile_pil(
    pil_file: &Path,
    output_dir: &Path,
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
) -> bool {
    compile(
        &analyzer::analyze(pil_file),
        pil_file.file_name().unwrap().to_str().unwrap(),
        output_dir,
        query_callback,
        false,
    )
}

pub fn compile_pil_ast(
    pil: &PILFile,
    file_name: &str,
    output_dir: &Path,
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
    verbose: bool,
) -> bool {
    // TODO exporting this to string as a hack because the parser
    // is tied into the analyzer due to imports.
    compile(
        &analyzer::analyze_string(&format!("{pil}")),
        file_name,
        output_dir,
        query_callback,
        verbose,
    )
}

/// Compiles a .asm file, outputs the PIL on stdout and tries to generate
/// fixed and witness columns.
pub fn compile_asm(
    file_name: &str,
    inputs: Vec<AbstractNumberType>,
    output_dir: &Path,
    force_overwrite: bool,
    verbose: bool,
) {
    let contents = fs::read_to_string(file_name).unwrap();
    compile_asm_string(
        file_name,
        &contents,
        inputs,
        output_dir,
        force_overwrite,
        verbose,
    )
}

/// Compiles the contents of a .asm file, outputs the PIL on stdout and tries to generate
/// fixed and witness columns.
pub fn compile_asm_string(
    file_name: &str,
    contents: &str,
    inputs: Vec<AbstractNumberType>,
    output_dir: &Path,
    force_overwrite: bool,
    verbose: bool,
) {
    let pil = asm_compiler::compile(Some(file_name), contents).unwrap_or_else(|err| {
        eprintln!("Error parsing .asm file:");
        err.output_to_stderr();
        panic!();
    });
    let pil_file_name = output_dir.join(format!(
        "{}.pil",
        Path::new(file_name).file_stem().unwrap().to_str().unwrap()
    ));
    if pil_file_name.exists() && !force_overwrite {
        eprint!(
            "Target file {} already exists. Not overwriting.",
            pil_file_name.to_str().unwrap()
        );
        return;
    }
    fs::write(pil_file_name.clone(), format!("{pil}")).unwrap();

    let query_callback = |query: &str| -> Option<AbstractNumberType> {
        let items = query.split(',').map(|s| s.trim()).collect::<Vec<_>>();
        let mut it = items.iter();
        let _current_step = it.next().unwrap();
        let current_pc = it.next().unwrap();
        assert!(it.clone().len() % 3 == 0);
        for (pc_check, input, index) in it.tuples() {
            if pc_check == current_pc {
                assert_eq!(*input, "\"input\"");
                let index: usize = index.parse().unwrap();
                return inputs.get(index).cloned();
            }
        }
        None
    };
    compile_pil_ast(
        &pil,
        pil_file_name.to_str().unwrap(),
        output_dir,
        Some(query_callback),
        verbose,
    );
}

fn compile(
    analyzed: &analyzer::Analyzed,
    file_name: &str,
    output_dir: &Path,
    query_callback: Option<impl FnMut(&str) -> Option<AbstractNumberType>>,
    verbose: bool,
) -> bool {
    let mut success = true;
    let (constants, degree) = constant_evaluator::generate(analyzed);
    if analyzed.constant_count() == constants.len() {
        write_polys_file(
            &mut BufWriter::new(&mut fs::File::create(output_dir.join("constants.bin")).unwrap()),
            degree,
            &constants,
        );
        println!("Wrote constants.bin.");
        let commits =
            commit_evaluator::generate(analyzed, degree, &constants, query_callback, verbose);
        write_polys_file(
            &mut BufWriter::new(&mut fs::File::create(output_dir.join("commits.bin")).unwrap()),
            degree,
            &commits,
        );
        println!("Wrote commits.bin.");
    } else {
        println!("Not writing constants.bin because not all declared constants are defined (or there are none).");
        success = false;
    }
    let json_out = json_exporter::export(analyzed);
    let json_file = format!("{file_name}.json");
    json_out
        .write(&mut fs::File::create(output_dir.join(&json_file)).unwrap())
        .unwrap();
    println!("Wrote {json_file}.");
    success
}

fn write_polys_file(
    file: &mut impl Write,
    degree: DegreeType,
    polys: &Vec<(&str, Vec<AbstractNumberType>)>,
) {
    for i in 0..degree as usize {
        for (_name, constant) in polys {
            let mut v = constant[i].clone();
            if v.sign() == Sign::Minus {
                // This hardcodes the goldilocks field
                v += 0xffffffff00000001u64;
            }
            file.write_all(&abstract_to_degree(&v).to_le_bytes())
                .unwrap();
        }
    }
}

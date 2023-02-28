use std::fs;
use std::io::{BufWriter, Write};
use std::path::Path;

use analyzer::ConstantNumberType;

use crate::parser::ast::PILFile;
use crate::{analyzer, commit_evaluator, constant_evaluator, json_exporter};

pub fn no_callback() -> Option<fn(&str) -> Option<ConstantNumberType>> {
    None
}

/// Compiles a .pil file to its json form and also tries to generate
/// constants and committed polynomials.
/// @returns true if all committed/witness and constant/fixed polynomials
/// could be generated.
pub fn compile_pil(
    pil_file: &Path,
    output_dir: &Path,
    query_callback: Option<impl FnMut(&str) -> Option<ConstantNumberType>>,
) -> bool {
    compile(
        &analyzer::analyze(pil_file),
        pil_file.file_name().unwrap().to_str().unwrap(),
        output_dir,
        query_callback,
    )
}

pub fn compile_pil_ast(
    pil: &PILFile,
    file_name: &str,
    output_dir: &Path,
    query_callback: Option<impl FnMut(&str) -> Option<ConstantNumberType>>,
) -> bool {
    // TODO exporting this to string as a hack because the parser
    // is tied into the analyzer due to imports.
    compile(
        &analyzer::analyze_string(&format!("{pil}")),
        file_name,
        output_dir,
        query_callback,
    )
}

fn compile(
    analyzed: &analyzer::Analyzed,
    file_name: &str,
    output_dir: &Path,
    query_callback: Option<impl FnMut(&str) -> Option<ConstantNumberType>>,
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
        let commits = commit_evaluator::generate(analyzed, &degree, &constants, query_callback);
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
    degree: ConstantNumberType,
    polys: &Vec<(&String, Vec<ConstantNumberType>)>,
) {
    for i in 0..degree as usize {
        for (_name, constant) in polys {
            let mut v = constant[i];
            if v < 0 {
                // This hardcodes the goldilocks field
                v += 0xffffffff00000001;
            }
            file.write_all(&(v as u64).to_le_bytes()).unwrap();
        }
    }
}

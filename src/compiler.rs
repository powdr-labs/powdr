use std::fs;
use std::io::{BufWriter, Write};
use std::path::Path;

use analyzer::ConstantNumberType;

use crate::{analyzer, commit_evaluator, constant_evaluator, json_exporter};

/// Compiles a .pil file to its json form and also tries to generate
/// constants and committed polynomials.
pub fn compile_pil(pil_file: &Path) {
    let analyzed = analyzer::analyze(pil_file);
    let (constants, degree) = constant_evaluator::generate(&analyzed);
    if analyzed.constant_count() == constants.len() {
        write_polys_file(
            &mut BufWriter::new(&mut fs::File::create("constants.bin").unwrap()),
            degree,
            &constants,
        );
        println!("Wrote constants.bin.");
        let commits = commit_evaluator::generate(&analyzed, &degree, &constants);
        write_polys_file(
            &mut BufWriter::new(&mut fs::File::create("commits.bin").unwrap()),
            degree,
            &commits,
        );
        println!("Wrote commits.bin.");
    } else {
        println!("Not writing constants.bin because not all declared constants are defined (or there are none).");
    }
    let json_out = json_exporter::export(&analyzed);
    let json_file = format!("{}.json", pil_file.file_name().unwrap().to_str().unwrap());
    json_out
        .write(&mut fs::File::create(&json_file).unwrap())
        .unwrap();
    println!("Wrote {json_file}.");
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

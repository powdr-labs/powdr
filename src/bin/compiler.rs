use std::{env, fs, io::Write, path::Path};

use powdr::analyzer::ConstantNumberType;

fn main() {
    let file = Path::new(&env::args().nth(1).unwrap()).to_owned();
    let analyzed = powdr::analyzer::analyze(&file);
    let (constants, degree) = powdr::constant_evaluator::generate(&analyzed);
    if analyzed.constant_count() == constants.len() {
        write_constants_file(
            &mut fs::File::create("constants.bin").unwrap(),
            degree,
            constants,
        );
        println!("Wrote constants.bin.");
    } else {
        println!("Not writing constants.bin because not all declared constants are defined (or there are none).");
    }
    let json_out = powdr::json_exporter::export(&analyzed);
    let json_file = format!("{}.json", file.file_name().unwrap().to_str().unwrap());
    json_out
        .write(&mut fs::File::create(&json_file).unwrap())
        .unwrap();
    println!("Wrote {json_file}.");
}

fn write_constants_file(
    file: &mut impl Write,
    degree: ConstantNumberType,
    constants: Vec<Vec<ConstantNumberType>>,
) {
    for i in 0..degree as usize {
        for constant in &constants {
            let mut v = constant[i];
            if v < 0 {
                // This hardcodes the goldilocks field
                v += 0xffffffff00000001;
            }
            file.write_all(&(v as u64).to_le_bytes()).unwrap();
        }
    }
}

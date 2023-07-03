use ast::analyzed::Analyzed;
use number::{read_polys_file, DegreeType, FieldElement};
use std::{fs::File, path::Path};

pub fn read_fixed<'a, T: FieldElement>(
    pil: &'a Analyzed<T>,
    dir: &Path,
) -> (Vec<(&'a str, Vec<T>)>, DegreeType) {
    let fixed_columns: Vec<&str> = pil
        .constant_polys_in_source_order()
        .iter()
        .map(|(poly, _)| poly.absolute_name.as_str())
        .collect();

    read_polys_file(
        &mut File::open(dir.join("constants").with_extension("bin")).unwrap(),
        &fixed_columns,
    )
}

pub fn read_witness<'a, T: FieldElement>(
    pil: &'a Analyzed<T>,
    dir: &Path,
) -> (Vec<(&'a str, Vec<T>)>, DegreeType) {
    let witness_columns: Vec<&str> = pil
        .committed_polys_in_source_order()
        .iter()
        .map(|(poly, _)| poly.absolute_name.as_str())
        .collect();

    read_polys_file(
        &mut File::open(dir.join("commits").with_extension("bin")).unwrap(),
        &witness_columns,
    )
}

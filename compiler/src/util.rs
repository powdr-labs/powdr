use ast::analyzed::{Analyzed, FunctionValueDefinition, Polynomial};
use number::{read_polys_file, DegreeType, FieldElement};
use std::{fs::File, io::BufReader, path::Path};

pub trait PolySet {
    const FILE_NAME: &'static str;
    fn get_polys<T: FieldElement>(
        pil: &Analyzed<T>,
    ) -> Vec<&(Polynomial, Option<FunctionValueDefinition<T>>)>;
}

pub struct FixedPolySet;
impl PolySet for FixedPolySet {
    const FILE_NAME: &'static str = "constants.bin";

    fn get_polys<T: FieldElement>(
        pil: &Analyzed<T>,
    ) -> Vec<&(Polynomial, Option<FunctionValueDefinition<T>>)> {
        pil.constant_polys_in_source_order()
    }
}

pub struct WitnessPolySet;
impl PolySet for WitnessPolySet {
    const FILE_NAME: &'static str = "commits.bin";

    fn get_polys<T: FieldElement>(
        pil: &Analyzed<T>,
    ) -> Vec<&(Polynomial, Option<FunctionValueDefinition<T>>)> {
        pil.committed_polys_in_source_order()
    }
}

pub fn read_poly_set<'a, P: PolySet, T: FieldElement>(
    pil: &'a Analyzed<T>,
    dir: &Path,
) -> (Vec<(&'a str, Vec<T>)>, DegreeType) {
    let fixed_columns: Vec<&str> = P::get_polys(pil)
        .iter()
        .map(|(poly, _)| poly.absolute_name.as_str())
        .collect();

    read_polys_file(
        &mut BufReader::new(File::open(dir.join(P::FILE_NAME)).unwrap()),
        &fixed_columns,
    )
}

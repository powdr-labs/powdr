use powdr_ast::analyzed::{Analyzed, FunctionValueDefinition, Symbol};
use powdr_number::{read_polys_file, FieldElement};
use std::{fs::File, io::BufReader, path::Path};

pub trait PolySet {
    const FILE_NAME: &'static str;
    fn get_polys<T: FieldElement>(
        pil: &Analyzed<T>,
    ) -> Vec<&(Symbol, Option<FunctionValueDefinition>)>;
}

pub struct FixedPolySet;
impl PolySet for FixedPolySet {
    const FILE_NAME: &'static str = "constants.bin";

    fn get_polys<T: FieldElement>(
        pil: &Analyzed<T>,
    ) -> Vec<&(Symbol, Option<FunctionValueDefinition>)> {
        pil.constant_polys_in_source_order()
    }
}

pub struct WitnessPolySet;
impl PolySet for WitnessPolySet {
    const FILE_NAME: &'static str = "commits.bin";

    fn get_polys<T: FieldElement>(
        pil: &Analyzed<T>,
    ) -> Vec<&(Symbol, Option<FunctionValueDefinition>)> {
        pil.committed_polys_in_source_order()
    }
}

#[allow(clippy::type_complexity)]
pub fn read_poly_set<P: PolySet, T: FieldElement>(
    dir: &Path,
) -> Vec<(String, Vec<T>)> {
    let path = dir.join(P::FILE_NAME);
    read_polys_file(&mut BufReader::new(
        File::open(path).unwrap(),
    ))
}

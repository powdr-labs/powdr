use powdr_ast::analyzed::{Analyzed, FunctionValueDefinition, Symbol};
use powdr_number::{read_polys_file, DegreeType, FieldElement};
use std::{
    fs::File,
    io::{self, BufReader},
    path::Path,
};

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
pub fn try_read_poly_set<P: PolySet, T: FieldElement>(
    pil: &Analyzed<T>,
    dir: &Path,
    name: &str,
) -> Option<(Vec<(String, Vec<T>)>, DegreeType)> {
    let column_names: Vec<String> = P::get_polys(pil)
        .iter()
        .flat_map(|(poly, _)| poly.array_elements())
        .map(|(name, _id)| name)
        .collect();

    (!column_names.is_empty()).then(|| {
        let fname = format!("{name}_{}", P::FILE_NAME);
        read_polys_file(
            &mut BufReader::new(File::open(dir.join(fname)).unwrap()),
            &column_names,
        )
    })
}

/// Calls a function with the given writer, flushes it, and panics on error.
pub fn write_or_panic<W, F, T>(mut writer: W, f: F) -> T
where
    W: io::Write,
    F: FnOnce(&mut W) -> T,
{
    let result = f(&mut writer);
    writer.flush().unwrap();
    result
}

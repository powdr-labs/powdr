use ast::{asm::AnalysisASMFile, parsed::asm::ASMFile};
use number::FieldElement;

mod batcher;
mod column_clusterer;

pub fn analyse<T: FieldElement>(file: ASMFile<T>) -> AnalysisASMFile<T> {
    let file = file.into();
    let file = column_clusterer::Clusterer::default().treat(file);
    batcher::ASMBatcher::default().convert(file)
}

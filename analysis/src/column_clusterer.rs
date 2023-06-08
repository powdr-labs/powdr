use std::{marker::PhantomData, collections::BTreeSet};

use ast::asm::AnalysisASMFile;

#[derive(Default)]
pub struct Clusterer<T> {
    /// the pil columns introduced in pil blocks
    pil_columns: BTreeSet<String>,
    marker: PhantomData<T>,
}

impl<T> Clusterer<T> {
    pub fn treat(&mut self, file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
        file
    }
}

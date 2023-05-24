use std::collections::{BTreeMap, HashMap};
use std::path::Path;

pub struct AsmProfiler {}

impl AsmProfiler {
    pub fn set_output_dir(&mut self, dir: &Path) {
        todo!();
    }
}

#[derive(Default)]
pub struct ProfilerBuilder {
    file_nrs: HashMap<usize, (String, String)>,
    source_locations: BTreeMap<usize, (usize, usize, usize)>,
}

impl ProfilerBuilder {
    pub fn add_file(&mut self, nr: usize, dir: String, file: String) {
        assert!(self.file_nrs.insert(nr, (dir, file)).is_none());
    }
    pub fn set_source_location(&mut self, pc: usize, file: usize, line: usize, col: usize) {
        assert!(self
            .source_locations
            .insert(pc, (file, line, col))
            .is_none());
    }
    pub fn to_profiler(self) -> AsmProfiler {
        todo!();
    }
}

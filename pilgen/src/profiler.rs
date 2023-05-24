use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::Write;
use std::path::Path;

pub struct AsmProfiler {
    pc_name: String,
    output: Option<File>,
}

impl AsmProfiler {
    pub fn pc_name(&self) -> &str {
        self.pc_name.as_str()
    }

    pub fn set_output_dir(&mut self, dir: &Path) {
        assert!(self.output.is_none());
        self.output = Some(File::create(dir.join("callgrind.out")).unwrap());
    }

    pub fn called_pc(&mut self, pc: u64) {
        writeln!(self.output.as_mut().unwrap(), "{pc}").unwrap();
    }
}

#[derive(Default)]
pub struct ProfilerBuilder {
    file_nrs: HashMap<usize, (String, String)>,
    source_locations: BTreeMap<usize, (usize, usize, usize)>,
    pc_name: String,
}

impl ProfilerBuilder {
    pub fn set_pc_name(&mut self, name: &str) {
        self.pc_name = name.to_string();
    }
    pub fn add_file(&mut self, nr: usize, dir: String, file: String) {
        assert!(self.file_nrs.insert(nr, (dir, file)).is_none());
    }
    pub fn set_source_location(&mut self, pc: usize, file: usize, line: usize, col: usize) {
        self.source_locations.insert(pc, (file, line, col));
    }
    pub fn to_profiler(self) -> AsmProfiler {
        AsmProfiler {
            pc_name: self.pc_name,
            output: None,
        }
    }
}

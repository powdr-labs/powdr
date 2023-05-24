use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::Write;
use std::path::Path;

pub struct AsmProfiler {
    pc_name: String,
    file_nrs: HashMap<usize, (String, String)>,
    // TODO use a struct
    source_locations: BTreeMap<usize, (usize, usize, usize)>,
    function_starts: BTreeMap<usize, String>,
    output: Option<File>,
    // TODO use a struct
    instruction_counts: BTreeMap<(usize, String), BTreeMap<usize, usize>>,
}

impl AsmProfiler {
    pub fn pc_name(&self) -> &str {
        self.pc_name.as_str()
    }

    pub fn set_output_dir(&mut self, dir: &Path) {
        assert!(self.output.is_none());
        self.output = Some(File::create(dir.join("callgrind.out")).unwrap());
        write!(self.output.as_mut().unwrap(), "events: Instructions\n\n").unwrap();
    }

    pub fn called_pc(&mut self, pc: u64) {
        if let Some((file, line)) = self
            .source_location(pc as usize)
            .map(|(file, l, c)| (*file, *l))
        {
            if let Some(function) = self.function(pc as usize) {
                *self
                    .instruction_counts
                    .entry((file, function.clone()))
                    .or_default()
                    .entry(line)
                    .or_default() += 1;
            }
        }
    }

    pub fn execution_finished(&mut self) {
        let out = self.output.as_mut().unwrap();
        for ((file_nr, function), data) in &self.instruction_counts {
            writeln!(
                out,
                "fl={}/{}",
                self.file_nrs[file_nr].0, self.file_nrs[file_nr].1
            )
            .unwrap();
            // TODO C++filt
            writeln!(out, "fn={function}").unwrap();
            for (line, count) in data {
                writeln!(out, "{line} {count}").unwrap();
            }
            writeln!(out).unwrap();
        }
        // for
        // # callgrind format
        // events: Instructions

        // fl=file1.c
        // fn=main
        // 16 20
        // cfn=func1
        // calls=1 50
        // 16 400
        // cfi=file2.c
        // cfn=func2
        // calls=3 20
        // 16 400
    }

    fn source_location(&self, pc: usize) -> Option<&(usize, usize, usize)> {
        self.source_locations
            .range(..=pc)
            .last()
            .map(|(_, loc)| loc)
    }

    fn function(&self, pc: usize) -> Option<&String> {
        self.function_starts.range(..=pc).last().map(|(_, fun)| fun)
    }
}

#[derive(Default)]
pub struct ProfilerBuilder {
    file_nrs: HashMap<usize, (String, String)>,
    source_locations: BTreeMap<usize, (usize, usize, usize)>,
    function_starts: BTreeMap<usize, String>,
    pc_name: String,
}

impl ProfilerBuilder {
    pub fn set_pc_name(&mut self, name: &str) {
        self.pc_name = name.to_string();
    }
    pub fn add_file(&mut self, nr: usize, dir: String, file: String) {
        assert!(self.file_nrs.insert(nr, (dir, file)).is_none());
    }
    pub fn set_label(&mut self, pc: usize, label: &str) {
        // TODO this is a hack
        if !label.contains("___dot_L") {
            self.function_starts.insert(pc, label.to_string());
        }
    }
    pub fn set_source_location(&mut self, pc: usize, file: usize, line: usize, col: usize) {
        self.source_locations.insert(pc, (file, line, col));
    }
    pub fn to_profiler(self) -> AsmProfiler {
        AsmProfiler {
            pc_name: self.pc_name,
            file_nrs: self.file_nrs,
            source_locations: self.source_locations,
            function_starts: self.function_starts,
            output: None,
            instruction_counts: Default::default(),
        }
    }
}

use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::Write;
use std::path::Path;

use rustc_demangle::demangle;

pub struct AsmProfiler {
    pc_name: String,
    instructions: HashMap<String, InstrKind>,
    file_nrs: HashMap<usize, (String, String)>,
    // TODO use a struct
    source_locations: BTreeMap<usize, (usize, usize, usize)>,
    function_starts: BTreeMap<usize, String>,
    output: Option<File>,
    instruction_counts: BTreeMap<Location, usize>,
    previous_pc: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstrKind {
    Regular,
    /// Call or tail-call.
    Call,
    Return,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct CallStackItem {
    source: Location,
    dest: Location,
    instructions: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Location {
    file_nr: usize,
    function: String, // TODO use index
    line: usize,
}

impl AsmProfiler {
    pub fn pc_name(&self) -> &str {
        self.pc_name.as_str()
    }
    pub fn instructions(&self) -> &HashMap<String, InstrKind> {
        &self.instructions
    }

    pub fn set_output_dir(&mut self, dir: &Path) {
        assert!(self.output.is_none());
        self.output = Some(File::create(dir.join("callgrind.out")).unwrap());
        write!(self.output.as_mut().unwrap(), "events: Instructions\n\n").unwrap();
    }

    // If we get a call, we push something to the stack and we start a counter from zero.
    // If we return, we pop it from the stack and store a "call"
    // At call: Store function, line to a stack.
    // At first instruction of call: add destination to that stack item

    // at ret: just record "ret"
    // at first instruction after ret: find function, check with stack (do this multiple times in case of tail call)
    // and store "cal"
    //
    pub fn called_pc(&mut self, pc: usize, instr_kind: InstrKind) {
        let Some(location) = self.location(pc) else { return; };
        *self.instruction_counts.entry(location).or_default() += 1;

        self.previous_pc = pc;
    }

    pub fn execution_finished(&mut self) {
        let out = self.output.as_mut().unwrap();
        let mut data: BTreeMap<(usize, String), Vec<(usize, usize)>> = BTreeMap::default();
        for (loc, cnt) in &self.instruction_counts {
            data.entry((loc.file_nr, loc.function.clone()))
                .or_default()
                .push((loc.line, *cnt));
        }
        for ((file_nr, function), items) in data {
            writeln!(
                out,
                "fl={}/{}",
                self.file_nrs[&file_nr].0, self.file_nrs[&file_nr].1
            )
            .unwrap();
            writeln!(out, "fn={:#}", demangle(&function)).unwrap();
            for (line, count) in items {
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

    fn location(&self, pc: usize) -> Option<Location> {
        let (file, line, column) = self.source_location(pc)?;
        let function = self.function(pc)?;
        Some(Location {
            file_nr: *file,
            function: function.to_string(),
            line: *line,
        })
    }
}

#[derive(Default)]
pub struct ProfilerBuilder {
    file_nrs: HashMap<usize, (String, String)>,
    source_locations: BTreeMap<usize, (usize, usize, usize)>,
    function_starts: BTreeMap<usize, String>,
    pc_name: String,
    instructions: HashMap<String, InstrKind>,
}

impl ProfilerBuilder {
    pub fn set_pc_name(&mut self, name: &str) {
        self.pc_name = name.to_string();
    }
    pub fn add_instruction(&mut self, name: &str, kind: InstrKind) {
        self.instructions.insert(name.to_string(), kind);
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
            instructions: self.instructions,
            file_nrs: self.file_nrs,
            source_locations: self.source_locations,
            function_starts: self.function_starts,
            output: None,
            instruction_counts: Default::default(),
            previous_pc: 0,
        }
    }
}

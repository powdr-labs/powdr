use std::{
    collections::BTreeMap,
    fs::File,
    io::BufWriter,
    io::Write,
    path::{Path, PathBuf},
};

use itertools::Itertools;

use rustc_demangle::demangle;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Call<'a> {
    from: Loc<'a>,
    target: Loc<'a>,
}

/// RISC-V asm profiler.
/// Tracks the self-cost of functions and the cumulative cost of specific function calls (i.e., callgrind style).
pub struct Profiler<'a> {
    /// profiling options
    options: ProfilerOptions,
    /// file number to (dir,file)
    debug_files: &'a [(&'a str, &'a str)],
    /// pc value of function beginnings
    function_begin: BTreeMap<usize, &'a str>,
    /// pc value of .debug loc statements
    location_begin: BTreeMap<usize, (usize, usize)>,
    /// current call stack, entries include running cost
    call_stack: Vec<(Call<'a>, usize)>,
    /// saved return address of "jump and link" instructions
    return_pc_stack: Vec<usize>,
    /// cost of each location
    location_stats: BTreeMap<Loc<'a>, usize>,
    /// (count, cumulative cost) of calls
    call_stats: BTreeMap<Call<'a>, (usize, usize)>,
    /// stack sampling format for FlameGraph
    folded_stack_stats: BTreeMap<Vec<&'a str>, usize>,
}

#[derive(Default, Clone)]
pub struct ProfilerOptions {
    pub output_directory: String,
    pub file_stem: Option<String>,
    pub flamegraph: bool,
    pub callgrind: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Loc<'a> {
    function: &'a str,
    file: usize,
    line: usize,
}

impl<'a> Profiler<'a> {
    pub fn new(
        options: ProfilerOptions,
        debug_files: &'a [(&'a str, &'a str)],
        function_begin: BTreeMap<usize, &'a str>,
        location_begin: BTreeMap<usize, (usize, usize)>,
    ) -> Self {
        Profiler {
            options,
            debug_files,
            function_begin,
            location_begin,
            call_stack: Default::default(),
            return_pc_stack: Default::default(),
            location_stats: Default::default(),
            call_stats: Default::default(),
            folded_stack_stats: Default::default(),
        }
    }

    pub fn write_callgrind<P: AsRef<Path>>(&self, path: P) {
        log::info!("Writing callgrind data to {:?}", path.as_ref());
        let file = File::create(path).unwrap();
        let mut w = BufWriter::new(file);
        writeln!(&mut w, "events: Instructions\n").unwrap();

        struct CallCost<'a> {
            call: &'a Call<'a>,
            count: usize,
            cost: usize,
        }

        let mut func_ids = BTreeMap::new();
        let mut loc_stats: BTreeMap<_, Vec<_>> = BTreeMap::new();
        let mut call_stats: BTreeMap<_, Vec<_>> = BTreeMap::new();

        // group stats per (function_id, file)
        for (id, func) in self.function_begin.values().enumerate() {
            for (loc, cost) in &self.location_stats {
                if &loc.function == func {
                    func_ids.entry(func).or_insert(id);
                    loc_stats
                        .entry((id, loc.file))
                        .or_default()
                        .push((loc.line, cost));
                }
            }

            for (call, (count, cost)) in &self.call_stats {
                if &call.from.function == func {
                    assert!(func_ids.contains_key(func));
                    call_stats
                        .entry((id, call.from.file))
                        .or_default()
                        .push(CallCost {
                            call,
                            count: *count,
                            cost: *cost,
                        });
                }
            }
        }

        // use function name id mapping, without it callgrind was showing duplicate entries
        for (func, id) in &func_ids {
            writeln!(&mut w, "fn=({id}) {}", format_function_name(func)).unwrap();
        }
        writeln!(w).unwrap();

        // print stats
        for ((func_id, file), line_costs) in &loc_stats {
            let (dir, name) = self.debug_files[*file - 1];
            writeln!(&mut w, "fl={dir}/{name}").unwrap();
            writeln!(w, "fn=({func_id})").unwrap();
            for (line, cost) in line_costs {
                writeln!(&mut w, "{line} {cost}").unwrap();
            }
            for CallCost { call, cost, count } in
                call_stats.get(&(*func_id, *file)).unwrap_or(&vec![])
            {
                let (dir, name) = self.debug_files[*file - 1];
                writeln!(&mut w, "cfl={dir}/{name}").unwrap();
                writeln!(w, "cfn=({})", func_ids[&call.target.function]).unwrap();
                writeln!(w, "calls={count} {}", call.target.line).unwrap();
                writeln!(w, "{} {cost}", call.from.line).unwrap();
            }
            writeln!(w).unwrap();
        }
    }

    pub fn write_flamegraph<P: AsRef<Path>>(&self, path: P) {
        log::info!("Writing flamegraph to {:?}", path.as_ref());
        let lines: Vec<_> = self
            .folded_stack_stats
            .iter()
            .map(|(stack, count)| {
                let stack = stack
                    .iter()
                    .map(|function| format_function_name(function))
                    .join(";");
                format!("{stack} {count}")
            })
            .collect();
        let mut options = Default::default();
        let file = File::create(path).unwrap();
        let w = BufWriter::new(file);
        inferno::flamegraph::from_lines(&mut options, lines.iter().map(|s| s.as_str()), w).unwrap();
    }

    /// calculate totals and write out results
    pub fn finish(&mut self) {
        let mut path = PathBuf::from(&self.options.output_directory)
            .join(self.options.file_stem.as_deref().unwrap_or("out"));
        if self.options.flamegraph {
            path.set_extension("svg");
            self.write_flamegraph(&path);
        }
        if self.options.callgrind {
            path.set_extension("callgrind");
            self.write_callgrind(&path);
        }
    }

    /// profiling only starts once "__runtime_start" is reached
    pub fn is_running(&self) -> bool {
        !self.call_stack.is_empty()
    }

    /// function at the top of the call stack
    pub fn curr_function(&self) -> Option<&'a str> {
        self.call_stack.last().map(|(c, _)| c.target.function)
    }

    /// get the function name and source location for a given pc value
    pub fn location_at(&self, pc: usize) -> Option<Loc<'a>> {
        self.function_begin
            .range(..=pc)
            .last()
            .map(|(_, function)| {
                let (file, line) = self
                    .location_begin
                    .range(..=pc)
                    .last()
                    .map(|(_, (file, line))| (*file, *line))
                    // for labels with no .loc above them, just point to main file
                    .unwrap_or((1, 0));
                Loc {
                    function,
                    file,
                    line,
                }
            })
    }

    /// add cost for instruction/row
    pub fn add_instruction_cost(&mut self, curr_pc: usize) {
        if !self.is_running() {
            return;
        }

        // add cost to current location. AFAIU need the function name from the call stack to handle inlining
        let function = self.curr_function().unwrap();
        let Loc { file, line, .. } = self.location_at(curr_pc).unwrap();
        *self
            .location_stats
            .entry(Loc {
                function,
                file,
                line,
            })
            .or_default() += 1;

        // add cost to current call
        self.call_stack.last_mut().unwrap().1 += 1;

        // add sample to folded stacks
        let stack: Vec<_> = self
            .call_stack
            .iter()
            .map(|(call, _)| call.target.function)
            .collect();
        *self.folded_stack_stats.entry(stack).or_default() += 1;
    }

    /// Should be called for instructions that jump and save the returning address in an actual RISC-V register.
    /// This is handled as a "call" into a function.
    pub fn jump_and_link(&mut self, curr_pc: usize, target_pc: usize, return_pc: usize) {
        if let Some(mut target) = self.location_at(target_pc) {
            if let Some(curr_function) = self.curr_function() {
                let Loc {
                    file: curr_file,
                    line: curr_line,
                    ..
                } = self.location_at(curr_pc).unwrap();
                // ecall handler code doesn't have a ".debug loc", so we keep current file/line
                if target.function == "__ecall_handler" {
                    target.file = curr_file;
                    target.line = curr_line;
                }
                let call = Call {
                    from: Loc {
                        function: curr_function,
                        file: curr_file,
                        line: curr_line,
                    },
                    target,
                };
                // increase call count
                self.call_stats.entry(call.clone()).or_default().0 += 1;
                self.call_stack.push((call, 0));
                self.return_pc_stack.push(return_pc);
            } else {
                // we start profiling on the initial call to "__runtime_start"
                if target.function == "__runtime_start" {
                    let call = Call {
                        // __runtime_start does not have a proper ".debug loc", just point to main file
                        from: Loc {
                            function: "",
                            file: 1,
                            line: 0,
                        },
                        target,
                    };
                    // increase call count
                    self.call_stats.entry(call.clone()).or_default().0 += 1;
                    self.call_stack.push((call, 0));
                    self.return_pc_stack.push(return_pc);
                }
            }
        } else {
            assert!(!self.is_running());
        }
    }

    /// Should be called for jumps that don't save the returning address.
    /// This is handled as one of 3 cases:
    /// - "return" from function: target_pc equal to last `jump_and_link` saved pc
    /// - "tail call": next_function != current_function
    /// - control flow: next_function == current_function
    pub fn jump(&mut self, target_pc: usize) {
        if !self.is_running() {
            return;
        }

        if self
            .return_pc_stack
            .last()
            .is_some_and(|saved_pc| *saved_pc == target_pc)
        {
            // "return" from current function
            let (done_call, cost) = self.call_stack.pop().unwrap();
            self.return_pc_stack.pop();
            // add to cumulative cost of call and to running cost of caller
            if let Some((_curr_call, curr_cost)) = self.call_stack.last_mut() {
                self.call_stats.get_mut(&done_call).unwrap().1 += cost;
                *curr_cost += cost;
            }
        } else {
            let target = self.location_at(target_pc).unwrap();
            let curr_function = self.curr_function().unwrap();
            if target.function != curr_function {
                // "tail call": replace the current call in the stack
                let (done_call, cost) = self.call_stack.pop().unwrap();

                // add to cumulative cost of call and to running cost of caller
                if let Some((_curr_call, curr_cost)) = self.call_stack.last_mut() {
                    self.call_stats.get_mut(&done_call).unwrap().1 += cost;
                    *curr_cost += cost;
                }

                // push new call.
                // here we keep the origin of the current call as the origin of the tail call replacing it
                let new_call = Call {
                    from: done_call.from,
                    target,
                };
                self.call_stats.entry(new_call.clone()).or_default().0 += 1;
                self.call_stack.push((new_call, 0));
            } else {
                // "control flow" (or "tail call" to self, if that is a thing), don't think this needs special handling
            }
        }
    }
}

fn format_function_name(name: &str) -> String {
    if let Some(prefix) = name.find("___ZN") {
        format!("{}", demangle(&name[prefix + 2..]))
    } else {
        format!("{}", demangle(name))
    }
}

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

/// Risc-v asm profiler.
/// Tracks the self-cost of functions and cummulative cost specific function calls (i.e., callgrind style).
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
    /// (count, cummulative cost) of calls
    call_stats: BTreeMap<Call<'a>, (usize, usize)>,
    /// stack sampling format for FlameGraph
    folded_stack_stats: BTreeMap<Vec<&'a str>, usize>,
}

#[derive(Default, Clone)]
pub struct ProfilerOptions {
    pub output_directory: String,
    pub file_stem: Option<String>,
    pub generate_flamegraph: bool,
    pub generate_callgrind: bool,
}

impl ProfilerOptions {
    fn is_enabled(&self) -> bool {
        self.generate_callgrind || self.generate_flamegraph
    }
}

/// A location is (file, line)
pub type Loc<'a> = (&'a str, usize, usize);

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
        let file = File::create(path).unwrap();
        let mut w = BufWriter::new(file);
        writeln!(&mut w, "events: Instructions\n").unwrap();
        for func in self.function_begin.values() {
            let loc_stats: Vec<_> = self
                .location_stats
                .iter()
                .filter_map(|(loc, cost)| {
                    if &loc.0 == func {
                        Some((loc.1, loc.2, cost))
                    } else {
                        None
                    }
                })
                .sorted()
                .collect();
            let call_stats: Vec<_> = self
                .call_stats
                .iter()
                .filter_map(|(call, (count, cost))| {
                    if &call.from.0 == func {
                        Some((call, count, cost))
                    } else {
                        None
                    }
                })
                .collect();

            if loc_stats.is_empty() && call_stats.is_empty() {
                continue;
            }

            writeln!(w, "fn={}", format_function_name(func)).unwrap();

            let mut curr_file = None;
            for (file_nr, line, cost) in loc_stats {
                if Some(file_nr) != curr_file {
                    curr_file = Some(file_nr);
                    let file = self.debug_files[file_nr - 1];
                    writeln!(w, "fl={}/{}", file.0, file.1).unwrap();
                }
                writeln!(w, "{line} {cost}").unwrap();
            }
            for (call, count, cost) in call_stats {
                let target_file_nr = call.target.1;
                if Some(target_file_nr) != curr_file {
                    curr_file = Some(target_file_nr);
                    let file = self.debug_files[target_file_nr - 1];
                    writeln!(w, "cfi={}/{}", file.0, file.1).unwrap();
                }
                writeln!(w, "cfn={}", format_function_name(call.target.0)).unwrap();
                writeln!(w, "calls={count} {}", call.target.2).unwrap();
                writeln!(w, "{} {cost}", call.from.2).unwrap();
            }
            writeln!(w).unwrap();
        }
    }

    pub fn write_flamegraph<P: AsRef<Path>>(&self, path: P) {
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

    // TODO: REMOVE
    #[allow(unused)]
    pub fn write_debug_output(&self) {
        log::debug!("====== EXECUTION STATS =======");
        // TODO: handle tail call from `main`?
        for func in self.function_begin.values() {
            let loc_stats: Vec<_> = self
                .location_stats
                .iter()
                .filter_map(|(loc, cost)| {
                    if &loc.0 == func {
                        Some((loc.1, loc.2, cost))
                    } else {
                        None
                    }
                })
                .collect();
            let call_stats: Vec<_> = self
                .call_stats
                .iter()
                .filter_map(|(call, (count, cost))| {
                    if &call.from.0 == func {
                        Some((call, count, cost))
                    } else {
                        None
                    }
                })
                .collect();
            if !loc_stats.is_empty() || !call_stats.is_empty() {
                log::debug!("===============================");
                log::debug!("{}:", format_function_name(func));
                let mut cumm_cost = 0;
                let mut self_cost = 0;
                if !loc_stats.is_empty() {
                    log::debug!("LOC STATS:");
                    for (file, line, cost) in loc_stats {
                        self_cost += cost;
                        cumm_cost += cost;
                        log::debug!("\t{:?} {line} {cost}", self.debug_files[file - 1]);
                    }
                }
                if !call_stats.is_empty() {
                    log::debug!("CALLS:");
                    for (call, count, cost) in call_stats {
                        cumm_cost += cost;
                        log::debug!(
                            "\t{} {:?} {} {count} {cost}",
                            format_function_name(call.target.0),
                            self.debug_files[call.from.1 - 1],
                            call.from.2
                        );
                    }
                }
                log::debug!("{self_cost} self cost {}", format_function_name(func));
                log::debug!(
                    "{cumm_cost} cummulative cost {}",
                    format_function_name(func)
                );
            }
        }
    }

    /// calculate totals and write out results
    pub fn execution_finished(&mut self) {
        if !self.options.is_enabled() {
            return;
        }
        let mut path = PathBuf::from(&self.options.output_directory)
            .join(self.options.file_stem.as_deref().unwrap_or("out"));
        if self.options.generate_flamegraph {
            path.set_extension("svg");
            self.write_flamegraph(&path);
        }
        if self.options.generate_callgrind {
            path.set_extension("callgrind");
            self.write_callgrind(&path);
        }
    }

    /// profiling only starts once "__runtime_start" is reached
    pub fn running(&self) -> bool {
        !self.call_stack.is_empty()
    }

    /// function at the top of the call stack
    pub fn curr_function(&self) -> Option<&'a str> {
        self.call_stack.last().map(|(c, _)| c.target.0)
    }

    /// get the function name and source location for a given pc value
    pub fn location_at(&self, pc: usize) -> Option<Loc<'a>> {
        self.function_begin
            .range(..=pc)
            .last()
            .and_then(|(_, func)| {
                self.location_begin
                    .range(..=pc)
                    .last()
                    .map(|(_, (file, line))| (*func, *file, *line))
            })
    }

    /// TODO: for dev debugging only REMOVE
    pub fn print_stack(&self, what: &str) {
        log::debug!("[ {what}");
        for (Call { target, .. }, cost) in self.call_stack.iter() {
            log::debug!("\t{} {cost},", format_function_name(target.0));
        }
        log::debug!("]");
    }

    /// add cost for instruction/row
    pub fn add_instruction_cost(&mut self, curr_pc: usize) {
        if !self.options.is_enabled() || !self.running() {
            return;
        }

        // add cost to current location. AFAIU need the function name from the call stack to handle inlining
        let function = self.curr_function().unwrap();
        let (_, file, line) = self.location_at(curr_pc).unwrap();
        *self
            .location_stats
            .entry((function, file, line))
            .or_default() += 1;

        // add cost to current call
        self.call_stack.last_mut().unwrap().1 += 1;

        // add sample to folded stacks
        let stack: Vec<_> = self
            .call_stack
            .iter()
            .map(|(call, _)| call.target.0)
            .collect();
        *self.folded_stack_stats.entry(stack).or_default() += 1;
    }

    /// Should be called for instructions that jump and save the returning address in an actual RISC-V register.
    /// This is handled as a "call" into a function.
    pub fn jump_and_link(&mut self, curr_pc: usize, target_pc: usize, return_pc: usize) {
        if !self.options.is_enabled() {
            return;
        }
        if let Some(target) = self.location_at(target_pc) {
            if let Some(curr_function) = self.curr_function() {
                let (_, curr_file, curr_line) = self.location_at(curr_pc).unwrap();
                let call = Call {
                    from: (curr_function, curr_file, curr_line),
                    target,
                };
                // increase call count
                self.call_stats.entry(call.clone()).or_default().0 += 1;
                self.call_stack.push((call, 0));
                self.return_pc_stack.push(return_pc);
                self.print_stack("CALL");
            } else {
                // we start profiling on the initial call to "__runtime_start"
                if target.0 == "__runtime_start" {
                    let call = Call {
                        from: ("", 0, 0),
                        target,
                    };
                    // increase call count
                    self.call_stats.entry(call.clone()).or_default().0 += 1;
                    self.call_stack.push((call, 0));
                    self.return_pc_stack.push(return_pc);
                    self.print_stack("CALL");
                }
            }
        } else {
            assert!(!self.running());
        }
    }

    /// Should be called for jumps that don't save the returning address.
    /// This is handled as one of 3 cases:
    /// - "return" from function: target_pc equal to last `jump_and_link` saved pc
    /// - "tail call": next_function != current_function
    /// - control flow: next_function == current_function
    pub fn jump(&mut self, target_pc: usize) {
        if !self.options.is_enabled() || !self.running() {
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
            // add to calls cummulative cost and to parent call
            if let Some((_curr_call, curr_cost)) = self.call_stack.last_mut() {
                self.call_stats.get_mut(&done_call).unwrap().1 += cost;
                *curr_cost += cost;
            }
            self.print_stack("RETURN");
        } else {
            let target = self.location_at(target_pc).unwrap();
            let curr_function = self.curr_function().unwrap();
            if target.0 != curr_function {
                // "tail call": replace the current call in the stack
                let (done_call, cost) = self.call_stack.pop().unwrap();

                // add to calls cummulative cost and to parent call
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
                self.print_stack("TAIL");
            } else {
                // "control flow" (or "tail call" to self, if that is a thing), don't think this needs special handling
            }
        }
    }
}

fn format_function_name(name: &str) -> String {
    if let Some(prefix) = name.find("___ZN") {
        // format!("{}_{}", &name[0..4], demangle(&name[prefix + 2..]))
        format!("{}", demangle(&name[prefix + 2..]))
    } else {
        format!("{}", demangle(name))
    }
}

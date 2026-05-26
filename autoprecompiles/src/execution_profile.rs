use crate::adapter::Adapter;
use crate::blocks::Program;
use std::collections::{BTreeMap, HashMap};
use std::io::Write;
use std::sync::Arc;
use std::sync::Mutex;
use tracing::dispatcher::Dispatch;
use tracing::field::Field as TracingField;
use tracing::{Event, Level, Subscriber};
use tracing_subscriber::{
    layer::Context,
    prelude::*,
    registry::{LookupSpan, Registry},
    Layer,
};

#[derive(Clone)]
/// Program execution information for PGO
pub struct ExecutionProfile {
    /// execution count of each pc
    pub pc_count: HashMap<u64, u32>,
    /// list of pcs executed in order
    pub pc_list: Vec<u64>,
}

/// Produces information about the program's execution for PGO.
/// Used in Pgo::Cell and Pgo::Instruction to help rank basic blocks to create APCs for.
pub fn execution_profile<A: Adapter>(
    program: &A::Program,
    execute_fn: impl FnOnce(),
) -> ExecutionProfile {
    // in memory collector storage
    let collector = PgoCollector::new();

    // build subscriber
    let subscriber = Registry::default().with(collector.clone());

    // dispatch constructs a local subscriber at trace level that is invoked during data collection but doesn't override the global one at info level
    let dispatch = Dispatch::new(subscriber);
    tracing::dispatcher::with_default(&dispatch, execute_fn);

    let pc_list = collector.take_pc_list();

    // Extract the collected data
    let pc_count = pc_list.iter().fold(HashMap::new(), |mut counts, pc| {
        *counts.entry(*pc).or_insert(0) += 1;
        counts
    });

    // the smallest pc is the same as the base_pc if there's no stdin
    let pc_min = pc_count.keys().min().unwrap();
    tracing::debug!("pc_min: {}; base_pc: {}", pc_min, program.base_pc());

    // print the total and by pc counts
    tracing::debug!("Pgo captured {} pc's", pc_count.len());

    if tracing::enabled!(Level::TRACE) {
        // print pc_index map in descending order of pc_index count
        let mut pc_index_count_sorted: Vec<_> = pc_count.iter().collect();
        pc_index_count_sorted.sort_by(|a, b| b.1.cmp(a.1));
        pc_index_count_sorted.iter().for_each(|(pc, count)| {
            tracing::trace!("pc_index {}: {}", pc, count);
        });
    }

    ExecutionProfile { pc_count, pc_list }
}

// holds basic type fields of execution objects captured in trace by subscriber
#[derive(Default)]
struct PgoData {
    pc: Option<u64>,
}

impl tracing::field::Visit for PgoData {
    // when we receive a u64 field, they are parsed into fields of the pgo data
    fn record_u64(&mut self, field: &tracing::field::Field, value: u64) {
        if field.name() == "pc" {
            self.pc = Some(value);
        }
    }

    // required for implementation, but in practice we will only receive u64 fields
    // the fields we receive are determined by the instruction trace print out of our openvm fork during execution
    fn record_debug(&mut self, _: &TracingField, _: &dyn std::fmt::Debug) {}
}

// A Layer that collects data we are interested in using for the pgo from the trace fields.
#[derive(Clone)]
struct PgoCollector {
    pc_list: Arc<Mutex<Vec<u64>>>,
}

impl PgoCollector {
    fn new() -> Self {
        Self {
            pc_list: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn increment(&self, pc: u64) {
        self.pc_list.lock().unwrap().push(pc);
    }

    fn take_pc_list(&self) -> Vec<u64> {
        std::mem::take(&mut self.pc_list.lock().unwrap())
    }
}

impl<S> Layer<S> for PgoCollector
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        // build a visitor to parse and hold trace fields we are interested in
        let mut visitor = PgoData::default();
        event.record(&mut visitor);

        // because our subscriber is at the trace level, for trace print outs that don't match PgoData,
        // the visitor can't parse them, and these cases are filtered out automatically
        if let Some(pc) = visitor.pc {
            self.increment(pc);
        }
    }
}

// ─── Flamechart profiler ────────────────────────────────────────────────────

/// State maintained by the flamechart collector during execution.
struct FlamechartState {
    /// Total instruction count (monotonic clock).
    insn_count: u64,
    /// Start PC of the function that owns the most-recently-seen PC.
    current_fn_start: Option<u32>,
    /// Simulated call stack: (fn_start_pc, fn_name).
    call_stack: Vec<(u32, String)>,
    /// Accumulated folded-stack samples: "fn1;fn2;fn3" → count.
    samples: HashMap<String, u64>,
}

impl Default for FlamechartState {
    fn default() -> Self {
        Self {
            insn_count: 0,
            current_fn_start: None,
            call_stack: Vec::new(),
            samples: HashMap::new(),
        }
    }
}

/// A tracing `Layer` that samples the simulated call stack every `sample_rate`
/// instructions and accumulates folded-stack counts.
#[derive(Clone)]
pub struct FlamechartCollector {
    fn_bounds: Arc<BTreeMap<u32, String>>,
    sample_rate: u64,
    state: Arc<Mutex<FlamechartState>>,
}

impl FlamechartCollector {
    /// Create a new collector.
    ///
    /// `entry_pc` is the ELF entry-point address.  The corresponding symbol is
    /// pushed as the initial (root) frame so that any function called before it
    /// returns (e.g. `memcpy` during early init) appears as a *child*, not as a
    /// root.
    pub fn new(fn_bounds: BTreeMap<u32, String>, sample_rate: u64, entry_pc: u32) -> Self {
        // Seed the call stack with the entry-point function.
        let initial_state = if let Some((fn_start, fn_name)) =
            fn_bounds.range(..=entry_pc).next_back()
        {
            FlamechartState {
                call_stack: vec![(*fn_start, fn_name.clone())],
                current_fn_start: Some(*fn_start),
                ..Default::default()
            }
        } else {
            FlamechartState::default()
        };
        Self {
            fn_bounds: Arc::new(fn_bounds),
            sample_rate,
            state: Arc::new(Mutex::new(initial_state)),
        }
    }

    fn on_pc(&self, pc: u64) {
        let pc32 = pc as u32;
        let mut state = self.state.lock().unwrap();
        state.insn_count += 1;

        // Look up the function that owns this PC.
        if let Some((fn_start, fn_name)) = self.fn_bounds.range(..=pc32).next_back() {
            let fn_start = *fn_start;
            if state.current_fn_start != Some(fn_start) {
                // Function boundary crossed.
                if let Some(pos) =
                    state.call_stack.iter().rposition(|(s, _)| *s == fn_start)
                {
                    // Function already in stack → return / tail call: pop up to it.
                    state.call_stack.truncate(pos + 1);
                } else {
                    // New function → call: push it.
                    state.call_stack.push((fn_start, fn_name.clone()));
                }
                state.current_fn_start = Some(fn_start);
            }
        }

        // Take a sample every `sample_rate` instructions.
        if state.insn_count % self.sample_rate == 0 {
            let folded: String = state
                .call_stack
                .iter()
                .map(|(_, name)| name.as_str())
                .collect::<Vec<_>>()
                .join(";");
            *state.samples.entry(folded).or_insert(0) += 1;
        }
    }

    /// Take ownership of the accumulated samples (drains the internal map).
    pub fn take_samples(&self) -> HashMap<String, u64> {
        std::mem::take(&mut self.state.lock().unwrap().samples)
    }
}

impl<S> Layer<S> for FlamechartCollector
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        let mut visitor = PgoData::default();
        event.record(&mut visitor);
        if let Some(pc) = visitor.pc {
            self.on_pc(pc);
        }
    }
}

/// Run `execute_fn` under a sampling flamechart profiler and write the result
/// in **folded stacks** format to `output`.
///
/// `fn_bounds` maps function-start PCs to demangled function names (built from
/// the guest ELF symbol table).  `sample_rate` is the number of instructions
/// between samples (e.g. 10 000).
///
/// The output is compatible with `flamegraph.pl`, `inferno-flamegraph`, and
/// the speedscope web UI (drag-and-drop, "Import > Custom" → "Stacks").
pub fn flamechart_profile(
    fn_bounds: BTreeMap<u32, String>,
    sample_rate: u64,
    entry_pc: u32,
    execute_fn: impl FnOnce(),
    output: &mut impl Write,
) {
    let collector = FlamechartCollector::new(fn_bounds, sample_rate, entry_pc);
    let subscriber = Registry::default().with(collector.clone());
    let dispatch = Dispatch::new(subscriber);
    tracing::dispatcher::with_default(&dispatch, execute_fn);

    let samples = collector.take_samples();
    for (stack, count) in &samples {
        writeln!(output, "{stack} {count}").expect("failed to write flamechart output");
    }
}

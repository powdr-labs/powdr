use crate::adapter::Adapter;
use crate::blocks::Program;
use std::collections::HashMap;
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

use crate::adapter::Adapter;
use crate::blocks::Program;
use std::collections::HashMap;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};
use tracing::dispatcher::Dispatch;
use tracing::field::Field as TracingField;
use tracing::{Event, Level, Subscriber};
use tracing_subscriber::{
    layer::Context,
    prelude::*,
    registry::{LookupSpan, Registry},
    Layer,
};

/// Produces execution count by pc
/// Used in Pgo::Cell and Pgo::Instruction to help rank basic blocks to create APCs for
pub fn execution_profile<A: Adapter>(
    program: &A::Program,
    execute_fn: impl FnOnce(),
) -> HashMap<u64, u32> {
    // in memory collector storage
    let collector = PgoCollector::new::<A>(program);

    // build subscriber
    let subscriber = Registry::default().with(collector.clone());

    // dispatch constructs a local subscriber at trace level that is invoked during data collection but doesn't override the global one at info level
    let dispatch = Dispatch::new(subscriber);
    tracing::dispatcher::with_default(&dispatch, execute_fn);

    // Extract the collected data
    let pc_index_count = collector.into_hashmap();

    // the smallest pc is the same as the base_pc if there's no stdin
    let pc_min = pc_index_count.keys().min().unwrap();
    tracing::debug!("pc_min: {}; base_pc: {}", pc_min, program.base_pc());

    // print the total and by pc counts
    tracing::debug!("Pgo captured {} pc's", pc_index_count.len());

    if tracing::enabled!(Level::TRACE) {
        // print pc_index map in descending order of pc_index count
        let mut pc_index_count_sorted: Vec<_> = pc_index_count.iter().collect();
        pc_index_count_sorted.sort_by(|a, b| b.1.cmp(a.1));
        pc_index_count_sorted.iter().for_each(|(pc, count)| {
            tracing::trace!("pc_index {}: {}", pc, count);
        });
    }

    pc_index_count
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
    step: u32,
    pc_base: u64,
    pc_index_map: Arc<Vec<AtomicU32>>,
}

impl PgoCollector {
    fn new<A: Adapter>(program: &A::Program) -> Self {
        let max_pc_index = program.length();
        // create a map with max_pc entries initialized to 0
        let pc_index_map = Arc::new((0..max_pc_index).map(|_| AtomicU32::new(0)).collect());
        Self {
            pc_index_map,
            step: program.pc_step(),
            pc_base: program.base_pc(),
        }
    }

    fn into_hashmap(self) -> HashMap<u64, u32> {
        // Turn the map into a HashMap of (pc_index, count)
        self.pc_index_map
            .iter()
            .enumerate()
            .filter_map(|(pc_index, count)| {
                let count = count.load(Ordering::Relaxed);

                // if the count is zero, we skip it
                if count == 0 {
                    return None;
                }
                let pc = self.pc_base + (pc_index as u64 * self.step as u64);

                Some((pc, count))
            })
            .collect()
    }

    fn increment(&self, pc: u64) {
        self.pc_index_map[(pc - self.pc_base) as usize / self.step as usize]
            .fetch_add(1, Ordering::Relaxed);
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

/// Produces a rich execution trace with per-cycle memory access information.
pub fn execution_data<A: Adapter>(_program: &A::Program, execute_fn: impl FnOnce()) -> Vec<Cycle> {
    let collector = PgoExecutionCollector::default();

    let subscriber = Registry::default().with(collector.clone());
    let dispatch = Dispatch::new(subscriber);
    tracing::dispatcher::with_default(&dispatch, execute_fn);

    collector.into_cycles()
}

// holds basic type fields of execution objects captured in trace by subscriber
#[derive(Default)]
struct CycleData {
    pc: Option<u64>,
    address_space: Option<u64>,
    address: Option<u64>,
    value: Option<u64>,
}

impl tracing::field::Visit for CycleData {
    // when we receive a u64 field, they are parsed into fields of the pgo data
    fn record_u64(&mut self, field: &tracing::field::Field, value: u64) {
        if field.name() == "pc" {
            self.pc = Some(value);
        } else if field.name() == "address_space" {
            self.address_space = Some(value);
        } else if field.name() == "address" {
            self.address = Some(value);
        } else if field.name() == "value" {
            self.value = Some(value);
        }
    }

    // required for implementation, but in practice we will only receive u64 fields
    // the fields we receive are determined by the instruction trace print out of our openvm fork during execution
    fn record_debug(&mut self, _: &TracingField, _: &dyn std::fmt::Debug) {}
}

impl CycleData {
    fn memory_access(&self) -> Option<MemoryAccess> {
        Some(MemoryAccess {
            address_space: self.address_space?,
            address: self.address?,
            value: self.value?,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MemoryAccess {
    pub address_space: u64,
    pub address: u64,
    pub value: u64,
}

/// Captures a complete description of a VM cycle.
#[derive(Debug, Clone)]
pub struct Cycle {
    pub pc: u64,
    pub reads: Vec<MemoryAccess>,
    pub writes: Vec<MemoryAccess>,
}

impl Cycle {
    fn new(pc: u64) -> Self {
        Self {
            pc,
            reads: Vec::new(),
            writes: Vec::new(),
        }
    }
}

#[derive(Default)]
struct ExecutionCollectorState {
    cycles: Vec<Cycle>,
    current_cycle: Option<Cycle>,
}

#[derive(Clone, Default)]
struct PgoExecutionCollector {
    state: Arc<Mutex<ExecutionCollectorState>>,
}

impl PgoExecutionCollector {
    fn start_cycle(&self, pc: u64) {
        let mut state = self
            .state
            .lock()
            .expect("execution collector state lock poisoned");
        if let Some(cycle) = state.current_cycle.take() {
            state.cycles.push(cycle);
        }
        state.current_cycle = Some(Cycle::new(pc));
    }

    fn record_read(&self, access: MemoryAccess) {
        self.record_access(access, false);
    }

    fn record_write(&self, access: MemoryAccess) {
        self.record_access(access, true);
    }

    fn record_access(&self, access: MemoryAccess, is_write: bool) {
        let mut state = self
            .state
            .lock()
            .expect("execution collector state lock poisoned");
        if let Some(cycle) = state.current_cycle.as_mut() {
            if is_write {
                cycle.writes.push(access);
            } else {
                cycle.reads.push(access);
            }
        }
    }

    fn into_cycles(self) -> Vec<Cycle> {
        let mut state = self
            .state
            .lock()
            .expect("execution collector state lock poisoned");
        if let Some(cycle) = state.current_cycle.take() {
            state.cycles.push(cycle);
        }
        std::mem::take(&mut state.cycles)
    }
}

impl<S> Layer<S> for PgoExecutionCollector
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        let mut visitor = CycleData::default();
        event.record(&mut visitor);

        if let Some(pc) = visitor.pc {
            self.start_cycle(pc);
        }

        match event.metadata().target() {
            "read" => {
                if let Some(access) = visitor.memory_access() {
                    self.record_read(access);
                }
            }
            "write" => {
                if let Some(access) = visitor.memory_access() {
                    self.record_write(access);
                }
            }
            _ => {}
        }
    }
}

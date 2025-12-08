use crate::adapter::Adapter;
use crate::blocks::Program;
use itertools::Itertools;
use std::collections::HashMap;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::RwLock;
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
pub struct ExecutionProfile {
    /// execution count of each pc
    pub pc_count: HashMap<u64, u32>,
    /// next pc count, for each pc
    // TODO: u32 count? itertools `count` returns usize
    pub next_pc: HashMap<u64, HashMap<u64, usize>>,
    /// list of pcs executed in order
    pub pc_list: Vec<u64>,
}

// Produces execution information for PGO.
// Used in Pgo::Cell and Pgo::Instruction to help rank basic blocks to create APCs for.
pub fn execution_profile<A: Adapter>(
    program: &A::Program,
    execute_fn: impl FnOnce(),
) -> ExecutionProfile {
    // in memory collector storage
    let collector = PgoCollector::new::<A>(program);

    // build subscriber
    let subscriber = Registry::default().with(collector.clone());

    // dispatch constructs a local subscriber at trace level that is invoked during data collection but doesn't override the global one at info level
    let dispatch = Dispatch::new(subscriber);
    tracing::dispatcher::with_default(&dispatch, execute_fn);

    let next_pcs_by_pc = {
        collector
            .pc_list
            .read()
            .unwrap()
            .iter()
            .tuple_windows()
            .map(|(a, b)| (*a, *b))
            .into_group_map()
            .into_iter()
            .map(|(pc, next_pcs)| (pc, next_pcs.into_iter().counts()))
            .collect::<HashMap<_, _>>()
    };

    // let insns: HashMap<_, _> = program.instructions().enumerate().map(|(idx, insn)| {
    //     let pc = program.instruction_index_to_pc(idx);
    //     (pc, insn)
    // }).collect();

    // for (pc, next) in next_pcs_by_pc.iter() {
    //     println!("pc {}: {:?}", pc, next.len());
    //     println!("\t{:?}", insns[pc]);
    // }

    let json = serde_json::to_string_pretty(&next_pcs_by_pc).unwrap();
    std::fs::write("next_pcs.json", json).unwrap();

    let pc_list = collector.pc_list.read().unwrap().clone();

    // Extract the collected data
    let pc_count = collector.into_hashmap();

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

    ExecutionProfile {
        pc_count,
        next_pc: next_pcs_by_pc,
        pc_list,
    }
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
    pub pc_list: Arc<RwLock<Vec<u64>>>,
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
            pc_list: Arc::new(RwLock::new(Vec::new())),
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
        self.pc_list.write().unwrap().push(pc);
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

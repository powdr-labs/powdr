use crate::adapter::{Adapter, AdapterVmConfig};
use crate::blocks::{BasicBlock, PcStep, Program};
use crate::empirical_constraints::EmpiricalConstraints;
use crate::evaluation::evaluate_apc;
use crate::DegreeBound;
use crate::InstructionHandler;
use indicatif::{ProgressBar, ProgressStyle};
use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};
use tracing::dispatcher::Dispatch;
use tracing::field::Field as TracingField;
use tracing::{Event, Subscriber};
use tracing_subscriber::{
    layer::Context,
    prelude::*,
    registry::{LookupSpan, Registry},
    Layer,
};

/// Result of computing full circuit effectiveness
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FullCircuitEffectiveness {
    /// Total number of main columns before optimization (sum across all chunks)
    pub total_main_columns_before: usize,
    /// Total number of main columns after optimization (sum across all chunks)
    pub total_main_columns_after: usize,
    /// Effectiveness ratio (before / after)
    pub effectiveness: f64,
    /// Number of chunks processed
    pub num_chunks: usize,
    /// Total number of instructions in the execution
    pub total_instructions: usize,
}

/// Computes the effectiveness of compiling the entire execution into circuits
/// by chunking the execution and processing each chunk in parallel.
pub fn compute_full_circuit_effectiveness<A: Adapter + Send + Sync>(
    program: &A::Program,
    execute_fn: impl FnOnce(),
    vm_config: AdapterVmConfig<A>,
    degree_bound: DegreeBound,
    chunk_size: usize,
) -> FullCircuitEffectiveness
where
    A::Instruction: Send + Sync,
{
    // Capture the execution trace
    tracing::info!("Capturing execution trace...");
    let pc_trace = capture_pc_trace::<A>(program, execute_fn);
    let total_instructions = pc_trace.len();
    tracing::info!("Captured {} PCs in the execution trace", total_instructions);

    // Chunk the execution into basic blocks
    let chunks = chunk_execution_into_blocks::<A>(
        program,
        &pc_trace,
        chunk_size,
        vm_config.instruction_handler,
    );
    let num_chunks = chunks.len();
    tracing::info!(
        "Chunked execution into {} chunks of up to {} instructions each",
        num_chunks,
        chunk_size
    );

    // Process chunks in parallel
    tracing::info!("Processing chunks in parallel...");
    let pb = ProgressBar::new(num_chunks as u64).with_style(
        ProgressStyle::with_template("[{elapsed_precise}] [{bar:50}] {wide_msg}").unwrap(),
    );
    let results: Vec<_> = chunks
        .into_par_iter()
        .map(|block| {
            // Build the APC for this chunk
            let apc = match crate::build::<A>(
                block.clone(),
                vm_config.clone(),
                degree_bound,
                None, // No APC candidates directory
                &EmpiricalConstraints::default(),
            ) {
                Ok(apc) => apc,
                Err(e) => {
                    tracing::error!("Failed to build APC for block (pcs: {:?}):", block.pcs);
                    for (statement, pc) in block.statements.iter().zip_eq(block.pcs.iter()) {
                        tracing::error!("   0x{:x} {}", pc, statement);
                    }
                    tracing::error!("Error: {:?}", e);
                    panic!("APC build failed");
                }
            };

            // Evaluate the APC to get before/after stats
            // TODO: Get total columns. In OpenVM, this can be done by doing:
            // stats().widths.{before,after}.total()
            let apc_with_stats = evaluate_apc::<A>(block, vm_config.instruction_handler, apc);
            let eval_result = apc_with_stats.evaluation_result();

            pb.inc(1);
            (
                eval_result.before.main_columns,
                eval_result.after.main_columns,
            )
        })
        .collect();

    // Aggregate results
    let (total_before, total_after): (usize, usize) = results
        .into_iter()
        .fold((0, 0), |(acc_before, acc_after), (before, after)| {
            (acc_before + before, acc_after + after)
        });

    let effectiveness = if total_after > 0 {
        total_before as f64 / total_after as f64
    } else {
        0.0
    };

    tracing::info!(
        "Full circuit effectiveness: {:.2}x ({} -> {} main columns)",
        effectiveness,
        total_before,
        total_after
    );

    FullCircuitEffectiveness {
        total_main_columns_before: total_before,
        total_main_columns_after: total_after,
        effectiveness,
        num_chunks,
        total_instructions,
    }
}

/// Chunks the execution trace into basic blocks of the specified size.
/// Only includes instructions that are allowed by the instruction handler.
fn chunk_execution_into_blocks<A: Adapter>(
    program: &A::Program,
    pc_trace: &[u64],
    chunk_size: usize,
    instruction_handler: &A::InstructionHandler,
) -> Vec<BasicBlock<A::Instruction>> {
    let pc_step = A::Instruction::pc_step() as u64;

    // Build a map of PC to instruction
    let mut pc_to_instruction = std::collections::HashMap::new();
    let base_pc = program.base_pc();
    for (i, instruction) in program.instructions().enumerate() {
        let pc = base_pc + (i as u64 * pc_step);
        pc_to_instruction.insert(pc, instruction);
    }

    // Chunk the trace, filtering out disallowed instructions
    let mut block_builder = BlockBuilder::new(chunk_size);
    for &pc in pc_trace {
        let instruction = pc_to_instruction.get(&pc).unwrap();
        // Only include instructions that are allowed by the instruction handler
        if instruction_handler.is_allowed(instruction) {
            block_builder.add_instruction(pc, instruction.clone());
        }
    }

    block_builder.into_blocks()
}

struct BlockBuilder<I> {
    current_pcs: Vec<u64>,
    current_statements: Vec<I>,
    blocks: Vec<BasicBlock<I>>,
    chunk_size: usize,
}

impl<I> BlockBuilder<I> {
    fn new(chunk_size: usize) -> Self {
        Self {
            current_pcs: Vec::new(),
            current_statements: Vec::new(),
            blocks: Vec::new(),
            chunk_size,
        }
    }

    fn add_instruction(&mut self, pc: u64, instruction: I) {
        if self.current_statements.len() >= self.chunk_size {
            self.finish_current_block();
        }
        self.current_pcs.push(pc);
        self.current_statements.push(instruction);
    }

    fn finish_current_block(&mut self) {
        if !self.current_statements.is_empty() {
            self.blocks.push(BasicBlock {
                pcs: std::mem::take(&mut self.current_pcs),
                statements: std::mem::take(&mut self.current_statements),
            });
        }
    }

    fn into_blocks(mut self) -> Vec<BasicBlock<I>> {
        self.finish_current_block();
        self.blocks
    }
}

/// Captures the list of PCs during execution (not just counts)
pub fn capture_pc_trace<A: Adapter>(program: &A::Program, execute_fn: impl FnOnce()) -> Vec<u64> {
    // in memory collector storage
    let collector = TraceCollector::new::<A>(program);

    // build subscriber
    let subscriber = Registry::default().with(collector.clone());

    // dispatch constructs a local subscriber at trace level that is invoked during data collection but doesn't override the global one at info level
    let dispatch = Dispatch::new(subscriber);
    tracing::dispatcher::with_default(&dispatch, execute_fn);

    // Extract the collected data
    let pc_trace = collector.into_vec();
    tracing::debug!("Captured {} PCs in the execution trace", pc_trace.len());
    pc_trace
}

// holds basic type fields of execution objects captured in trace by subscriber
#[derive(Default)]
struct TraceData {
    pc: Option<u64>,
}

impl tracing::field::Visit for TraceData {
    // when we receive a u64 field, they are parsed into fields of the trace data
    fn record_u64(&mut self, field: &tracing::field::Field, value: u64) {
        if field.name() == "pc" {
            self.pc = Some(value);
        }
    }

    // required for implementation, but in practice we will only receive u64 fields
    // the fields we receive are determined by the instruction trace print out of our openvm fork during execution
    fn record_debug(&mut self, _: &TracingField, _: &dyn std::fmt::Debug) {}
}

// A Layer that collects the list of PCs during execution
#[derive(Clone)]
struct TraceCollector {
    pc_trace: Arc<Mutex<Vec<u64>>>,
}

impl TraceCollector {
    fn new<A: Adapter>(_program: &A::Program) -> Self {
        Self {
            pc_trace: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn into_vec(self) -> Vec<u64> {
        match Arc::try_unwrap(self.pc_trace) {
            Ok(mutex) => mutex.into_inner().unwrap(),
            Err(arc) => {
                // If we can't unwrap the Arc, just lock it and clone the vec
                arc.lock().unwrap().clone()
            }
        }
    }

    fn push(&self, pc: u64) {
        self.pc_trace.lock().unwrap().push(pc);
    }
}

impl<S> Layer<S> for TraceCollector
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        // build a visitor to parse and hold trace fields we are interested in
        let mut visitor = TraceData::default();
        event.record(&mut visitor);

        // because our subscriber is at the trace level, for trace print outs that don't match TraceData,
        // the visitor can't parse them, and these cases are filtered out automatically
        if let Some(pc) = visitor.pc {
            self.push(pc);
        }
    }
}

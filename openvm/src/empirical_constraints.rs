use itertools::Itertools;
use openvm_circuit::arch::VmCircuitConfig;
use openvm_sdk::StdIn;
use openvm_stark_backend::p3_field::FieldAlgebra;
use openvm_stark_backend::p3_maybe_rayon::prelude::IntoParallelIterator;
use openvm_stark_backend::p3_maybe_rayon::prelude::ParallelIterator;
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::bus_map::BusType;
use powdr_autoprecompiles::empirical_constraints::BlockCell;
use powdr_autoprecompiles::empirical_constraints::Partition;
use powdr_autoprecompiles::empirical_constraints::{DebugInfo, EmpiricalConstraints};
use powdr_autoprecompiles::expression::AlgebraicEvaluator;
use powdr_autoprecompiles::expression::RowEvaluator;
use powdr_autoprecompiles::DegreeBound;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::iter::once;

use crate::bus_map::default_openvm_bus_map;
use crate::trace_generation::do_with_cpu_trace;
use crate::{CompiledProgram, OriginalCompiledProgram};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Timestamp {
    // Note that the order of the fields matters for correct ordering.
    segment_idx: usize,
    value: u32,
}

/// A single row in the execution trace
#[derive(Debug)]
struct Row {
    /// The program counter value for this row
    pc: u32,
    /// The timestamp for this row (segment index, row index within segment)
    timestamp: Timestamp,
    /// The values of the cells in this row
    cells: Vec<u32>,
}

/// Materialized execution trace
#[derive(Default)]
struct Trace {
    /// The raw rows, in any order
    rows: Vec<Row>,
}

impl Trace {
    /// Groups rows by their program counter value. The order of rows within each PC group is arbitrary.
    fn rows_by_pc(&self) -> BTreeMap<u32, Vec<&Row>> {
        self.rows.iter().fold(BTreeMap::new(), |mut acc, row| {
            acc.entry(row.pc).or_insert(Vec::new()).push(row);
            acc
        })
    }

    /// Returns all rows sorted by their timestamp
    fn rows_sorted_by_time(&self) -> impl Iterator<Item = &Row> {
        self.rows.iter().sorted_by_key(|row| &row.timestamp)
    }
}

pub fn detect_empirical_constraints(
    program: &OriginalCompiledProgram,
    degree_bound: DegreeBound,
    inputs: Vec<StdIn>,
) -> EmpiricalConstraints {
    tracing::info!("Collecting empirical constraints...");
    let blocks = program.collect_basic_blocks(degree_bound.identities);
    let instruction_counts = blocks
        .iter()
        .map(|block| (block.start_pc, block.statements.len()))
        .collect();

    // Collect trace, without any autoprecompiles.
    let program = program.compiled_program(degree_bound.identities);

    let mut constraint_detector = ConstraintDetector::new(instruction_counts);

    let num_inputs = inputs.len();
    for (i, input) in inputs.into_iter().enumerate() {
        tracing::info!("  Processing input {} / {}", i + 1, num_inputs);
        // Materialize the full trace for a given input.
        // If this becomes a RAM issue, we can also pass individual segments to process_trace.
        // The advantage of the current approach is that the percentiles can be computed more accurately.
        tracing::info!("    Collecting trace...");
        let (trace, new_debug_info) = collect_trace(&program, input, degree_bound.identities);
        tracing::info!("    Detecting constraints...");
        constraint_detector.process_trace(trace, new_debug_info);
    }
    tracing::info!("Done collecting empirical constraints.");

    constraint_detector.finalize()
}

fn collect_trace(
    program: &CompiledProgram,
    inputs: StdIn,
    degree_bound: usize,
) -> (Trace, DebugInfo) {
    let mut trace = Trace::default();
    let mut debug_info = DebugInfo::default();

    do_with_cpu_trace(program, inputs, |seg_idx, vm, _pk, ctx| {
        let airs = program.vm_config.sdk.airs(degree_bound).unwrap();
        let global_airs = vm
            .config()
            .create_airs()
            .unwrap()
            .into_airs()
            .enumerate()
            .collect::<HashMap<_, _>>();

        for (air_id, proving_context) in &ctx.per_air {
            let main = proving_context.common_main.as_ref().unwrap();
            let air_name = global_airs[air_id].name();
            let Some((machine, _)) = &airs.air_name_to_machine.get(&air_name) else {
                // air_name_to_machine only contains instruction AIRs, and we are only
                // interested in those here.
                continue;
            };
            assert!(
                proving_context.cached_mains.is_empty(),
                "Unexpected cached main in {air_name}."
            );

            // Find the execution bus interaction
            // This assumes there is exactly one, which is the case for instruction chips
            let execution_bus_interaction = machine
                .bus_interactions
                .iter()
                .find(|interaction| {
                    interaction.id
                        == default_openvm_bus_map()
                            .get_bus_id(&BusType::ExecutionBridge)
                            .unwrap()
                })
                .unwrap();

            for row in main.row_slices() {
                // Create an evaluator over this row
                let evaluator = RowEvaluator::new(row);

                // Evaluate the execution bus interaction
                let execution = evaluator.eval_bus_interaction(execution_bus_interaction);

                // `is_valid` is the multiplicity
                let is_valid = execution.mult;
                if is_valid == BabyBear::ZERO {
                    // If `is_valid` is zero, this is a padding row
                    continue;
                }

                // Recover the values of the pc and timestamp
                let [pc, timestamp] = execution
                    .args
                    .map(|v| v.as_canonical_u32())
                    .collect_vec()
                    .try_into()
                    .unwrap();

                // Convert the row to u32s
                let row = row.iter().map(|v| v.as_canonical_u32()).collect();

                let row = Row {
                    cells: row,
                    pc,
                    timestamp: Timestamp {
                        segment_idx: seg_idx,
                        value: timestamp,
                    },
                };
                trace.rows.push(row);

                match debug_info.air_id_by_pc.entry(pc) {
                    Entry::Vacant(entry) => {
                        entry.insert(*air_id);
                    }
                    Entry::Occupied(existing) => {
                        assert_eq!(*existing.get(), *air_id);
                    }
                }
                if !debug_info.column_names_by_air_id.contains_key(air_id) {
                    debug_info.column_names_by_air_id.insert(
                        *air_id,
                        machine.main_columns().map(|r| (*r.name).clone()).collect(),
                    );
                }
            }
        }
    })
    .unwrap();
    (trace, debug_info)
}

struct ConstraintDetector {
    /// Mapping from block PC to number of instructions in that block
    instruction_counts: HashMap<u64, usize>,
    empirical_constraints: EmpiricalConstraints,
}

/// An instance of a basic block in the trace
struct ConcreteBlock<'a> {
    rows: Vec<&'a Row>,
}

impl<'a> ConcreteBlock<'a> {
    fn equivalence_classes(&self) -> Partition<BlockCell> {
        self.rows
            .iter()
            .enumerate()
            // Map each cell to a (value, (instruction_index, col_index)) pair
            .flat_map(|(instruction_index, row)| {
                row.cells
                    .iter()
                    .enumerate()
                    .map(|(col_index, v)| (*v, BlockCell::new(instruction_index, col_index)))
                    .collect::<Vec<_>>()
            })
            // Group by value
            .into_group_map()
            .into_values()
            .map(|cells| cells.into_iter().collect())
            .collect()
    }
}

impl ConstraintDetector {
    pub fn new(instruction_counts: HashMap<u64, usize>) -> Self {
        Self {
            instruction_counts,
            empirical_constraints: EmpiricalConstraints::default(),
        }
    }

    pub fn finalize(self) -> EmpiricalConstraints {
        self.empirical_constraints
    }

    pub fn process_trace(&mut self, trace: Trace, debug_info: DebugInfo) {
        let pc_counts = trace
            .rows_by_pc()
            .into_iter()
            .map(|(pc, rows)| (pc, rows.len() as u64))
            .collect();
        // Compute empirical constraints from the current trace
        tracing::info!("      Detecting equivalence classes by block...");
        let equivalence_classes_by_block = self.generate_equivalence_classes_by_block(&trace);
        tracing::info!("      Detecting column ranges by PC...");
        let column_ranges_by_pc = self.detect_column_ranges_by_pc(trace);
        let new_empirical_constraints = EmpiricalConstraints {
            column_ranges_by_pc,
            equivalence_classes_by_block,
            debug_info,
            pc_counts,
        };

        // Combine the new empirical constraints and debug info with the existing ones
        self.empirical_constraints
            .combine_with(new_empirical_constraints);
    }

    fn detect_column_ranges_by_pc(&self, trace: Trace) -> BTreeMap<u32, Vec<(u32, u32)>> {
        // Map all column values to their range (1st and 99th percentile) for each pc
        trace
            .rows_by_pc()
            .into_iter()
            .map(|(pc, rows)| (pc, self.detect_column_ranges(&rows)))
            .collect()
    }

    fn detect_column_ranges(&self, rows: &[&Row]) -> Vec<(u32, u32)> {
        for row in rows {
            // All rows for a given PC should be in the same chip
            assert_eq!(row.cells.len(), rows[0].cells.len());
        }

        (0..rows[0].cells.len())
            .map(|col_index| {
                let mut values = rows
                    .iter()
                    .map(|row| row.cells[col_index])
                    .collect::<Vec<_>>();
                values.sort_unstable();
                let len = values.len();
                let p1_index = len / 100; // 1st percentile
                let p99_index = len * 99 / 100; // 99th percentile
                (values[p1_index], values[p99_index])
            })
            .collect()
    }

    fn generate_equivalence_classes_by_block(
        &self,
        trace: &Trace,
    ) -> BTreeMap<u64, Partition<BlockCell>> {
        tracing::info!("        Segmenting trace into blocks...");
        let blocks = self.get_blocks(trace);
        tracing::info!("        Finding equivalence classes...");

        use std::sync::atomic::{AtomicU64, Ordering};
        let build_time_us = AtomicU64::new(0);
        let intersect_time_us = AtomicU64::new(0);
        let num_blocks = AtomicU64::new(0);
        let num_instances = AtomicU64::new(0);
        let num_cells = AtomicU64::new(0);

        let result = blocks
            .into_par_iter()
            .map(|(block_id, block_instances)| {
                num_blocks.fetch_add(1, Ordering::Relaxed);
                num_instances.fetch_add(block_instances.len() as u64, Ordering::Relaxed);
                // Count total cells: instances × rows × cells_per_row
                let cells_in_block: u64 = block_instances
                    .iter()
                    .map(|b| b.rows.iter().map(|r| r.cells.len() as u64).sum::<u64>())
                    .sum();
                num_cells.fetch_add(cells_in_block, Ordering::Relaxed);

                // Segment each block instance into equivalence classes
                let build_start = std::time::Instant::now();
                let partition_by_block_instance = block_instances
                    .into_iter()
                    .map(|block| block.equivalence_classes())
                    .collect::<Vec<_>>();
                build_time_us.fetch_add(build_start.elapsed().as_micros() as u64, Ordering::Relaxed);

                // Intersect the equivalence classes across all instances of the block
                let intersect_start = std::time::Instant::now();
                let intersected = Partition::intersect(&partition_by_block_instance);
                intersect_time_us.fetch_add(intersect_start.elapsed().as_micros() as u64, Ordering::Relaxed);

                (block_id, intersected)
            })
            .collect();

        tracing::info!("        Blocks: {}, Instances: {}, Cells: {}, Build: {}ms, Intersect: {}ms",
            num_blocks.load(Ordering::Relaxed),
            num_instances.load(Ordering::Relaxed),
            num_cells.load(Ordering::Relaxed),
            build_time_us.load(Ordering::Relaxed) / 1000,
            intersect_time_us.load(Ordering::Relaxed) / 1000);

        result
    }

    /// Segments a trace into basic blocks.
    /// Returns a mapping from block ID to all instances of that block in the trace.
    fn get_blocks<'a>(&self, trace: &'a Trace) -> BTreeMap<u64, Vec<ConcreteBlock<'a>>> {
        trace
            .rows_sorted_by_time()
            // take entire blocks from the rows
            .batching(|it| {
                let first = it.next()?;
                let block_id = first.pc as u64;

                if let Some(&count) = self.instruction_counts.get(&block_id) {
                    let rows = once(first).chain(it.take(count - 1)).collect_vec();

                    for (r1, r2) in rows.iter().tuple_windows() {
                        assert_eq!(r2.pc, r1.pc + 4);
                    }

                    Some(Some((block_id, ConcreteBlock { rows })))
                } else {
                    // Single instruction block, yield `None` to be filtered.
                    Some(None)
                }
            })
            // filter out single instruction blocks
            .flatten()
            // collect by start_pc
            .fold(Default::default(), |mut block_rows, (block_id, chunk)| {
                block_rows.entry(block_id).or_insert(Vec::new()).push(chunk);
                block_rows
            })
    }
}

#[cfg(test)]
mod tests {
    use powdr_autoprecompiles::equivalence_classes::EquivalenceClass;

    use super::*;

    fn make_trace(rows_by_time_with_pc: Vec<(u32, Vec<u32>)>) -> Trace {
        Trace {
            rows: rows_by_time_with_pc
                .into_iter()
                .enumerate()
                .map(|(clk, (pc, cells))| Row {
                    cells,
                    pc,
                    timestamp: Timestamp {
                        segment_idx: 0,
                        value: clk as u32,
                    },
                })
                .collect(),
        }
    }

    #[test]
    fn test_constraint_detector() {
        // Assume the following test program:
        // ADDI x1, x1, 1    // note how the second operand is always 1
        // BLT x1, x2, -4    // Note how the first operand is always equal to the result of the previous ADDI

        let instruction_counts = vec![(0, 2)].into_iter().collect();
        let mut detector = ConstraintDetector::new(instruction_counts);

        let trace1 = make_trace(vec![
            (0, vec![1, 0, 1]),  // ADDI: 0 + 1 = 1
            (4, vec![0, 1, 2]),  // BLT: 1 < 2 => PC = 0
            (0, vec![2, 1, 1]),  // ADDI: 1 + 1 = 2
            (4, vec![12, 2, 2]), // BLT: 2 >= 2 => PC = 8
        ]);
        detector.process_trace(trace1, DebugInfo::default());

        let empirical_constraints = detector.finalize();

        assert_eq!(
            empirical_constraints.column_ranges_by_pc.get(&0),
            // For the ADDI instruction, the second operand (col 2) is always 1; the other columns vary
            Some(&vec![(1, 2), (0, 1), (1, 1)])
        );
        assert_eq!(
            empirical_constraints.column_ranges_by_pc.get(&4),
            // For the BLT instruction, second operand (col 2) is always 2; the other columns vary
            Some(&vec![(0, 12), (1, 2), (2, 2)])
        );

        let equivalence_classes = empirical_constraints
            .equivalence_classes_by_block
            .get(&0)
            .unwrap();
        println!("Equivalence classes: {:?}", equivalence_classes);
        let expected: Partition<_> = once(
            // The result of the first instruction (col 0) is always equal to the
            // first operand of the second instruction (col 1)
            [BlockCell::new(0, 0), BlockCell::new(1, 1)]
                .into_iter()
                .collect::<EquivalenceClass<_>>(),
        )
        .collect();
        assert_eq!(*equivalence_classes, expected,);
    }
}

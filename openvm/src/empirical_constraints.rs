use itertools::Itertools;
use openvm_sdk::StdIn;
use openvm_stark_backend::p3_maybe_rayon::prelude::IntoParallelIterator;
use openvm_stark_backend::p3_maybe_rayon::prelude::ParallelIterator;
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::BasicBlock;
use powdr_autoprecompiles::empirical_constraints::BlockCell;
use powdr_autoprecompiles::empirical_constraints::EquivalenceClasses;
use powdr_autoprecompiles::empirical_constraints::{
    intersect_partitions, DebugInfo, EmpiricalConstraints,
};
use powdr_autoprecompiles::expression::AlgebraicEvaluator;
use powdr_autoprecompiles::expression::RowEvaluator;
use powdr_autoprecompiles::DegreeBound;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::iter::once;

use crate::trace_generation::do_with_cpu_trace;
use crate::{CompiledProgram, OriginalCompiledProgram};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Timestamp {
    segment_id: u32,
    value: u32,
}

#[derive(Debug)]
struct Row {
    pc: u32,
    timestamp: Timestamp,
    cells: Vec<u32>,
}

/// Materialized execution trace, Indexed by time and by PC
#[derive(Default)]
struct Trace {
    /// The raw rows, in any order
    rows: Vec<Row>,
}

impl Trace {
    fn rows_by_pc(&self) -> BTreeMap<u32, Vec<&Row>> {
        self.rows.iter().fold(BTreeMap::new(), |mut acc, row| {
            acc.entry(row.pc).or_insert(Vec::new()).push(row);
            acc
        })
    }

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

    // Collect trace, without any autoprecompiles.
    let program = program.compiled_program(Vec::new(), degree_bound.identities);

    let mut constraint_detector = ConstraintDetector::new(&blocks);

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

use openvm_stark_backend::p3_field::FieldAlgebra;

fn collect_trace(
    program: &CompiledProgram,
    inputs: StdIn,
    degree_bound: usize,
) -> (Trace, DebugInfo) {
    let mut trace = Trace::default();
    let mut debug_info = DebugInfo::default();

    do_with_cpu_trace(program, inputs, |segment_idx, _vm, _pk, ctx| {
        let airs = program.vm_config.sdk.airs(degree_bound).unwrap();

        for (air_id, proving_context) in &ctx.per_air {
            if !proving_context.cached_mains.is_empty() {
                // Instruction chips always have a cached main.
                continue;
            }
            let main = proving_context.common_main.as_ref().unwrap();
            let (symbolic_machine, _) = airs.machine_by_insertion_idx.get(air_id).unwrap();

            let execution_bus_interaction = symbolic_machine
                .bus_interactions
                .iter()
                .find(|interaction| interaction.id == 0)
                .unwrap();

            for row in main.row_slices() {
                let evaluator = RowEvaluator::new(row);
                let execution = evaluator.eval_bus_interaction(execution_bus_interaction);
                let is_valid = execution.mult;
                if is_valid == BabyBear::ZERO {
                    continue;
                }
                let [pc_value, ts_value] = execution
                    .args
                    .map(|v| v.as_canonical_u32())
                    .collect_vec()
                    .try_into()
                    .unwrap();

                let row = row.iter().map(|v| v.as_canonical_u32()).collect();

                let row = Row {
                    cells: row,
                    pc: pc_value,
                    timestamp: Timestamp {
                        segment_id: segment_idx,
                        value: ts_value,
                    },
                };
                trace.rows.push(row);

                match debug_info.air_id_by_pc.entry(pc_value) {
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
                        symbolic_machine
                            .main_columns()
                            .map(|r| (*r.name).clone())
                            .collect(),
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
    instruction_count_by_start_pc: HashMap<u64, usize>,
    empirical_constraints: EmpiricalConstraints,
}

struct ConcreteBlock<'a> {
    rows: Vec<&'a Row>,
}

impl<'a> ConcreteBlock<'a> {
    fn equivalence_classes(&self) -> EquivalenceClasses<BlockCell> {
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
    pub fn new<I>(blocks: &[BasicBlock<I>]) -> Self {
        Self {
            instruction_count_by_start_pc: blocks
                .iter()
                .map(|block| (block.start_pc, block.statements.len()))
                .collect(),
            empirical_constraints: EmpiricalConstraints::default(),
        }
    }

    pub fn finalize(self) -> EmpiricalConstraints {
        self.empirical_constraints
    }

    pub fn process_trace(&mut self, trace: Trace, debug_info: DebugInfo) {
        // Compute empirical constraints from the current trace
        tracing::info!("      Detecting equivalence classes by block...");
        let equivalence_classes_by_block = self.generate_equivalence_classes_by_block(&trace);
        tracing::info!("      Detecting column ranges by PC...");
        let column_ranges_by_pc = self.detect_column_ranges_by_pc(trace);
        let new_empirical_constraints = EmpiricalConstraints {
            column_ranges_by_pc,
            equivalence_classes_by_block,
            debug_info,
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
    ) -> BTreeMap<u64, EquivalenceClasses<BlockCell>> {
        tracing::info!("        Segmenting trace into blocks...");
        let blocks = self.get_blocks(trace);
        tracing::info!("        Finding equivalence classes...");
        blocks
            .into_par_iter()
            .map(|(block_id, block_instances)| {
                // Segment each block instance into equivalence classes
                let classes = block_instances
                    .iter()
                    .map(|block| block.equivalence_classes())
                    .collect::<Vec<_>>();

                // Intersect the equivalence classes across all instances of the block
                let intersected = intersect_partitions(classes);

                (block_id, intersected)
            })
            .collect()
    }

    /// Segments a trace into basic blocks.
    /// Returns a mapping from block start pc to all instances of that block in the trace.
    fn get_blocks<'a>(&self, trace: &'a Trace) -> BTreeMap<u64, Vec<ConcreteBlock<'a>>> {
        trace
            .rows_sorted_by_time()
            // take entire blocks from the rows
            .batching(|it| {
                let first = it.next()?;
                let block_id = first.pc as u64;

                if let Some(&count) = self.instruction_count_by_start_pc.get(&block_id) {
                    let rows = once(first).chain(it.take(count - 1)).collect_vec();

                    for (r1, r2) in rows.iter().tuple_windows() {
                        assert_eq!(r2.pc, r1.pc + 4);
                    }

                    Some(Some((block_id, ConcreteBlock { rows })))
                } else {
                    // Single instruction block, ignore.
                    Some(None)
                }
            })
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
    use powdr_autoprecompiles::empirical_constraints::EquivalenceClass;

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
                        segment_id: 0,
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

        let instruction_counts = vec![BasicBlock {
            start_pc: 0,
            statements: vec![(), ()],
        }];
        let mut detector = ConstraintDetector::new(&instruction_counts);

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
        let expected: EquivalenceClasses<_> = once(
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

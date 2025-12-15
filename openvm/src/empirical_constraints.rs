use itertools::Itertools;
use openvm_circuit::arch::VmCircuitConfig;
use openvm_sdk::StdIn;
use openvm_stark_backend::p3_maybe_rayon::prelude::IntoParallelIterator;
use openvm_stark_backend::p3_maybe_rayon::prelude::ParallelIterator;
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::empirical_constraints::EquivalenceClass;
use powdr_autoprecompiles::empirical_constraints::Partition;
use powdr_autoprecompiles::empirical_constraints::VariableId;
use powdr_autoprecompiles::empirical_constraints::{DebugInfo, EmpiricalConstraints};
use powdr_autoprecompiles::DegreeBound;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::collections::HashMap;

use crate::trace_generation::do_with_trace;
use crate::{CompiledProgram, OriginalCompiledProgram};

/// A single row in the execution trace
#[derive(Debug)]
struct Row {
    /// The program counter value for this row
    pc: u32,
    /// The timestamp for this row (segment index, row index within segment)
    timestamp: (u32, u32),
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
    fn rows_by_time(&self) -> Vec<&Row> {
        self.rows
            .iter()
            .sorted_by_key(|row| row.timestamp)
            .collect()
    }
}

pub fn detect_empirical_constraints(
    program: &OriginalCompiledProgram,
    degree_bound: DegreeBound,
    inputs: Vec<StdIn>,
) -> (EmpiricalConstraints, DebugInfo) {
    tracing::info!("Collecting empirical constraints...");
    let blocks = program.collect_basic_blocks(degree_bound.identities);
    let instruction_counts = blocks
        .iter()
        .map(|block| (block.start_pc, block.statements.len()))
        .collect();

    // Collect trace, without any autoprecompiles.
    let program = program.compiled_program(Vec::new(), degree_bound.identities);

    let mut constraint_detector = ConstraintDetector::new(instruction_counts);

    let num_inputs = inputs.len();
    for (i, input) in inputs.into_iter().enumerate() {
        tracing::info!("  Processing input {} / {}", i + 1, num_inputs);
        // Materialize the full trace for a given input.
        // If this becomes a RAM issue, we can also pass individual segments to process_trace.
        // The advantage of the current approach is that the percentiles can be computed more accurately.
        tracing::info!("    Collecting trace...");
        let (trace, new_debug_info) = collect_trace(&program, input);
        tracing::info!("    Detecting constraints...");
        constraint_detector.process_trace(trace, new_debug_info);
    }
    tracing::info!("Done collecting empirical constraints.");

    constraint_detector.finalize()
}

fn collect_trace(program: &CompiledProgram, inputs: StdIn) -> (Trace, DebugInfo) {
    let mut trace = Trace::default();
    let mut debug_info = DebugInfo::default();

    do_with_trace(program, inputs, |seg_idx, vm, _pk, ctx| {
        let global_airs = vm
            .config()
            .create_airs()
            .unwrap()
            .into_airs()
            .enumerate()
            .collect::<HashMap<_, _>>();

        for (air_id, proving_context) in &ctx.per_air {
            let air = &global_airs[air_id];
            let Some(column_names) = air.columns() else {
                // Instruction chips always have column names.
                continue;
            };

            if !proving_context.cached_mains.is_empty() {
                // Instruction chips always have a cached main.
                continue;
            }
            let main = proving_context.common_main.as_ref().unwrap();
            assert_eq!(main.width, column_names.len());

            // Instruction chips have a PC and timestamp
            let find_col = |name: &str| -> Option<usize> {
                column_names.iter().position(|col_name| {
                    col_name == name || col_name == &format!("inner__{}", name)
                })
            };
            let Some(pc_index) = find_col("from_state__pc") else {
                continue;
            };
            let ts_index = find_col("from_state__timestamp").unwrap();

            for row in main.row_slices() {
                let row = row.iter().map(|v| v.as_canonical_u32()).collect::<Vec<_>>();
                let pc_value = row[pc_index];
                let ts_value = row[ts_index];

                if pc_value == 0 {
                    // Padding row!
                    continue;
                }

                let row = Row {
                    cells: row,
                    pc: pc_value,
                    timestamp: (seg_idx as u32, ts_value),
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
                    debug_info
                        .column_names_by_air_id
                        .insert(*air_id, column_names.clone());
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
    debug_info: DebugInfo,
}

impl ConstraintDetector {
    pub fn new(instruction_counts: HashMap<u64, usize>) -> Self {
        Self {
            instruction_counts,
            empirical_constraints: EmpiricalConstraints::default(),
            debug_info: DebugInfo::default(),
        }
    }

    pub fn finalize(self) -> (EmpiricalConstraints, DebugInfo) {
        (self.empirical_constraints, self.debug_info)
    }

    pub fn process_trace(&mut self, trace: Trace, new_debug_info: DebugInfo) {
        // Compute empirical constraints from the current trace
        tracing::info!("      Detecting equivalence classes by block...");
        let equivalence_classes_by_block = self.generate_equivalence_classes_by_block(&trace);
        tracing::info!("      Detecting column ranges by PC...");
        let column_ranges_by_pc = self.detect_column_ranges_by_pc(trace);
        let new_empirical_constraints = EmpiricalConstraints {
            column_ranges_by_pc,
            equivalence_classes_by_block,
        };

        // Combine the new empirical constraints and debug info with the existing ones
        self.empirical_constraints
            .combine_with(new_empirical_constraints);
        self.debug_info.combine_with(new_debug_info);
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
    ) -> BTreeMap<u64, Partition<VariableId>> {
        tracing::info!("        Segmenting trace into blocks...");
        let blocks = self.get_blocks(trace);
        tracing::info!("        Finding equivalence classes...");
        blocks
            .into_par_iter()
            .map(|(block_id, block_instances)| {
                // Segment each block instance into equivalence classes
                let classes = block_instances
                    .into_iter()
                    .map(|block| self.block_equivalence_classes(block))
                    .collect::<Vec<_>>();

                // Intersect the equivalence classes across all instances of the block
                let intersected = Partition::intersect(&classes);

                (block_id, intersected)
            })
            .collect()
    }

    /// Segments a trace into basic blocks.
    /// Returns a mapping from block ID to all instances of that block in the trace.
    fn get_blocks<'a>(&self, trace: &'a Trace) -> BTreeMap<u64, Vec<Vec<&'a Row>>> {
        let mut block_rows = BTreeMap::new();
        let mut row_index = 0;
        let rows_by_time = trace.rows_by_time();

        while row_index < rows_by_time.len() {
            let first_row = rows_by_time[row_index];
            let block_id = first_row.pc as u64;

            if let Some(instruction_count) = self.instruction_counts.get(&block_id) {
                let block_row_slice = &rows_by_time[row_index..row_index + instruction_count];

                for (row1, row2) in block_row_slice.iter().tuple_windows() {
                    assert_eq!(row2.pc, row1.pc + 4);
                }

                block_rows
                    .entry(block_id)
                    .or_insert(Vec::new())
                    .push(block_row_slice.to_vec());
                row_index += instruction_count;
            } else {
                // Single instruction block, ignore.
                row_index += 1;
            }
        }

        block_rows
    }

    fn block_equivalence_classes(&self, block: Vec<&Row>) -> Partition<VariableId> {
        Partition {
            classes: block
                .into_iter()
                .enumerate()
                // Map each cell to a (value, (instruction_index, col_index)) pair
                .flat_map(|(instruction_index, row)| {
                    row.cells
                        .iter()
                        .enumerate()
                        .map(|(col_index, v)| (*v, (instruction_index, col_index)))
                        .collect::<Vec<_>>()
                })
                // Group by value
                .into_group_map()
                .values()
                // Convert to set
                .map(|v| EquivalenceClass {
                    ids: v.clone().into_iter().collect(),
                })
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;

    fn make_trace(rows_by_time_with_pc: Vec<(u32, Vec<u32>)>) -> Trace {
        Trace {
            rows: rows_by_time_with_pc
                .into_iter()
                .enumerate()
                .map(|(clk, (pc, cells))| Row {
                    cells,
                    pc,
                    timestamp: (0, clk as u32),
                })
                .collect(),
        }
    }

    fn assert_equivalence_classes_equal(
        actual: Partition<VariableId>,
        expected: Vec<Vec<(usize, usize)>>,
    ) {
        assert_eq!(actual.classes.len(), expected.len());
        let mut actual = actual.classes.into_iter();
        for expected_class in expected {
            let actual_class = actual.next().unwrap();
            let expected_class_set: BTreeSet<(usize, usize)> = expected_class.into_iter().collect();
            assert_eq!(actual_class.ids, expected_class_set);
        }
        assert!(actual.next().is_none());
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

        let (empirical_constraints, _debug_info) = detector.finalize();

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
            .unwrap()
            .clone();
        println!("Equivalence classes: {:?}", equivalence_classes);
        assert_equivalence_classes_equal(
            equivalence_classes,
            vec![
                // The result of the first instruction (col 0) is always equal to the
                // first operand of the second instruction (col 1)
                vec![(0, 0), (1, 1)],
            ],
        );
    }
}

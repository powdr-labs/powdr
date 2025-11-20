#![cfg_attr(feature = "tco", allow(internal_features))]
#![cfg_attr(feature = "tco", allow(incomplete_features))]
#![cfg_attr(feature = "tco", feature(explicit_tail_calls))]
#![cfg_attr(feature = "tco", feature(core_intrinsics))]

use eyre::Result;
use itertools::Itertools;
use openvm_circuit::arch::execution_mode::Segment;
use openvm_circuit::arch::{PreflightExecutionOutput, VirtualMachine, VmCircuitConfig, VmInstance};
use openvm_instructions::exe::VmExe;
use openvm_sdk::prover::vm::new_local_prover;
use openvm_sdk::{
    config::{AppConfig, DEFAULT_APP_LOG_BLOWUP},
    StdIn,
};
use openvm_stark_backend::p3_matrix::dense::DenseMatrix;
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPermutationEngine;
use openvm_stark_sdk::config::FriParameters;
use openvm_stark_sdk::openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::BasicBlock;
use powdr_autoprecompiles::ExecutionStats;
use std::collections::hash_map::Entry;
use std::collections::BTreeMap;
use std::{collections::HashMap, sync::Arc};

#[cfg(not(feature = "cuda"))]
use crate::PowdrSdkCpu;
use crate::{Instr, SpecializedConfig, SpecializedConfigCpuBuilder};
use tracing::info_span;

use std::collections::HashSet;
use std::hash::Hash;

// ChatGPT generated code
fn intersect_partitions<Id>(partitions: &[Vec<Vec<Id>>]) -> Vec<Vec<Id>>
where
    Id: Eq + Hash + Copy,
{
    if partitions.is_empty() {
        return Vec::new();
    }

    // 1) For each partition, build a map: Id -> class_index
    let mut maps: Vec<HashMap<Id, usize>> = Vec::with_capacity(partitions.len());
    for part in partitions {
        let mut m = HashMap::new();
        for (class_idx, class) in part.iter().enumerate() {
            for &id in class {
                m.insert(id, class_idx);
            }
        }
        maps.push(m);
    }

    // 2) Collect the universe of all Ids
    let mut universe: HashSet<Id> = HashSet::new();
    for part in partitions {
        for class in part {
            for &id in class {
                universe.insert(id);
            }
        }
    }

    // 3) For each Id, build its "signature" of class indices across all partitions
    //    and group by that signature.
    let mut grouped: HashMap<Vec<usize>, Vec<Id>> = HashMap::new();

    for &id in &universe {
        let mut signature = Vec::with_capacity(maps.len());
        for m in &maps {
            if let Some(class_idx) = m.get(&id) {
                signature.push(*class_idx);
            } else {
                continue;
            }
        }
        grouped.entry(signature).or_default().push(id);
    }

    // 4) Resulting equivalence classes are the grouped values
    grouped.into_values().collect()
}

pub fn execution_stats(
    exe: Arc<VmExe<BabyBear>>,
    vm_config: SpecializedConfig,
    inputs: StdIn,
    blocks: &[BasicBlock<Instr<BabyBear>>],
) -> Result<ExecutionStats, Box<dyn std::error::Error>> {
    // Set app configuration
    let app_fri_params =
        FriParameters::standard_with_100_bits_conjectured_security(DEFAULT_APP_LOG_BLOWUP);
    let app_config = AppConfig::new(app_fri_params, vm_config.clone());

    // Create the SDK
    #[cfg(feature = "cuda")]
    let sdk = PowdrSdkGpu::new(app_config).unwrap();
    #[cfg(not(feature = "cuda"))]
    let sdk = PowdrSdkCpu::new(app_config).unwrap();
    // Build owned vm instance, so we can mutate it later
    let vm_builder = sdk.app_vm_builder().clone();
    let vm_pk = sdk.app_pk().app_vm_pk.clone();
    let exe = sdk.convert_to_exe(exe.clone())?;
    let mut vm_instance: VmInstance<_, _> = new_local_prover(vm_builder, &vm_pk, exe.clone())?;

    vm_instance.reset_state(inputs.clone());
    let metered_ctx = vm_instance.vm.build_metered_ctx(&exe);
    let metered_interpreter = vm_instance.vm.metered_interpreter(vm_instance.exe())?;
    let (segments, _) = metered_interpreter.execute_metered(inputs.clone(), metered_ctx)?;
    let mut state = vm_instance.state_mut().take();

    // Get reusable inputs for `debug_proving_ctx`, the mock prover API from OVM.
    let vm: &mut VirtualMachine<BabyBearPermutationEngine<_>, SpecializedConfigCpuBuilder> =
        &mut vm_instance.vm;

    // Mapping (segment_idx, timestamp) -> Vec<u32>
    let mut rows_by_time = BTreeMap::new();

    let mut trace_values_by_pc = HashMap::new();
    let mut column_names_by_air_id = HashMap::new();
    let mut air_id_by_pc = HashMap::new();

    for (seg_idx, segment) in segments.into_iter().enumerate() {
        let _segment_span = info_span!("prove_segment", segment = seg_idx).entered();
        // We need a separate span so the metric label includes "segment" from _segment_span
        let _prove_span = info_span!("total_proof").entered();
        let Segment {
            instret_start,
            num_insns,
            trace_heights,
        } = segment;
        assert_eq!(state.as_ref().unwrap().instret(), instret_start);
        let from_state = Option::take(&mut state).unwrap();
        vm.transport_init_memory_to_device(&from_state.memory);
        let PreflightExecutionOutput {
            system_records,
            record_arenas,
            to_state,
        } = vm.execute_preflight(
            &mut vm_instance.interpreter,
            from_state,
            Some(num_insns),
            &trace_heights,
        )?;
        state = Some(to_state);

        // Generate proving context for each segment
        let ctx = vm.generate_proving_ctx(system_records, record_arenas)?;

        let global_airs = vm
            .config()
            .create_airs()
            .unwrap()
            .into_airs()
            .enumerate()
            .collect::<HashMap<_, _>>();

        for (air_id, proving_context) in &ctx.per_air {
            if !proving_context.cached_mains.is_empty() {
                // Not the case for instruction circuits
                continue;
            }
            let main: &Arc<DenseMatrix<BabyBear>> = proving_context.common_main.as_ref().unwrap();

            let air = &global_airs[air_id];
            let Some(column_names) = air.columns() else {
                continue;
            };
            assert_eq!(main.width, column_names.len());

            // This is the case for all instruction circuits
            let Some(pc_index) = column_names
                .iter()
                .position(|name| name == "from_state__pc")
            else {
                continue;
            };
            let ts_index = 1;

            for row in main.row_slices() {
                let row = row.iter().map(|v| v.as_canonical_u32()).collect::<Vec<_>>();
                let pc_value = row[pc_index];
                let ts_value = row[ts_index];
                rows_by_time.insert((seg_idx, ts_value), row.clone());

                if pc_value == 0 {
                    // Padding row!
                    continue;
                }

                if let Entry::Vacant(e) = trace_values_by_pc.entry(pc_value) {
                    // First time we see this PC, initialize the column -> values map
                    e.insert(vec![Vec::new(); row.len()]);
                    column_names_by_air_id.insert(*air_id, column_names.clone());
                    air_id_by_pc.insert(pc_value, *air_id);
                }
                let values_by_col = trace_values_by_pc.get_mut(&pc_value).unwrap();
                assert_eq!(
                    air_id_by_pc[&pc_value],
                    *air_id,
                    "Mismatched air IDs for PC {}: {} vs {}",
                    pc_value,
                    global_airs[&air_id_by_pc[&pc_value]].name(),
                    air.name()
                );
                assert_eq!(values_by_col.len(), row.len());

                for (col_idx, value) in row.iter().enumerate() {
                    values_by_col[col_idx].push(*value);
                }
            }
        }
    }

    // Block ID -> instruction count mapping
    let instruction_counts = blocks
        .iter()
        .map(|block| (block.start_pc, block.statements.len()))
        .collect::<HashMap<_, _>>();

    // Block ID -> Vec<Vec<Row>>
    let mut block_rows = BTreeMap::new();
    let mut i = 0;
    let rows_by_time = rows_by_time.values().collect::<Vec<_>>();
    while i < rows_by_time.len() {
        let row = &rows_by_time[i];
        let pc_value = row[0] as u64;

        if instruction_counts.contains_key(&pc_value) {
            let instruction_count = instruction_counts[&pc_value];
            let block_row_slice = &rows_by_time[i..i + instruction_count];
            block_rows
                .entry(pc_value)
                .or_insert(Vec::new())
                .push(block_row_slice.to_vec());
            i += instruction_count;
        } else {
            i += 1;
        }
    }

    // Block ID -> Vec<Vec<Vec<(instruction_index, col_index)>>>:
    // Indices: block ID, instance idx, equivalence class idx, cell
    let equivalence_classes = block_rows
        .into_iter()
        .map(|(block_id, blocks)| {
            let classes = blocks
                .into_iter()
                .map(|rows| {
                    let value_to_cells = rows
                        .into_iter()
                        .enumerate()
                        .flat_map(|(instruction_index, row)| {
                            row.iter()
                                .enumerate()
                                .map(|(col_index, v)| (*v, (instruction_index, col_index)))
                                .collect::<Vec<_>>()
                        })
                        .into_group_map();
                    value_to_cells.values().cloned().collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            (block_id, classes)
        })
        .collect::<HashMap<_, _>>();

    // Intersect equivalence classes across all instances
    let intersected_equivalence_classes = equivalence_classes
        .into_iter()
        .map(|(block_id, classes)| {
            let intersected = intersect_partitions(&classes);

            // Remove singleton classes
            let intersected = intersected
                .into_iter()
                .filter(|class| class.len() > 1)
                .collect::<Vec<_>>();

            (block_id, intersected)
        })
        .collect::<BTreeMap<_, _>>();

    // Map all column values to their range (1st and 99th percentile) for each pc
    let column_ranges_by_pc: HashMap<u32, Vec<(u32, u32)>> = trace_values_by_pc
        .into_iter()
        .map(|(pc, values_by_col)| {
            let column_ranges = values_by_col
                .into_iter()
                .map(|mut values| {
                    values.sort_unstable();
                    let len = values.len();
                    let p1_index = len / 100; // 1st percentile
                    let p99_index = len * 99 / 100; // 99th percentile
                    (values[p1_index], values[p99_index])
                })
                .collect();
            (pc, column_ranges)
        })
        .collect();

    let export = ExecutionStats {
        air_id_by_pc: air_id_by_pc.into_iter().collect(),
        column_names_by_air_id: column_names_by_air_id.into_iter().collect(),
        column_ranges_by_pc: column_ranges_by_pc.into_iter().collect(),
        equivalence_classes_by_block: intersected_equivalence_classes,
    };

    // Write to pgo_range_constraints.json
    let json = serde_json::to_string_pretty(&export).unwrap();
    std::fs::write("pgo_range_constraints.json", json).unwrap();

    Ok(export)
}

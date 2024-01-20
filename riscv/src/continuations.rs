use std::collections::{BTreeSet, HashMap};

use ast::{
    asm_analysis::{AnalysisASMFile, RegisterTy},
    parsed::asm::parse_absolute_path,
};
use number::FieldElement;
use pipeline::{Pipeline, Stage};
use riscv_executor::ExecutionTrace;

pub mod bootloader;
mod memory_merkle_tree;

use bootloader::{default_input, PAGE_SIZE_BYTES_LOG, PC_INDEX, REGISTER_NAMES};
use memory_merkle_tree::MerkleTree;

use crate::continuations::bootloader::{
    default_register_values, BOOTLOADER_INPUTS_PER_PAGE, BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES,
    DEFAULT_PC, MEMORY_HASH_START_INDEX, PAGE_INPUTS_OFFSET, WORDS_PER_PAGE,
};

fn transposed_trace<F: FieldElement>(trace: &ExecutionTrace) -> HashMap<String, Vec<F>> {
    let mut reg_values: HashMap<&str, Vec<F>> = HashMap::with_capacity(trace.reg_map.len());

    let mut rows = trace.replay();
    while let Some(row) = rows.next_row() {
        for (reg_name, &index) in trace.reg_map.iter() {
            reg_values
                .entry(reg_name)
                .or_default()
                .push(row[index as usize].0.into());
        }
    }

    reg_values
        .into_iter()
        .map(|(n, c)| (format!("main.{}", n), c))
        .collect()
}

fn render_hash<F: FieldElement>(hash: &[F]) -> String {
    hash.iter()
        .map(|&f| format!("{:016x}", f.to_arbitrary_integer()))
        .collect::<Vec<_>>()
        .join("")
}

pub fn rust_continuations<F: FieldElement, PipelineFactory, PipelineCallback, E>(
    pipeline_factory: PipelineFactory,
    pipeline_callback: PipelineCallback,
    bootloader_inputs: Vec<Vec<F>>,
) -> Result<(), E>
where
    PipelineFactory: Fn() -> Pipeline<F>,
    PipelineCallback: Fn(Pipeline<F>) -> Result<(), E>,
{
    let num_chunks = bootloader_inputs.len();

    log::info!("Advancing pipeline to PilWithEvaluatedFixedCols stage...");
    let pipeline = pipeline_factory();
    let pil_with_evaluated_fixed_cols = pipeline.pil_with_evaluated_fixed_cols().unwrap();

    // This returns the same pipeline as pipeline_factory() (with the same name, output dir, etc...)
    // but starting from the PilWithEvaluatedFixedCols stage. This is more efficient, because we can advance
    // to that stage once before we branch into different chunks.
    let optimized_pipeline_factory = || {
        pipeline_factory().from_pil_with_evaluated_fixed_cols(pil_with_evaluated_fixed_cols.clone())
    };

    bootloader_inputs
        .into_iter()
        .enumerate()
        .map(|(i, bootloader_inputs)| -> Result<(), E> {
            log::info!("\nRunning chunk {} / {}...", i + 1, num_chunks);
            let pipeline = optimized_pipeline_factory();
            let name = format!("{}_chunk_{}", pipeline.name(), i);
            let pipeline = pipeline.with_name(name);
            let pipeline = pipeline.add_external_witness_values(vec![(
                "main.bootloader_input_value".to_string(),
                bootloader_inputs,
            )]);
            pipeline_callback(pipeline)?;
            Ok(())
        })
        .collect::<Result<Vec<_>, E>>()?;
    Ok(())
}

fn sanity_check<T>(program: &AnalysisASMFile<T>) {
    let main_machine = program.items[&parse_absolute_path("::Main")]
        .try_to_machine()
        .unwrap();
    for expected_instruction in BOOTLOADER_SPECIFIC_INSTRUCTION_NAMES {
        if !main_machine
            .instructions
            .iter()
            .any(|i| i.name == expected_instruction)
        {
            log::error!(
                "Main machine is missing bootloader-specific instruction: {}. Did you set `with_bootloader` to true?",
                expected_instruction
            );
            panic!();
        }
    }

    // Check that the registers of the machine are as expected.
    let machine_registers = main_machine
        .registers
        .iter()
        .filter_map(|r| {
            ((r.ty == RegisterTy::Pc || r.ty == RegisterTy::Write) && r.name != "x0")
                .then_some(format!("main.{}", r.name))
        })
        .collect::<BTreeSet<_>>();
    let expected_registers = REGISTER_NAMES
        .iter()
        .map(|s| s.to_string())
        .collect::<BTreeSet<_>>();
    assert_eq!(machine_registers, expected_registers);
}

pub fn rust_continuations_dry_run<F: FieldElement>(mut pipeline: Pipeline<F>) -> Vec<Vec<F>> {
    log::info!("Initializing memory merkle tree...");
    let mut merkle_tree = MerkleTree::<F>::new();

    // All inputs for all chunks.
    let mut all_bootloader_inputs = vec![];

    // Initial register values for the current chunk.
    let mut register_values = default_register_values();

    pipeline.advance_to(Stage::AnalyzedAsm).unwrap();
    let program = pipeline.artifact().unwrap().to_analyzed_asm().unwrap();
    sanity_check(program);

    log::info!("Executing powdr-asm...");
    let (full_trace, memory_accesses) = {
        let trace = riscv_executor::execute_ast::<F>(
            program,
            pipeline.data_callback().unwrap(),
            // Run full trace without any accessed pages. This would actually violate the
            // constraints, but the executor does the right thing (read zero if the memory
            // cell has never been accessed). We can't pass the accessed pages here, because
            // we only know them after the full trace has been generated.
            &default_input(&[]),
            usize::MAX,
            riscv_executor::ExecMode::Trace,
        )
        .0;
        (transposed_trace::<F>(&trace), trace.mem_ops)
    };

    let full_trace_length = full_trace["main.pc"].len();
    log::info!("Total trace length: {}", full_trace_length);

    let (first_real_execution_row, _) = full_trace["main.pc"]
        .iter()
        .enumerate()
        .find(|(_, &pc)| pc == F::from(DEFAULT_PC))
        .unwrap();

    // The number of rows of the full trace that we consider proven.
    // Initialized with `first_real_execution_row`, because the bootloader
    // execution in the first chunk will be different from the full trace
    // execution (because of paged-in memeory).
    let mut proven_trace = first_real_execution_row;
    let mut chunk_index = 0;

    // Run for 2**degree - 2 steps, because the executor doesn't run the dispatcher,
    // which takes 2 rows.
    let degree = program
        .machines()
        .fold(None, |acc, (_, m)| acc.or(m.degree.clone()))
        .unwrap()
        .degree;
    let degree = F::from(degree).to_degree();
    let num_rows = degree as usize - 2;

    loop {
        log::info!("\nRunning chunk {}...", chunk_index);

        log::info!("Building bootloader inputs for chunk {}...", chunk_index);
        let mut accessed_pages = BTreeSet::new();
        let mut accessed_addresses = BTreeSet::new();
        let start_idx = memory_accesses
            .binary_search_by_key(&proven_trace, |a| a.row)
            .unwrap_or_else(|v| v);

        for access in &memory_accesses[start_idx..] {
            // proven_trace + num_rows is an upper bound for the last row index we'll reach in the next chunk.
            // In practice, we'll stop earlier, because the bootloader needs to run as well, but we don't know for
            // how long as that depends on the number of pages.
            if access.row >= proven_trace + num_rows {
                break;
            }
            accessed_addresses.insert(access.address);
            accessed_pages.insert(access.address >> PAGE_SIZE_BYTES_LOG);
        }
        log::info!(
            "{} unique memory accesses over {} accessed pages: {:?}",
            accessed_addresses.len(),
            accessed_pages.len(),
            accessed_pages
        );

        // Build the bootloader inputs for the current chunk.
        // Note that while we do know the accessed pages, we don't yet know the hashes
        // of those pages at the end of the execution, because that will depend on how
        // long the bootloader runs.
        // Similarly, we don't yet know the final register values.
        // So, we do a bit of a hack: For now, we'll just pretend that the state does not change, i.e.:
        // - The final register values are equal to the initial register values.
        // - The updated page hashes are equal to the current page hashes.
        // - The updated root hash is equal to the current root hash.
        // After simulating the chunk execution, we'll replace those values with the actual values.
        let mut bootloader_inputs = register_values.clone();
        bootloader_inputs.extend(register_values.clone());
        bootloader_inputs.extend(merkle_tree.root_hash());
        bootloader_inputs.extend(merkle_tree.root_hash());
        bootloader_inputs.push((accessed_pages.len() as u64).into());
        for &page_index in accessed_pages.iter() {
            bootloader_inputs.push(page_index.into());
            let (page, page_hash, proof) = merkle_tree.get(page_index as usize);
            bootloader_inputs.extend(page);
            bootloader_inputs.extend(page_hash);
            for sibling in proof {
                bootloader_inputs.extend(sibling);
            }
        }

        log::info!("Bootloader inputs length: {}", bootloader_inputs.len());

        log::info!("Simulating chunk execution...");
        let (chunk_trace, memory_snapshot_update) = {
            let (trace, memory_snapshot_update) = riscv_executor::execute_ast::<F>(
                program,
                pipeline.data_callback().unwrap(),
                &bootloader_inputs,
                num_rows,
                riscv_executor::ExecMode::Trace,
            );
            (transposed_trace(&trace), memory_snapshot_update)
        };
        let mut memory_updates_by_page =
            merkle_tree.organize_updates_by_page(memory_snapshot_update.into_iter());
        for (i, &page_index) in accessed_pages.iter().enumerate() {
            let page_index = page_index as usize;
            let (_, _, proof) = merkle_tree.get(page_index);

            // Replace the proof
            let proof_start_index =
                PAGE_INPUTS_OFFSET + BOOTLOADER_INPUTS_PER_PAGE * i + 1 + WORDS_PER_PAGE + 4;
            for (j, sibling) in proof.into_iter().enumerate() {
                bootloader_inputs[proof_start_index + j * 4..proof_start_index + j * 4 + 4]
                    .copy_from_slice(sibling);
            }

            // Update one child of the Merkle tree
            merkle_tree.update_page(
                page_index,
                memory_updates_by_page
                    .remove(&page_index)
                    .unwrap()
                    .into_iter(),
            );

            let (_, page_hash, proof) = merkle_tree.get(page_index);

            // Assert the proof hasn't changed (because we didn't update any page except the current).
            for (j, sibling) in proof.into_iter().enumerate() {
                assert_eq!(
                    &bootloader_inputs[proof_start_index + j * 4..proof_start_index + j * 4 + 4],
                    sibling
                );
            }

            // Replace the page hash
            let updated_page_hash_index =
                PAGE_INPUTS_OFFSET + BOOTLOADER_INPUTS_PER_PAGE * i + 1 + WORDS_PER_PAGE;
            bootloader_inputs[updated_page_hash_index..updated_page_hash_index + 4]
                .copy_from_slice(page_hash);
        }

        // Update initial register values for the next chunk.
        register_values = REGISTER_NAMES
            .iter()
            .map(|&r| *chunk_trace[r].last().unwrap())
            .collect();

        // Replace final register values of the current chunk
        bootloader_inputs[REGISTER_NAMES.len()..2 * REGISTER_NAMES.len()]
            .copy_from_slice(&register_values);

        // Replace the updated root hash
        let updated_root_hash_index = MEMORY_HASH_START_INDEX + 4;
        bootloader_inputs[updated_root_hash_index..updated_root_hash_index + 4]
            .copy_from_slice(merkle_tree.root_hash());

        log::info!(
            "Initial memory root hash: {}",
            render_hash(&bootloader_inputs[MEMORY_HASH_START_INDEX..MEMORY_HASH_START_INDEX + 4])
        );
        log::info!(
            "Final memory root hash: {}",
            render_hash(
                &bootloader_inputs[MEMORY_HASH_START_INDEX + 4..MEMORY_HASH_START_INDEX + 8]
            )
        );

        all_bootloader_inputs.push(bootloader_inputs.clone());

        log::info!("Chunk trace length: {}", chunk_trace["main.pc"].len());
        log::info!("Validating chunk...");
        let (start, _) = chunk_trace["main.pc"]
            .iter()
            .enumerate()
            .find(|(_, &pc)| pc == bootloader_inputs[PC_INDEX])
            .unwrap();
        log::info!("Bootloader used {} rows.", start);
        for i in 0..(chunk_trace["main.pc"].len() - start) {
            for &reg in REGISTER_NAMES.iter() {
                let chunk_i = i + start;
                let full_i = i + proven_trace;
                if chunk_trace[reg][chunk_i] != full_trace[reg][full_i] {
                    log::error!("The Chunk trace differs from the full trace!");
                    log::error!(
                        "Started comparing from row {start} in the chunk to row {proven_trace} in the full trace; the difference is at offset {i}."
                    );
                    log::error!(
                        "The PCs are {} and {}.",
                        chunk_trace["main.pc"][chunk_i],
                        full_trace["main.pc"][full_i]
                    );
                    log::error!(
                        "The first difference is in register {}: {} != {} ",
                        reg,
                        chunk_trace[reg][chunk_i],
                        full_trace[reg][full_i],
                    );
                    panic!();
                }
            }
        }

        if chunk_trace["main.pc"].len() < num_rows {
            log::info!("Done!");
            break;
        }

        // Minus one, because the last row will have to be repeated in the next chunk.
        let new_rows = num_rows - start - 1;
        proven_trace += new_rows;
        log::info!("Proved {} rows.", new_rows);

        chunk_index += 1;
    }
    all_bootloader_inputs
}

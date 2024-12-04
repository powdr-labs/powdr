use std::{
    collections::BTreeSet,
    fs::{create_dir_all, hard_link, remove_file},
};

use powdr_ast::{
    asm_analysis::{AnalysisASMFile, Machine},
    parsed::{asm::parse_absolute_path, Expression, Number, PilStatement},
};
use powdr_number::{FieldElement, KnownField, LargeInt};
use powdr_pipeline::Pipeline;
use powdr_riscv_executor::{get_main_machine, MemoryState, ProfilerOptions};

pub mod bootloader;
mod memory_merkle_tree;

use bootloader::split_fe;
use bootloader::{
    default_input, PAGE_SIZE_BYTES_LOG, PC_INDEX, REGISTER_MEMORY_NAMES, REGISTER_NAMES,
};
use memory_merkle_tree::MerkleTree;

use crate::continuations::bootloader::{
    default_register_values, shutdown_routine_upper_bound, BOOTLOADER_INPUTS_PER_PAGE, DEFAULT_PC,
    MEMORY_HASH_START_INDEX, PAGE_INPUTS_OFFSET, WORDS_PER_PAGE,
};

use crate::code_gen::Register;

fn render_memory_hash<F: FieldElement>(hash: &[F]) -> String {
    // Main memory values must fit into u32
    hash.iter()
        .map(|&f| {
            let v = f
                .to_integer()
                .try_into_u32()
                .expect("memory value larger than u32");
            format!("{v:08x}")
        })
        .collect::<Vec<_>>()
        .join("")
}

/// Calls the provided `pipeline_callback` for each chunk of the execution.
///
/// # Arguments
/// - `pipeline`: The pipeline that should be the starting point for all the chunks.
/// - `pipeline_callback`: A function that will be called for each chunk. It will be passed a prepared `pipeline`,
///    with all chunk-specific information set (witness, fixed cols, inputs, optimized pil)
/// - `bootloader_inputs`: The inputs to the bootloader and the index of the row at which the shutdown routine
///   is supposed to execute, for each chunk, as returned by `rust_continuations_dry_run`.
pub fn rust_continuations<F: FieldElement, PipelineCallback, E>(
    pipeline: &mut Pipeline<F>,
    pipeline_callback: PipelineCallback,
    dry_run_result: DryRunResult<F>,
) -> Result<(), E>
where
    PipelineCallback: Fn(&mut Pipeline<F>) -> Result<(), E>,
{
    let bootloader_inputs = dry_run_result.bootloader_inputs;
    let num_chunks = bootloader_inputs.len();

    log::info!("Computing fixed columns...");
    pipeline.compute_fixed_cols().unwrap();

    // Advance the pipeline to the optimized PIL stage, so that it doesn't need to be computed
    // in every chunk.
    pipeline.compute_optimized_pil().unwrap();

    bootloader_inputs
        .into_iter()
        .enumerate()
        .map(
            |(i, (bootloader_inputs, start_of_shutdown_routine))| -> Result<(), E> {
                log::info!("\nRunning chunk {} / {}...", i + 1, num_chunks);

                let parent_dir = pipeline.output_dir().clone();
                let force_overwrite = pipeline.is_force_overwrite();

                if let Some(parent_dir) = parent_dir.clone() {
                    let chunk_dir = parent_dir.join(format!("chunk_{i}"));
                    create_dir_all(&chunk_dir).unwrap();

                    // Hardlink constants.bin so that chunk dir will be self sufficient
                    let link_to_consts = chunk_dir.join("constants.bin");
                    if force_overwrite {
                        // Remove the file if it already exists
                        let _ = remove_file(&link_to_consts);
                    }
                    hard_link(parent_dir.join("constants.bin"), link_to_consts).unwrap();

                    // The output directory is set here to output witness and proof artifacts
                    // inside the chunk directory.
                    // TODO This is hacky and should be improved.
                    pipeline.set_output(chunk_dir, force_overwrite)
                }

                // get the length of the main machine
                // quite hacky, is there a better way?
                let length = pipeline
                    .optimized_pil()
                    .unwrap()
                    .definitions
                    .iter()
                    .find_map(|(name, (s, _))| match (name.starts_with("main::"), s) {
                        (true, s) => s.degree.map(|d| d.max),
                        _ => None,
                    })
                    .unwrap();

                pipeline.rollback_from_witness();

                // The `jump_to_shutdown_routine` column indicates when the execution should jump to the shutdown routine.
                // In that row, the normal PC update is ignored and the PC is set to the address of the shutdown routine.
                // In other words, it should be a one-hot encoding of `start_of_shutdown_routine`.
                let jump_to_shutdown_routine = (0..length)
                    .map(|i| (i == start_of_shutdown_routine - 1).into())
                    .collect();
                pipeline.add_external_witness_values_mut(vec![
                    (
                        "main_bootloader_inputs::value".to_string(),
                        bootloader_inputs,
                    ),
                    (
                        "main::jump_to_shutdown_routine".to_string(),
                        jump_to_shutdown_routine,
                    ),
                ]);
                pipeline_callback(pipeline)?;

                if let Some(original_dir) = parent_dir {
                    pipeline.set_output(original_dir, force_overwrite);
                }

                Ok(())
            },
        )
        .collect::<Result<Vec<_>, E>>()?;
    Ok(())
}

fn sanity_check(main_machine: &Machine, field: KnownField) {
    for expected_instruction in bootloader::bootloader_specific_instruction_names(field) {
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
}

pub fn load_initial_memory(program: &AnalysisASMFile) -> MemoryState {
    let machine = get_main_machine(program);
    let Some(expr) = machine.pil.iter().find_map(|v| match v {
        PilStatement::LetStatement(_, n, _, expr) if n == "initial_memory" => expr.as_ref(),
        _ => None,
    }) else {
        log::warn!("No initial_memory variable found in the machine. Assuming zeroed memory.");
        return MemoryState::default();
    };

    let Expression::ArrayLiteral(_, array) = expr else {
        panic!("initial_memory is not an array literal");
    };

    array
        .items
        .iter()
        .map(|entry| {
            let Expression::Tuple(_, tuple) = entry else {
                panic!("initial_memory entry is not a tuple");
            };
            assert_eq!(tuple.len(), 2);
            let Expression::Number(
                _,
                Number {
                    value: key,
                    type_: None,
                },
            ) = &tuple[0]
            else {
                panic!("initial_memory entry key is not a number");
            };
            let Expression::Number(_, Number { value, type_: None }) = &tuple[1] else {
                panic!("initial_memory entry value is not a number");
            };

            (key.try_into().unwrap(), value.try_into().unwrap())
        })
        .collect()
}

pub struct DryRunResult<F: FieldElement> {
    pub bootloader_inputs: Vec<(Vec<F>, u64)>,
    // full execution trace length (i.e., length of main::pc)
    pub trace_len: usize,
}

/// Runs the entire execution using the RISC-V executor. For each chunk, it collects:
/// - The inputs to the bootloader, needed to restore the correct state.
/// - The number of rows after which the prover should jump to the shutdown routine.
pub fn rust_continuations_dry_run<F: FieldElement>(
    pipeline: &mut Pipeline<F>,
    profiler_opt: Option<ProfilerOptions>,
) -> DryRunResult<F> {
    let field = F::known_field().unwrap();

    // All inputs for all chunks.
    let mut bootloader_inputs_and_num_rows = vec![];

    // Initial register values for the current chunk.
    let mut register_values = default_register_values();

    let asm = pipeline.compute_analyzed_asm().unwrap().clone();
    let pil = pipeline.compute_optimized_pil().unwrap();
    let fixed = pipeline.compute_fixed_cols().unwrap();
    let main_machine = asm.get_machine(&parse_absolute_path("::Main")).unwrap();
    sanity_check(main_machine, field);

    log::info!("Initializing memory merkle tree...");

    // Get initial memory contents from the special variable "initial_memory".
    // In the first full run, we use it as the memory contents of the executor;
    // on the independent chunk runs, the executor uses zeroed initial memory,
    // and the pages are loaded via the bootloader.
    let initial_memory = load_initial_memory(&asm);

    let mut merkle_tree = MerkleTree::<F>::new();
    merkle_tree.update(initial_memory.iter().map(|(k, v)| (*k, *v)));

    // TODO: commit to the merkle_tree root in the verifier.

    log::info!("Initial execution...");
    let full_exec = powdr_riscv_executor::execute::<F>(
        &asm,
        &pil,
        fixed.clone(),
        initial_memory,
        pipeline.data_callback_mut().unwrap(),
        // Run full trace without any accessed pages. This would actually violate the
        // constraints, but the executor does the right thing (read zero if the memory
        // cell has never been accessed). We can't pass the accessed pages here, because
        // we only know them after the full trace has been generated.
        &default_input(&[]),
        None,
        profiler_opt,
    );

    let full_trace_length = full_exec.trace_len;
    log::info!("Total trace length: {}", full_trace_length);

    let (first_real_execution_row, _) = full_exec.trace["main::pc"]
        .iter()
        .enumerate()
        .find(|(_, &pc)| pc == DEFAULT_PC.into())
        .unwrap();

    // The number of rows of the full trace that we consider proven.
    // Initialized with `first_real_execution_row`, because the bootloader
    // execution in the first chunk will be different from the full trace
    // execution (because of paged-in memory).
    let mut proven_trace = first_real_execution_row;
    let mut chunk_index = 0;

    let max_degree_expr = main_machine.degree.max.as_ref();

    let length: usize = match max_degree_expr {
        Some(Expression::Number(_, n)) => n.value.clone().try_into().unwrap(),
        None => unimplemented!("Continuations rely on `Main` defining a max degree"),
        Some(e) => {
            unimplemented!("Continuations rely on `Main` not using a complex expression as its max degree, found {e}")
        }
    };

    loop {
        log::info!("\nRunning chunk {} for {} steps...", chunk_index, length);

        log::info!("Building bootloader inputs for chunk {}...", chunk_index);
        let mut accessed_pages = BTreeSet::new();
        let mut accessed_addresses = BTreeSet::new();

        let start_idx = full_exec
            .memory_accesses
            .binary_search_by_key(&proven_trace, |a| a.row)
            .unwrap_or_else(|v| v);

        for access in &full_exec.memory_accesses[start_idx..] {
            // proven_trace + length is an upper bound for the last row index we'll reach in the next chunk.
            // In practice, we'll stop earlier, because the bootloader & shutdown routine need to run as well,
            // but we don't know for how long as that depends on the number of pages.
            if access.row >= proven_trace + length {
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

        let shutdown_routine_rows = shutdown_routine_upper_bound(accessed_pages.len());
        log::info!(
            "Estimating the shutdown routine to use {} rows.",
            shutdown_routine_rows
        );
        let num_rows = length - shutdown_routine_rows;

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
        let mut bootloader_inputs = bootloader::create_input(
            register_values,
            &merkle_tree,
            accessed_pages.iter().cloned(),
        );

        log::info!("Bootloader inputs length: {}", bootloader_inputs.len());
        log::info!(
            "Initial memory root hash: {}",
            render_memory_hash(
                &bootloader_inputs[MEMORY_HASH_START_INDEX..MEMORY_HASH_START_INDEX + 8]
            )
        );

        log::info!("Simulating chunk execution...");
        let chunk_exec = powdr_riscv_executor::execute::<F>(
            &asm,
            &pil,
            fixed.clone(),
            MemoryState::new(),
            pipeline.data_callback_mut().unwrap(),
            &bootloader_inputs,
            Some(num_rows),
            // profiling was done when full trace was generated
            None,
        );

        let mut memory_updates_by_page =
            merkle_tree.organize_updates_by_page(chunk_exec.memory.into_iter());
        for (i, &page_index) in accessed_pages.iter().enumerate() {
            let page_index = page_index as usize;
            let (_, _, proof) = merkle_tree.get(page_index);

            // Replace the proof
            let proof_start_index =
                PAGE_INPUTS_OFFSET + BOOTLOADER_INPUTS_PER_PAGE * i + 1 + WORDS_PER_PAGE + 8;
            for (j, sibling) in proof.into_iter().enumerate() {
                bootloader_inputs[proof_start_index + j * 8..proof_start_index + j * 8 + 8]
                    .copy_from_slice(
                        &sibling
                            .iter()
                            .flat_map(|e| split_fe(*e))
                            .collect::<Vec<_>>(),
                    );
            }

            // Update one child of the Merkle tree
            merkle_tree.update_page(
                page_index,
                &memory_updates_by_page
                    .remove(&page_index)
                    .unwrap_or_default(),
            );

            let (_, page_hash, proof) = merkle_tree.get(page_index);

            // Assert the proof hasn't changed (because we didn't update any page except the current).
            for (j, sibling) in proof.into_iter().enumerate() {
                assert_eq!(
                    &bootloader_inputs[proof_start_index + j * 8..proof_start_index + j * 8 + 8],
                    sibling
                        .iter()
                        .flat_map(|e| split_fe(*e))
                        .collect::<Vec<_>>()
                );
            }

            // Replace the page hash
            let updated_page_hash_index =
                PAGE_INPUTS_OFFSET + BOOTLOADER_INPUTS_PER_PAGE * i + 1 + WORDS_PER_PAGE;
            bootloader_inputs[updated_page_hash_index..updated_page_hash_index + 8]
                .copy_from_slice(
                    &page_hash
                        .iter()
                        .flat_map(|e| split_fe(*e))
                        .collect::<Vec<_>>(),
                );
        }

        // Go over all memory registers
        register_values = REGISTER_MEMORY_NAMES
            .iter()
            .map(|reg| {
                let reg = reg.strip_prefix("main::").unwrap();
                let id = Register::from(reg).addr();
                *chunk_exec
                    .register_memory
                    .get(&(id as u32))
                    .unwrap_or(&0.into())
            })
            .collect::<Vec<_>>();

        // Go over all machine registers
        for reg in REGISTER_NAMES {
            register_values.push(*chunk_exec.trace[reg].last().unwrap());
        }

        // Replace final register values of the current chunk
        bootloader_inputs[(REGISTER_MEMORY_NAMES.len() + REGISTER_NAMES.len())
            ..2 * (REGISTER_MEMORY_NAMES.len() + REGISTER_NAMES.len())]
            .copy_from_slice(&register_values);

        // Replace the updated root hash
        let updated_root_hash_index = MEMORY_HASH_START_INDEX + 8;
        bootloader_inputs[updated_root_hash_index..updated_root_hash_index + 8].copy_from_slice(
            &merkle_tree
                .root_hash()
                .iter()
                .flat_map(|e| split_fe(*e))
                .collect::<Vec<_>>(),
        );

        log::info!(
            "Initial memory root hash: {}",
            render_memory_hash(
                &bootloader_inputs[MEMORY_HASH_START_INDEX..MEMORY_HASH_START_INDEX + 8]
            )
        );
        log::info!(
            "Final memory root hash: {}",
            render_memory_hash(
                &bootloader_inputs[MEMORY_HASH_START_INDEX + 8..MEMORY_HASH_START_INDEX + 16]
            )
        );

        let actual_num_rows = chunk_exec.trace_len;
        let bootloader_pc = bootloader_inputs[PC_INDEX];
        bootloader_inputs_and_num_rows.push((bootloader_inputs, actual_num_rows as u64));

        log::info!("Chunk trace length: {}", chunk_exec.trace_len);
        log::info!("Validating chunk...");
        log::info!("Looking for pc = {}...", bootloader_pc);
        let (start, _) = chunk_exec.trace["main::pc"]
            .iter()
            .enumerate()
            .find(|(_, &pc)| pc == bootloader_pc)
            .unwrap();
        log::info!("Bootloader used {} rows.", start);
        log::info!(
            "  => {} / {} ({}%) of rows are used for the actual computation!",
            length - start - shutdown_routine_rows,
            length,
            (length - start - shutdown_routine_rows) * 100 / length
        );
        for i in 0..(chunk_exec.trace_len - start) {
            for &reg in ["main::pc", "main::query_arg_1", "main::query_arg_2"].iter() {
                let chunk_i = i + start;
                let full_i = i + proven_trace;
                if chunk_exec.trace[reg][chunk_i] != full_exec.trace[reg][full_i] {
                    log::error!("The Chunk trace differs from the full trace!");
                    log::error!(
                        "Started comparing from row {start} in the chunk to row {proven_trace} in the full trace; the difference is at offset {i}."
                    );
                    log::error!(
                        "The PCs are {} and {}.",
                        chunk_exec.trace["main::pc"][chunk_i],
                        full_exec.trace["main::pc"][full_i]
                    );
                    log::error!(
                        "The first difference is in register {}: {} != {} ",
                        reg,
                        chunk_exec.trace[reg][chunk_i],
                        full_exec.trace[reg][full_i],
                    );
                    panic!();
                }
            }
        }

        if chunk_exec.trace_len < num_rows {
            log::info!("Done!");
            break;
        }
        assert_eq!(chunk_exec.trace_len, num_rows);

        // Minus one, because the last row will have to be repeated in the next chunk.
        let new_rows = num_rows - start - 1;
        proven_trace += new_rows;
        log::info!("Proved {} rows.", new_rows);

        chunk_index += 1;
    }
    DryRunResult {
        bootloader_inputs: bootloader_inputs_and_num_rows,
        trace_len: full_trace_length,
    }
}

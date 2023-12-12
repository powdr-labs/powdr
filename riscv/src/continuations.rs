use std::collections::{BTreeSet, HashMap};

use compiler::{access_element, parse_query, pipeline::Pipeline};
use executor::witgen::QueryCallback;
use itertools::Itertools;
use number::FieldElement;
use riscv_executor::ExecutionTrace;

use crate::bootloader::{
    default_input, BYTES_PER_WORD, PAGE_SIZE_BYTES_LOG, PC_INDEX, REGISTER_NAMES,
};

fn make_query_callback<T: FieldElement>(bootloader_inputs: Vec<T>) -> impl QueryCallback<T> {
    move |query: &str| -> Result<Option<T>, String> {
        match &parse_query(query)?[..] {
            ["\"bootloader_input\"", index] => {
                access_element("bootloader input", &bootloader_inputs, index)
            }
            k => Err(format!("Unsupported query: {}", k.iter().format(", "))),
        }
    }
}

fn add_bootloader_inputs<F: FieldElement>(
    pipeline: Pipeline<F>,
    bootload_inputs: Vec<F>,
) -> Pipeline<F> {
    pipeline.add_query_callback(Box::new(make_query_callback(bootload_inputs)))
}

fn transposed_trace<F: FieldElement>(trace: &ExecutionTrace) -> HashMap<String, Vec<F>> {
    let mut reg_values: HashMap<&str, Vec<F>> = HashMap::with_capacity(trace.reg_map.len());

    for row in trace.regs_rows() {
        for (reg_name, &index) in trace.reg_map.iter() {
            reg_values
                .entry(reg_name)
                .or_default()
                .push(row[index].0.into());
        }
    }

    reg_values
        .into_iter()
        .map(|(n, c)| (format!("main.{}", n), c))
        .collect()
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

    bootloader_inputs
        .into_iter()
        .enumerate()
        .map(|(i, bootloader_inputs)| -> Result<(), E> {
            log::info!("Running chunk {} / {}...", i + 1, num_chunks);
            // TODO(#840): If Pipeline implemented Clone, we could advance it once to
            // the OptimizedPil stage and clone it here instead of creating and
            // running a fresh pipeline for each chunk.
            let pipeline = pipeline_factory();
            let name = format!("{}_chunk_{}", pipeline.name(), i);
            let pipeline = pipeline.with_name(name);
            let pipeline = add_bootloader_inputs(pipeline, bootloader_inputs);
            pipeline_callback(pipeline)?;
            Ok(())
        })
        .collect::<Result<Vec<_>, E>>()?;
    Ok(())
}

pub fn rust_continuations_dry_run<F: FieldElement>(
    pipeline: Pipeline<F>,
    inputs: Vec<F>,
) -> Vec<Vec<F>> {
    // All inputs for all chunks.
    let mut all_bootloader_inputs = vec![];

    // Bootloader inputs for the current chunk.
    let mut bootloader_inputs = default_input();

    let program = pipeline.analyzed_asm().unwrap();

    let inputs: HashMap<F, Vec<F>> = vec![(F::from(0), inputs)].into_iter().collect();

    log::info!("Executing powdr-asm...");
    let (full_trace, memory_accesses) = {
        let trace = riscv_executor::execute_ast::<F>(
            &program,
            &inputs,
            &bootloader_inputs,
            usize::MAX,
            riscv_executor::ExecMode::Trace,
        )
        .0;
        (transposed_trace::<F>(&trace), trace.mem)
    };

    let full_trace_length = full_trace["main.pc"].len();
    log::info!("Total trace length: {}", full_trace_length);

    let mut proven_trace = 0;
    let mut chunk_index = 0;
    let mut memory_snapshot = HashMap::new();

    loop {
        all_bootloader_inputs.push(bootloader_inputs.clone());

        log::info!("\nRunning chunk {}...", chunk_index);
        // Run for 2**degree - 2 steps, because the executor doesn't run the dispatcher,
        // which takes 2 rows.
        let degree = program
            .machines
            .iter()
            .fold(None, |acc, (_, m)| acc.or(m.degree.clone()))
            .unwrap()
            .degree;
        let degree = F::from(degree).to_degree();
        let num_rows = degree as usize - 2;
        let (chunk_trace, memory_snapshot_update) = {
            let (trace, memory_snapshot_update) = riscv_executor::execute_ast::<F>(
                &program,
                &inputs,
                &bootloader_inputs,
                num_rows,
                riscv_executor::ExecMode::Trace,
            );
            (transposed_trace(&trace), memory_snapshot_update)
        };
        log::info!("{} memory slots updated.", memory_snapshot_update.len());
        memory_snapshot.extend(memory_snapshot_update);
        log::info!("Chunk trace length: {}", chunk_trace["main.pc"].len());

        log::info!("Validating chunk...");
        let (start, _) = chunk_trace["main.pc"]
            .iter()
            .enumerate()
            .find(|(_, &pc)| pc == bootloader_inputs[PC_INDEX])
            .unwrap();
        let full_trace_start = match chunk_index {
            // The bootloader execution in the first chunk is part of the full trace.
            0 => start,
            // Any other chunk starts at where we left off in the full trace.
            _ => proven_trace - 1,
        };
        log::info!("Bootloader used {} rows.", start);
        for i in 0..(chunk_trace["main.pc"].len() - start) {
            for &reg in REGISTER_NAMES.iter() {
                let chunk_i = i + start;
                let full_i = i + full_trace_start;
                if chunk_trace[reg][chunk_i] != full_trace[reg][full_i] {
                    log::error!("The Chunk trace differs from the full trace!");
                    log::error!(
                        "Started comparing from row {start} in the chunk to row {full_trace_start} in the full trace; the difference is at offset {i}."
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

        let new_rows = match chunk_index {
            0 => num_rows,
            // Minus 1 because the first row was proven already.
            _ => num_rows - start - 1,
        };
        proven_trace += new_rows;
        log::info!("Proved {} rows.", new_rows);

        log::info!("Building inputs for chunk {}...", chunk_index + 1);
        let mut accessed_pages = BTreeSet::new();
        let start_idx = memory_accesses
            .binary_search_by_key(&proven_trace, |a| a.idx)
            .unwrap_or_else(|v| v);

        for access in &memory_accesses[start_idx..] {
            // proven_trace + num_rows is an upper bound for the last row index we'll reach in the next chunk.
            // In practice, we'll stop earlier, because the bootloader needs to run as well, but we don't know for
            // how long as that depends on the number of pages.
            if access.idx >= proven_trace + num_rows {
                break;
            }
            accessed_pages.insert(access.address >> PAGE_SIZE_BYTES_LOG);
        }
        log::info!(
            "{} accessed pages: {:?}",
            accessed_pages.len(),
            accessed_pages
        );

        bootloader_inputs = vec![];
        for &reg in REGISTER_NAMES.iter() {
            bootloader_inputs.push(*chunk_trace[reg].last().unwrap());
        }
        bootloader_inputs.push((accessed_pages.len() as u64).into());
        for &page in accessed_pages.iter() {
            let start_addr = page << PAGE_SIZE_BYTES_LOG;
            bootloader_inputs.push(page.into());
            let words_per_page = (1 << (PAGE_SIZE_BYTES_LOG)) / BYTES_PER_WORD;
            for i in 0..words_per_page {
                let addr = start_addr + (i * BYTES_PER_WORD) as u32;
                bootloader_inputs.push((*memory_snapshot.get(&addr).unwrap_or(&0)).into());
            }
        }

        log::info!("Inputs length: {}", bootloader_inputs.len());

        chunk_index += 1;
    }
    all_bootloader_inputs
}

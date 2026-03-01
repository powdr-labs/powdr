use std::sync::Arc;

use itertools::Itertools;
use openvm_instructions::VmOpcode;
use powdr_autoprecompiles::{
    adapter::{ApcWithStats, PgoAdapter},
    empirical_constraints::EmpiricalConstraints,
    PowdrConfig, VmConfig,
};
use powdr_openvm_bus_interaction_handler::OpenVmBusInteractionHandler;
use powdr_openvm_common::{
    isa::OpenVmISA, opcode::PowdrOpcode, vm::PowdrPrecompile, BabyBearOpenVmApcAdapter,
    SpecializedConfig, POWDR_OPCODE,
};

use crate::{CompiledProgram, OriginalCompiledProgram, RiscvISA};

// TODO: make generic on ISA
pub fn customize<'a, P: PgoAdapter<Adapter = BabyBearOpenVmApcAdapter<'a, RiscvISA>>>(
    original_program: OriginalCompiledProgram<RiscvISA>,
    config: PowdrConfig,
    pgo: P,
    empirical_constraints: EmpiricalConstraints,
) -> CompiledProgram<RiscvISA> {
    let original_config = original_program.vm_config.clone();
    let airs = original_config.airs(config.degree_bound).expect("Failed to convert the AIR of an OpenVM instruction, even after filtering by the blacklist!");
    let bus_map = original_config.bus_map();

    let vm_config = VmConfig {
        instruction_handler: &airs,
        bus_interaction_handler: OpenVmBusInteractionHandler::new(bus_map.clone()),
        bus_map: bus_map.clone(),
    };

    let blocks = original_program.collect_basic_blocks();
    let exe = original_program.exe;
    let debug_info = original_program.elf.debug_info();
    tracing::info!(
        "Got {} basic blocks from `collect_basic_blocks`",
        blocks.len()
    );
    if tracing::enabled!(tracing::Level::DEBUG) {
        tracing::debug!("Basic blocks sorted by execution count (top 10):");
        for (count, block) in blocks
            .iter()
            .filter_map(|block| Some((pgo.pc_execution_count(block.start_pc)?, block)))
            .sorted_by_key(|(count, _)| *count)
            .rev()
            .take(10)
        {
            let name = debug_info
                .symbols
                .try_get_one_or_preceding(block.start_pc)
                .map(|(symbol, offset)| format!("{} + {offset}", rustc_demangle::demangle(symbol)))
                .unwrap_or_default();
            tracing::debug!("Basic block (executed {count} times), {name}:\n{block}",);
        }
    }

    let labels = debug_info
        .symbols
        .table()
        .iter()
        .map(|(addr, names)| {
            (
                *addr as u64,
                names
                    .iter()
                    .map(|name| rustc_demangle::demangle(name).to_string())
                    .collect(),
            )
        })
        .collect();

    let start = std::time::Instant::now();
    let apcs = pgo.filter_blocks_and_create_apcs_with_pgo(
        blocks,
        &config,
        vm_config,
        labels,
        empirical_constraints.apply_pc_threshold(),
    );
    metrics::gauge!("total_apc_gen_time_ms").set(start.elapsed().as_millis() as f64);

    let pc_base = exe.program.pc_base;
    let pc_step = RiscvISA::DEFAULT_PC_STEP;
    // We need to clone the program because we need to modify it to add the apc instructions.
    let mut exe = (*exe).clone();
    let program = &mut exe.program;

    tracing::info!("Adjust the program with the autoprecompiles");

    let extensions = apcs
        .into_iter()
        .map(ApcWithStats::into_parts)
        .enumerate()
        .map(|(i, (apc, apc_stats, _))| {
            let opcode = POWDR_OPCODE + i;
            let start_pc = apc
                .block
                .try_as_basic_block()
                .expect("superblocks unsupported")
                .start_pc;
            let start_index = ((start_pc - pc_base as u64) / pc_step as u64)
                .try_into()
                .unwrap();

            // We encode in the program that the prover should execute the apc instruction instead of the original software version.
            // This is only for witgen: the program in the program chip is left unchanged.
            program.add_apc_instruction_at_pc_index(start_index, VmOpcode::from_usize(opcode));

            PowdrPrecompile::new(
                format!("PowdrAutoprecompile_{}", start_pc),
                PowdrOpcode {
                    class_offset: opcode,
                },
                apc,
                apc_stats,
            )
        })
        .collect();

    CompiledProgram {
        exe: Arc::new(exe),
        vm_config: SpecializedConfig::new(original_config, extensions, config.degree_bound),
    }
}

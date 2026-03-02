#[cfg(test)]
use openvm_circuit::arch::{VmBuilder, VmCircuitConfig};
#[cfg(test)]
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
#[cfg(test)]
use powdr_openvm_common::{
    extraction_utils::{get_air_metrics, AirMetrics, AirWidthsDiff},
    program::CompiledProgram,
};

#[cfg(test)]
use crate::{RiscvISA, SpecializedConfigCpuBuilder};

// TODO: This cannot be generic over ISA because SpecializedConfigCpuBuilder is not
#[cfg(test)]
// Return a tuple of (powdr AirMetrics, non-powdr AirMetrics)
pub fn air_metrics(
    program: CompiledProgram<RiscvISA>,
    max_degree: usize,
) -> (Vec<(AirMetrics, Option<AirWidthsDiff>)>, Vec<AirMetrics>) {
    let air_inventory = program.vm_config.create_airs().unwrap();

    let chip_complex =
        <SpecializedConfigCpuBuilder as VmBuilder<BabyBearPoseidon2Engine>>::create_chip_complex(
            &SpecializedConfigCpuBuilder,
            &program.vm_config,
            air_inventory,
        )
        .unwrap();

    let inventory = chip_complex.inventory;

    // Order of precompile is the same as that of Powdr executors in chip inventory
    let mut apc_stats = program
        .vm_config
        .powdr
        .precompiles
        .iter()
        .map(|precompile| precompile.apc_stats.clone());

    inventory.airs().ext_airs().iter().fold(
        (Vec::new(), Vec::new()),
        |(mut powdr_air_metrics, mut non_powdr_air_metrics), air| {
            let name = air.name();
            // We actually give name "powdr_air_for_opcode_<opcode>" to the AIRs,
            // but OpenVM uses the actual Rust type (PowdrAir) as the name in this method.
            // TODO this is hacky but not sure how to do it better rn.
            if name.starts_with("PowdrAir") {
                powdr_air_metrics.push((
                    get_air_metrics(air.clone(), max_degree),
                    Some(apc_stats.next().unwrap().widths),
                ));
            } else {
                non_powdr_air_metrics.push(get_air_metrics(air.clone(), max_degree));
            }

            (powdr_air_metrics, non_powdr_air_metrics)
        },
    )
}

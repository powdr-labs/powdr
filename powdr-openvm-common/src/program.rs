use openvm_circuit::arch::{VmBuilder, VmCircuitConfig};
use openvm_instructions::program::Program as OpenVmProgram;
use openvm_stark_backend::p3_field::PrimeField32;
use powdr_autoprecompiles::blocks::Program;

use crate::extraction_utils::OriginalVmConfig;
use crate::extraction_utils::{get_air_metrics, AirMetrics, AirWidthsDiff};
use crate::BabyBearPoseidon2Engine;
use crate::{instruction::Instr, isa::OpenVmISA};
use crate::{BabyBearOpenVmApcAdapter, SpecializedConfig, SpecializedConfigCpuBuilder};

/// A newtype wrapper around `OpenVmProgram` to implement the `Program` trait.
/// This is necessary because we cannot implement a foreign trait for a foreign type.
pub struct Prog<'a, F>(&'a OpenVmProgram<F>);

impl<'a, F> From<&'a OpenVmProgram<F>> for Prog<'a, F> {
    fn from(program: &'a OpenVmProgram<F>) -> Self {
        Prog(program)
    }
}

impl<'a, F: PrimeField32, ISA: OpenVmISA> Program<Instr<F, ISA>> for Prog<'a, F> {
    fn base_pc(&self) -> u64 {
        self.0.pc_base as u64
    }

    fn instructions(&self) -> Box<dyn Iterator<Item = Instr<F, ISA>> + '_> {
        Box::new(
            self.0
                .instructions_and_debug_infos
                .iter()
                .filter_map(|x| x.as_ref().map(|i| Instr::from(i.0.clone()))),
        )
    }

    fn length(&self) -> u32 {
        self.0.instructions_and_debug_infos.len() as u32
    }
}

use std::{collections::BTreeSet, sync::Arc};

use openvm_instructions::exe::VmExe;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::blocks::{collect_basic_blocks, BasicBlock};
use powdr_autoprecompiles::DegreeBound;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
#[serde(bound = "")]
pub struct CompiledProgram<ISA: OpenVmISA> {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: SpecializedConfig<ISA>,
}

// the original openvm program and config without powdr extension, along with the elf
pub struct OriginalCompiledProgram<'a, ISA: OpenVmISA> {
    pub exe: Arc<VmExe<BabyBear>>,
    pub vm_config: OriginalVmConfig<ISA>,
    pub elf: ISA::Program<'a>,
}

impl<'a, ISA: OpenVmISA> OriginalCompiledProgram<'a, ISA> {
    /// Segments the program into basic blocks
    pub fn collect_basic_blocks(&self) -> Vec<BasicBlock<Instr<BabyBear, ISA>>> {
        let jumpdest_set = ISA::get_labels_debug(&self.elf);

        let program = Prog::from(&self.exe.program);

        // Convert the jump destinations to u64 for compatibility with the `collect_basic_blocks` function.
        let jumpdest_set = jumpdest_set.keys().copied().collect::<BTreeSet<_>>();

        collect_basic_blocks::<BabyBearOpenVmApcAdapter<ISA>>(&program, &jumpdest_set)
    }

    /// Converts to a `CompiledProgram` with the original vm config (without autoprecompiles).
    pub fn compiled_program(&self, degree_bound: DegreeBound) -> CompiledProgram<ISA> {
        CompiledProgram {
            exe: self.exe.clone(),
            vm_config: SpecializedConfig::new(self.vm_config.clone(), Vec::new(), degree_bound),
        }
    }
}

impl<ISA: OpenVmISA> CompiledProgram<ISA> {
    // Return a tuple of (powdr AirMetrics, non-powdr AirMetrics)
    pub fn air_metrics(
        &self,
        max_degree: usize,
    ) -> (Vec<(AirMetrics, Option<AirWidthsDiff>)>, Vec<AirMetrics>) {
        let air_inventory = self.vm_config.create_airs().unwrap();

        let chip_complex = <SpecializedConfigCpuBuilder<ISA> as VmBuilder<
            BabyBearPoseidon2Engine,
        >>::create_chip_complex(
            &SpecializedConfigCpuBuilder::default(),
            &self.vm_config,
            air_inventory,
        )
        .unwrap();

        let inventory = chip_complex.inventory;

        // Order of precompile is the same as that of Powdr executors in chip inventory
        let mut apc_stats = self
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
}

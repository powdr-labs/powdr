use std::{cell::RefCell, rc::Rc};

use openvm_circuit::arch::MatrixRecordArena;
use openvm_stark_sdk::p3_baby_bear::BabyBear;

use crate::{
    executor::OriginalArenas,
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    isa::OpenVmISA,
    trace_generator::cpu::{
        periphery::SharedPeripheryChipsCpu, PowdrPeripheryInstancesCpu, PowdrTraceGeneratorCpu,
    },
    vm::PowdrPrecompile,
};

pub struct PowdrChipCpu<ISA: OpenVmISA> {
    pub name: String,
    pub record_arena_by_air_name: Rc<RefCell<OriginalArenas<MatrixRecordArena<BabyBear>>>>,
    pub trace_generator: PowdrTraceGeneratorCpu<ISA>,
}

impl<ISA: OpenVmISA> PowdrChipCpu<ISA> {
    pub fn new(
        precompile: PowdrPrecompile<BabyBear, ISA>,
        original_airs: OriginalAirs<BabyBear, ISA>,
        base_config: OriginalVmConfig<ISA>,
        periphery: PowdrPeripheryInstancesCpu<SharedPeripheryChipsCpu<ISA>>,
    ) -> Self {
        let PowdrPrecompile {
            name,
            apc,
            apc_record_arena_cpu: apc_record_arena,
            ..
        } = precompile;
        let trace_generator =
            PowdrTraceGeneratorCpu::new(apc, original_airs, base_config, periphery);

        Self {
            name,
            record_arena_by_air_name: apc_record_arena,
            trace_generator,
        }
    }
}

use crate::PowdrSdkCpu;
use crate::SpecializedConfigCpuBuilder;
use crate::{isa::OpenVmISA, program::CompiledProgram, SpecializedConfig};
use openvm_circuit::arch::{
    execution_mode::Segment, Executor, MeteredExecutor, PreflightExecutionOutput,
    PreflightExecutor, VirtualMachine, VmBuilder, VmExecutionConfig,
};
use openvm_stark_backend::StarkEngine;
use openvm_stark_backend::Val;
use openvm_stark_backend::{keygen::types::MultiStarkProvingKey, prover::ProvingContext};
use sdk_v2::{
    config::{
        default_app_params, AggregationSystemParams, AppConfig, DEFAULT_APP_LOG_BLOWUP,
        DEFAULT_APP_L_SKIP,
    },
    GenericSdk, StdIn,
};
use tracing::info_span;

use crate::BabyBearSC;

#[cfg(not(feature = "cuda"))]
use crate::PowdrSdkCpu as PowdrSdk;
#[cfg(feature = "cuda")]
use crate::PowdrSdkGpu as PowdrSdk;

#[cfg(not(feature = "cuda"))]
use crate::SpecializedConfigCpuBuilder as SpecializedConfigBuilder;
#[cfg(feature = "cuda")]
use crate::SpecializedConfigGpuBuilder as SpecializedConfigBuilder;

#[cfg(feature = "cuda")]
use openvm_cuda_backend::engine::GpuBabyBearPoseidon2CpuEngine as BabyBearPoseidon2CpuEngine;
#[cfg(not(feature = "cuda"))]
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2CpuEngine;

/// Given a program and input, generates the trace segment by segment and calls the provided
/// callback with the VM, proving key, and proving context (containing the trace) for each segment.
pub fn do_with_trace<ISA: OpenVmISA>(
    program: &CompiledProgram<ISA>,
    inputs: StdIn,
    callback: impl FnMut(
        usize,
        &VirtualMachine<BabyBearPoseidon2CpuEngine, SpecializedConfigBuilder<ISA>>,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<<BabyBearPoseidon2CpuEngine as StarkEngine>::PB>,
    ),
) -> Result<(), Box<dyn std::error::Error>> {
    let (app_config, agg_params) = create_app_config(program);
    let sdk = PowdrSdk::new(app_config, agg_params)?;
    do_with_trace_with_sdk::<ISA, BabyBearPoseidon2CpuEngine, SpecializedConfigBuilder<ISA>>(
        program, inputs, sdk, callback,
    )
}

/// Like [`do_with_trace`], but always uses the CPU engine and CPU VM config builder.
pub fn do_with_cpu_trace<ISA: OpenVmISA>(
    program: &CompiledProgram<ISA>,
    inputs: StdIn,
    callback: impl FnMut(
        usize,
        &VirtualMachine<
            openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2CpuEngine,
            SpecializedConfigCpuBuilder<ISA>,
        >,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<
            <openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2CpuEngine as StarkEngine>::PB,
        >,
    ),
) -> Result<(), Box<dyn std::error::Error>> {
    let (app_config, agg_params) = create_app_config(program);
    let sdk = PowdrSdkCpu::new(app_config, agg_params)?;
    do_with_trace_with_sdk::<
        ISA,
        openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2CpuEngine,
        SpecializedConfigCpuBuilder<ISA>,
    >(program, inputs, sdk, callback)
}

fn do_with_trace_with_sdk<ISA: OpenVmISA, E, VB>(
    program: &CompiledProgram<ISA>,
    inputs: StdIn,
    sdk: GenericSdk<E, VB>,
    mut callback: impl FnMut(
        usize,
        &VirtualMachine<E, VB>,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<<E as StarkEngine>::PB>,
    ),
) -> Result<(), Box<dyn std::error::Error>>
where
    E: StarkEngine<SC = BabyBearSC>,
    VB: VmBuilder<E> + Clone,
    <VB::VmConfig as VmExecutionConfig<Val<E::SC>>>::Executor: Executor<Val<E::SC>>
        + MeteredExecutor<Val<E::SC>>
        + PreflightExecutor<Val<E::SC>, VB::RecordArena>,
{
    let app_pk = sdk.app_pk();
    let pk = &*app_pk.app_vm_pk.vm_pk;

    let mut app_prover = sdk.app_prover(program.exe.clone())?;
    let instance = app_prover.instance_mut();

    // Metered execution to get segment boundaries
    let inputs_streams: openvm_circuit::arch::Streams<Val<E::SC>> = inputs.into();
    instance.reset_state(inputs_streams.clone());
    let exe = instance.exe().clone();
    let metered_ctx = instance.vm.build_metered_ctx(&exe);
    let metered_interpreter = instance.vm.metered_interpreter(&exe)?;
    let (segments, _) = metered_interpreter.execute_metered(inputs_streams, metered_ctx)?;

    // For each segment: preflight → generate proving context → callback
    let mut state = instance.state_mut().take();
    for (seg_idx, segment) in segments.into_iter().enumerate() {
        let _span = info_span!("trace_segment", segment = seg_idx).entered();
        let Segment {
            num_insns,
            trace_heights,
            ..
        } = segment;
        let from_state = state.take().unwrap();
        instance
            .vm
            .transport_init_memory_to_device(&from_state.memory);
        let PreflightExecutionOutput {
            system_records,
            record_arenas,
            to_state,
        } = instance.vm.execute_preflight(
            &mut instance.interpreter,
            from_state,
            Some(num_insns),
            &trace_heights,
        )?;
        state = Some(to_state);

        let ctx = instance
            .vm
            .generate_proving_ctx(system_records, record_arenas)?;
        callback(seg_idx, &instance.vm, pk, ctx);
    }
    *instance.state_mut() = state;

    Ok(())
}

fn create_app_config<ISA: OpenVmISA>(
    program: &CompiledProgram<ISA>,
) -> (AppConfig<SpecializedConfig<ISA>>, AggregationSystemParams) {
    let system_params = default_app_params(DEFAULT_APP_LOG_BLOWUP, DEFAULT_APP_L_SKIP, 21);
    let app_config = AppConfig::new(program.vm_config.clone(), system_params);
    (app_config, AggregationSystemParams::default())
}

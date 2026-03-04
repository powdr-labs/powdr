use openvm_circuit::arch::{
    execution_mode::Segment, Executor, MeteredExecutor, PreflightExecutionOutput,
    PreflightExecutor, VirtualMachine, VmBuilder, VmCircuitConfig, VmExecutionConfig, VmInstance,
};
use sdk_v2::{
    config::{AppConfig, AggregationSystemParams, DEFAULT_APP_LOG_BLOWUP, DEFAULT_APP_L_SKIP, default_app_params},
    GenericSdk, StdIn,
};
use openvm_stark_backend::Val;
use openvm_stark_backend::{keygen::types::MultiStarkProvingKey, prover::ProvingContext};
use openvm_stark_backend::StarkEngine;
use tracing::info_span;

use crate::{BabyBearSC, CompiledProgram};
use crate::{PowdrSdkCpu, SpecializedConfigCpuBuilder};

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
/// callback with the VM and proving context for each segment.
pub fn do_with_trace(
    program: &CompiledProgram,
    inputs: StdIn,
    callback: impl FnMut(
        usize,
        &VirtualMachine<BabyBearPoseidon2CpuEngine, SpecializedConfigBuilder>,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<<BabyBearPoseidon2CpuEngine as StarkEngine>::PB>,
    ),
) -> Result<(), Box<dyn std::error::Error>> {
    let (app_config, agg_params) = create_app_config(program);
    let sdk = PowdrSdk::new(app_config, agg_params)?;
    do_with_trace_with_sdk::<BabyBearPoseidon2CpuEngine, SpecializedConfigBuilder>(
        program, inputs, sdk, callback,
    )
}

/// Like [`do_with_trace`], but always uses the CPU engine and CPU VM config builder.
pub fn do_with_cpu_trace(
    program: &CompiledProgram,
    inputs: StdIn,
    callback: impl FnMut(
        usize,
        &VirtualMachine<openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2CpuEngine, SpecializedConfigCpuBuilder>,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<<openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2CpuEngine as StarkEngine>::PB>,
    ),
) -> Result<(), Box<dyn std::error::Error>> {
    let (app_config, agg_params) = create_app_config(program);
    let sdk = PowdrSdkCpu::new(app_config, agg_params)?;
    do_with_trace_with_sdk::<openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2CpuEngine, SpecializedConfigCpuBuilder>(
        program, inputs, sdk, callback,
    )
}

fn do_with_trace_with_sdk<E, VB>(
    _program: &CompiledProgram,
    _inputs: StdIn,
    _sdk: GenericSdk<E, VB>,
    mut _callback: impl FnMut(
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
    // TODO: The v2 API changed significantly for segment-by-segment proving.
    // The keygen/proving flow works differently in v2 (WHIR-based).
    // This needs to be adapted once the proving flow is fully ported.
    tracing::warn!("do_with_trace: v2 segment-by-segment proving not yet ported");
    Ok(())
}

fn create_app_config(program: &CompiledProgram) -> (AppConfig<crate::SpecializedConfig>, AggregationSystemParams) {
    let system_params = default_app_params(DEFAULT_APP_LOG_BLOWUP, DEFAULT_APP_L_SKIP, 21);
    let app_config = AppConfig::new(program.vm_config.clone(), system_params);
    (app_config, AggregationSystemParams::default())
}

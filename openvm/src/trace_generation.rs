use openvm_circuit::arch::{
    execution_mode::Segment, Executor, MeteredExecutor, PreflightExecutionOutput,
    PreflightExecutor, VirtualMachine, VmBuilder, VmCircuitConfig, VmExecutionConfig, VmInstance,
};
use openvm_native_circuit::NativeConfig;
use openvm_sdk::{
    config::{AppConfig, DEFAULT_APP_LOG_BLOWUP},
    prover::vm::new_local_prover,
    GenericSdk, StdIn,
};
use openvm_stark_backend::config::Val;
use openvm_stark_backend::{keygen::types::MultiStarkProvingKey, prover::types::ProvingContext};
use openvm_stark_sdk::{
    config::{
        baby_bear_poseidon2::BabyBearPoseidon2Engine as CpuBabyBearPoseidon2Engine, FriParameters,
    },
    engine::{StarkEngine, StarkFriEngine},
};
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
use openvm_cuda_backend::engine::GpuBabyBearPoseidon2Engine as BabyBearPoseidon2Engine;
#[cfg(not(feature = "cuda"))]
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;

/// Given a program and input, generates the trace segment by segment and calls the provided
/// callback with the VM, proving key, and proving context (containing the trace) for each segment.
pub fn do_with_trace(
    program: &CompiledProgram,
    inputs: StdIn,
    callback: impl FnMut(
        usize,
        &VirtualMachine<BabyBearPoseidon2Engine, SpecializedConfigBuilder>,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<<BabyBearPoseidon2Engine as StarkEngine>::PB>,
    ),
) -> Result<(), Box<dyn std::error::Error>> {
    let sdk = PowdrSdk::new(create_app_config(program))?;
    do_with_trace_with_sdk::<BabyBearPoseidon2Engine, SpecializedConfigBuilder, _>(
        program, inputs, sdk, callback,
    )
}

/// Like [`do_with_trace`], but always uses the CPU engine and CPU VM config builder.
pub fn do_with_cpu_trace(
    program: &CompiledProgram,
    inputs: StdIn,
    callback: impl FnMut(
        usize,
        &VirtualMachine<CpuBabyBearPoseidon2Engine, SpecializedConfigCpuBuilder>,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<<CpuBabyBearPoseidon2Engine as StarkEngine>::PB>,
    ),
) -> Result<(), Box<dyn std::error::Error>> {
    let sdk = PowdrSdkCpu::new(create_app_config(program))?;
    do_with_trace_with_sdk::<CpuBabyBearPoseidon2Engine, SpecializedConfigCpuBuilder, _>(
        program, inputs, sdk, callback,
    )
}

fn do_with_trace_with_sdk<E, VB, NB>(
    program: &CompiledProgram,
    inputs: StdIn,
    sdk: GenericSdk<E, VB, NB>,
    mut callback: impl FnMut(
        usize,
        &VirtualMachine<E, VB>,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<<E as StarkEngine>::PB>,
    ),
) -> Result<(), Box<dyn std::error::Error>>
where
    E: StarkFriEngine<SC = BabyBearSC>,
    VB: VmBuilder<E> + Clone,
    <VB::VmConfig as VmExecutionConfig<Val<E::SC>>>::Executor: Executor<Val<E::SC>>
        + MeteredExecutor<Val<E::SC>>
        + PreflightExecutor<Val<E::SC>, VB::RecordArena>,
    NB: VmBuilder<E, VmConfig = NativeConfig> + Clone,
    <NativeConfig as VmExecutionConfig<Val<E::SC>>>::Executor:
        PreflightExecutor<Val<E::SC>, NB::RecordArena>,
{
    let exe = sdk.convert_to_exe(program.exe.clone())?;
    // Build owned vm instance, so we can mutate it later
    let vm_builder = sdk.app_vm_builder().clone();
    let vm_pk = sdk.app_pk().app_vm_pk.clone();
    let mut vm_instance: VmInstance<_, _> = new_local_prover(vm_builder, &vm_pk, exe.clone())?;

    vm_instance.reset_state(inputs.clone());
    let metered_ctx = vm_instance.vm.build_metered_ctx(&exe);
    let metered_interpreter = vm_instance.vm.metered_interpreter(vm_instance.exe())?;
    let (segments, _) = metered_interpreter.execute_metered(inputs.clone(), metered_ctx)?;
    let mut state = vm_instance.state_mut().take();

    // Move `vm` and `interpreter` out of `vm_instance`
    // (after this, you can't use `vm_instance` anymore).
    let mut vm = vm_instance.vm;
    let mut interpreter = vm_instance.interpreter;

    // Get reusable inputs for `debug_proving_ctx`, the mock prover API from OVM.
    let air_inv = vm.config().create_airs()?;
    let pk = air_inv.keygen::<E>(&vm.engine);

    for (seg_idx, segment) in segments.into_iter().enumerate() {
        let _segment_span = info_span!("prove_segment", segment = seg_idx).entered();
        // We need a separate span so the metric label includes "segment" from _segment_span
        let _prove_span = info_span!("total_proof").entered();
        let Segment {
            num_insns,
            trace_heights,
            ..
        } = segment;
        let from_state = Option::take(&mut state).unwrap();
        vm.transport_init_memory_to_device(&from_state.memory);
        let PreflightExecutionOutput {
            system_records,
            record_arenas,
            to_state,
        } = vm.execute_preflight(
            &mut interpreter,
            from_state,
            Some(num_insns),
            &trace_heights,
        )?;
        state = Some(to_state);

        let ctx = vm.generate_proving_ctx(system_records, record_arenas)?;

        callback(seg_idx, &vm, &pk, ctx);
    }
    Ok(())
}

fn create_app_config(program: &CompiledProgram) -> AppConfig<crate::SpecializedConfig> {
    let app_fri_params =
        FriParameters::standard_with_100_bits_conjectured_security(DEFAULT_APP_LOG_BLOWUP);
    AppConfig::new(app_fri_params, program.vm_config.clone())
}

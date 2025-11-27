use openvm_circuit::arch::{
    execution_mode::Segment, PreflightExecutionOutput, VirtualMachine, VmCircuitConfig, VmInstance,
};
use openvm_sdk::{
    config::{AppConfig, DEFAULT_APP_LOG_BLOWUP},
    prover::vm::new_local_prover,
    StdIn,
};
use openvm_stark_backend::{keygen::types::MultiStarkProvingKey, prover::types::ProvingContext};
use openvm_stark_sdk::{config::FriParameters, engine::StarkEngine};
use tracing::info_span;

use crate::{BabyBearSC, CompiledProgram, SpecializedConfigCpuBuilder};

#[cfg(not(feature = "cuda"))]
use crate::PowdrSdkCpu;
#[cfg(feature = "cuda")]
use crate::PowdrSdkGpu;

#[cfg(not(feature = "cuda"))]
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;
#[cfg(feature = "cuda")]
use openvm_stark_sdk::config::gpu_baby_bear_poseidon2::GpuBabyBearPoseidon2Engine;

/// Given a program and input, generates the trace segment by segment and calls the provided
/// callback with the VM, proving key, and proving context (containing the trace) for each segment.
pub fn do_with_trace(
    program: &CompiledProgram,
    inputs: StdIn,
    mut callback: impl FnMut(
        &VirtualMachine<BabyBearPoseidon2Engine, SpecializedConfigCpuBuilder>,
        &MultiStarkProvingKey<BabyBearSC>,
        ProvingContext<<BabyBearPoseidon2Engine as StarkEngine>::PB>,
    ),
) {
    let exe = &program.exe;
    let vm_config = program.vm_config.clone();

    // Set app configuration
    let app_fri_params =
        FriParameters::standard_with_100_bits_conjectured_security(DEFAULT_APP_LOG_BLOWUP);
    let app_config = AppConfig::new(app_fri_params, vm_config.clone());

    // Create the SDK
    #[cfg(feature = "cuda")]
    let sdk = PowdrSdkGpu::new(app_config).unwrap();
    #[cfg(not(feature = "cuda"))]
    let sdk = PowdrSdkCpu::new(app_config).unwrap();
    // Build owned vm instance, so we can mutate it later
    let vm_builder = sdk.app_vm_builder().clone();
    let vm_pk = sdk.app_pk().app_vm_pk.clone();
    let exe = sdk.convert_to_exe(exe.clone()).unwrap();
    let mut vm_instance: VmInstance<_, _> =
        new_local_prover(vm_builder, &vm_pk, exe.clone()).unwrap();

    vm_instance.reset_state(inputs.clone());
    let metered_ctx = vm_instance.vm.build_metered_ctx(&exe);
    let metered_interpreter = vm_instance
        .vm
        .metered_interpreter(vm_instance.exe())
        .unwrap();
    let (segments, _) = metered_interpreter
        .execute_metered(inputs.clone(), metered_ctx)
        .unwrap();
    let mut state = vm_instance.state_mut().take();

    // Move `vm` and `interpreter` out of `vm_instance`
    // (after this, you can't use `vm_instance` anymore).
    let mut vm = vm_instance.vm;
    let mut interpreter = vm_instance.interpreter;

    // Get reusable inputs for `debug_proving_ctx`, the mock prover API from OVM.
    let air_inv = vm.config().create_airs().unwrap();
    #[cfg(feature = "cuda")]
    let pk = air_inv.keygen::<GpuBabyBearPoseidon2Engine>(&vm.engine);
    #[cfg(not(feature = "cuda"))]
    let pk = air_inv.keygen::<BabyBearPoseidon2Engine>(&vm.engine);

    for (seg_idx, segment) in segments.into_iter().enumerate() {
        let _segment_span = info_span!("prove_segment", segment = seg_idx).entered();
        // We need a separate span so the metric label includes "segment" from _segment_span
        let _prove_span = info_span!("total_proof").entered();
        let Segment {
            instret_start,
            num_insns,
            trace_heights,
        } = segment;
        assert_eq!(state.as_ref().unwrap().instret(), instret_start);
        let from_state = Option::take(&mut state).unwrap();
        vm.transport_init_memory_to_device(&from_state.memory);
        let PreflightExecutionOutput {
            system_records,
            record_arenas,
            to_state,
        } = vm
            .execute_preflight(
                &mut interpreter,
                from_state,
                Some(num_insns),
                &trace_heights,
            )
            .unwrap();
        state = Some(to_state);

        // Generate proving context for each segment
        let ctx = vm
            .generate_proving_ctx(system_records, record_arenas)
            .unwrap();

        callback(&vm, &pk, ctx);
    }
}

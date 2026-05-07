use std::collections::{BTreeMap, HashMap};

use itertools::Itertools;
use openvm_circuit::{
    arch::{ChipInventory, DenseRecordArena},
    utils::next_power_of_two_or_zero,
};
use openvm_circuit_primitives::Chip;
use openvm_cuda_backend::base::DeviceMatrix;
use openvm_cuda_common::copy::MemCopyH2D;
use openvm_stark_backend::{
    p3_field::PrimeField32,
    prover::{AirProvingContext, ProverBackend},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicExpression, AlgebraicReference},
    symbolic_machine::SymbolicBusInteraction,
};
use powdr_constraint_solver::constraint_system::{ComputationMethod, DerivedVariable};
use powdr_expression::{AlgebraicBinaryOperator, AlgebraicUnaryOperator};

use crate::{
    cuda_abi::{self, DerivedExprSpec, DevInteraction, ExprSpan, OpCode, OriginalAir, Subst},
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    isa::{IsaApc, OpenVmISA},
    powdr_extension::{chip::PowdrChipGpu, executor::OriginalArenas},
    BabyBearSC, GpuBackend,
};

mod inventory;
pub mod nvrtc_bus_emit;
pub mod nvrtc_cache;
pub mod nvrtc_emit;
mod periphery;

pub use inventory::GpuDummyChipComplex;
pub use periphery::{
    PowdrPeripheryInstancesGpu, SharedPeripheryChipsGpu, SharedPeripheryChipsGpuProverExt,
};

/// Encodes an algebraic expression into GPU stack-machine bytecode.
///
/// Appends instructions to `bc` representing `expr` using the opcodes in `OpCode`.
/// References are encoded as `PushApc` with a column-major offset computed from
/// `id_to_apc_index` and `apc_height` (offset = apc_col_index * apc_height).
/// Constants are encoded as `PushConst` followed by the field element as `u32`.
/// Unary minus and binary operations map to `Neg`, `Add`, `Sub`, and `Mul`.
///
/// Note: This function does not track or enforce the evaluation stack depth,
/// which is done in device code.
fn emit_expr(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<BabyBear>,
    id_to_apc_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) {
    match expr {
        AlgebraicExpression::Number(c) => {
            bc.push(OpCode::PushConst as u32);
            bc.push(c.as_canonical_u32());
        }
        AlgebraicExpression::Reference(r) => {
            let idx = (id_to_apc_index[&r.id] * apc_height) as u32;
            bc.push(OpCode::PushApc as u32);
            bc.push(idx);
        }
        AlgebraicExpression::UnaryOperation(u) => {
            emit_expr(bc, &u.expr, id_to_apc_index, apc_height);
            match u.op {
                AlgebraicUnaryOperator::Minus => bc.push(OpCode::Neg as u32),
            }
        }
        AlgebraicExpression::BinaryOperation(b) => {
            emit_expr(bc, &b.left, id_to_apc_index, apc_height);
            emit_expr(bc, &b.right, id_to_apc_index, apc_height);
            match b.op {
                AlgebraicBinaryOperator::Add => bc.push(OpCode::Add as u32),
                AlgebraicBinaryOperator::Sub => bc.push(OpCode::Sub as u32),
                AlgebraicBinaryOperator::Mul => bc.push(OpCode::Mul as u32),
            }
        }
    }
}

/// Given the current bytecode, appends bytecode for the expression `expr` and returns the associated span
fn emit_expr_span(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<BabyBear>,
    id_to_apc_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> ExprSpan {
    // The span starts where the bytecode currently ends
    let off = bc.len() as u32;
    // Append the bytecode for `expr`
    emit_expr(bc, expr, id_to_apc_index, apc_height);
    // Calculate the length of the span
    let len = (bc.len() as u32) - off;
    ExprSpan { off, len }
}

/// Compile derived columns to GPU bytecode according to input order.
fn compile_derived_to_gpu(
    derived_columns: &[DerivedVariable<
        BabyBear,
        AlgebraicReference,
        AlgebraicExpression<BabyBear>,
    >],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DerivedExprSpec>, Vec<u32>) {
    let mut specs = Vec::with_capacity(derived_columns.len());
    let mut bytecode = Vec::new();

    for DerivedVariable {
        variable,
        computation_method,
    } in derived_columns
    {
        let apc_col_index = apc_poly_id_to_index[&variable.id];
        let off = bytecode.len() as u32;
        match computation_method {
            ComputationMethod::Constant(c) => {
                // Encode constant as an expression
                bytecode.push(OpCode::PushConst as u32);
                bytecode.push(c.as_canonical_u32());
            }
            ComputationMethod::QuotientOrZero(e1, e2) => {
                // Invert denominator (or use zero), then multiply with numerator.
                emit_expr(&mut bytecode, e2, apc_poly_id_to_index, apc_height);
                bytecode.push(OpCode::InvOrZero as u32);
                emit_expr(&mut bytecode, e1, apc_poly_id_to_index, apc_height);
                bytecode.push(OpCode::Mul as u32);
            }
        }
        let len = (bytecode.len() as u32) - off;
        specs.push(DerivedExprSpec {
            col_base: (apc_col_index * apc_height) as u64,
            span: ExprSpan { off, len },
        });
    }

    (specs, bytecode)
}

pub fn compile_bus_to_gpu(
    bus_interactions: &[SymbolicBusInteraction<BabyBear>],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DevInteraction>, Vec<ExprSpan>, Vec<u32>) {
    let mut interactions = Vec::with_capacity(bus_interactions.len());
    let mut arg_spans = Vec::new();
    let mut bytecode = Vec::new();

    for bus_interaction in bus_interactions {
        // multiplicity as first arg span
        let args_index_off = arg_spans.len() as u32;
        let mult_span = emit_expr_span(
            &mut bytecode,
            &bus_interaction.mult,
            apc_poly_id_to_index,
            apc_height,
        );
        arg_spans.push(mult_span);

        // args
        for arg in &bus_interaction.args {
            let span = emit_expr_span(&mut bytecode, arg, apc_poly_id_to_index, apc_height);
            arg_spans.push(span);
        }

        interactions.push(DevInteraction {
            bus_id: (bus_interaction.id as u32),
            num_args: bus_interaction.args.len() as u32,
            args_index_off,
        });
    }

    (interactions, arg_spans, bytecode)
}

/// Lazily-initialized per-APC NVRTC bus-codegen state. Holds the bus
/// partition, the per-APC poly-id → APC-column index map, and the four
/// compiled kernel handles. Built once per APC at construction (eagerly,
/// when `POWDR_BUS_CODEGEN=1`); reused for every prove of the same APC.
struct CachedBusCodegenKernels {
    partition: nvrtc_bus_emit::PartitionedBus,
    apc_poly_id_to_index: BTreeMap<u64, usize>,
    var_compiled: Option<std::sync::Arc<nvrtc_cache::CompiledKernel>>,
    tup_compiled: Option<std::sync::Arc<nvrtc_cache::CompiledKernel>>,
    br_compiled: Option<std::sync::Arc<nvrtc_cache::CompiledKernel>>,
    bx_compiled: Option<std::sync::Arc<nvrtc_cache::CompiledKernel>>,
}

/// Returns true when `POWDR_BUS_CODEGEN=1` selects the per-APC NVRTC
/// codegen path for `apc_apply_bus`. Cached env-read.
fn bus_codegen_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var("POWDR_BUS_CODEGEN").is_ok())
}

pub struct PowdrTraceGeneratorGpu<ISA: OpenVmISA> {
    pub apc: IsaApc<BabyBear, ISA>,
    pub original_airs: OriginalAirs<BabyBear, ISA>,
    pub config: OriginalVmConfig<ISA>,
    pub periphery: PowdrPeripheryInstancesGpu<ISA>,
    /// Per-APC codegen kernel cache. Populated at construction when
    /// `POWDR_BUS_CODEGEN=1`; otherwise empty and never read.
    codegen_cache: std::sync::OnceLock<std::sync::Arc<CachedBusCodegenKernels>>,
}

/// Cached `POWDR_TRACE_PROFILE` env var read. When set, `timed_substage!`
/// synchronizes the GPU stream before each span closes so per-stage timings
/// reflect device-side execution. Off by default to avoid serializing host
/// work with GPU work — pure-launch stages (`tracegen_kernel`, `bus_kernel`)
/// then show enqueue time only.
fn trace_profile_sync_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var("POWDR_TRACE_PROFILE").is_ok())
}

/// Wrap a substage in an INFO `tracing` span so openvm's `TimingMetricsLayer`
/// auto-emits `<name>_time_ms`, with the `group` / `air` labels from the
/// surrounding `app_prove` / `single_trace_gen` spans propagated by
/// `TracingContextLayer`.
macro_rules! timed_substage {
    ($name:literal, $body:expr) => {{
        let _span = ::tracing::info_span!($name).entered();
        let r = $body;
        if crate::powdr_extension::trace_generator::cuda::trace_profile_sync_enabled() {
            let _ = ::openvm_cuda_common::stream::current_stream_sync();
        }
        r
    }};
}

impl<ISA: OpenVmISA> PowdrTraceGeneratorGpu<ISA> {
    pub fn new(
        apc: IsaApc<BabyBear, ISA>,
        original_airs: OriginalAirs<BabyBear, ISA>,
        config: OriginalVmConfig<ISA>,
        periphery: PowdrPeripheryInstancesGpu<ISA>,
    ) -> Self {
        let gen = Self {
            apc,
            original_airs,
            config,
            periphery,
            codegen_cache: std::sync::OnceLock::new(),
        };

        // When NVRTC bus codegen is selected, eagerly partition + emit +
        // compile at construction time so the cost moves to APC-load time
        // and never hits any prove timer. The kernel sources are
        // APC-deterministic and `H`-independent, so this is safe to do
        // once per APC.
        if bus_codegen_enabled() {
            gen.warm_codegen_cache();
        }

        gen
    }

    /// Eagerly populate `codegen_cache`. Idempotent — `OnceLock::get_or_init`
    /// makes repeated calls a no-op. Safe to skip when codegen is disabled.
    fn warm_codegen_cache(&self) {
        let apc_poly_id_to_index: BTreeMap<u64, usize> = self
            .apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        let var_range_bus_id = self.periphery.bus_ids.range_checker as u32;
        let tuple2_bus_id = self
            .periphery
            .bus_ids
            .tuple_range_checker
            .map(|x| x as u32)
            .unwrap_or(0);
        let bitwise_bus_id = self
            .periphery
            .bus_ids
            .bitwise_lookup
            .map(|x| x as u32)
            .unwrap_or(0);

        let _ = self.codegen_cache.get_or_init(|| {
            self.build_codegen_cache(
                apc_poly_id_to_index,
                var_range_bus_id,
                tuple2_bus_id,
                bitwise_bus_id,
            )
        });
    }

    /// Partition + emit + NVRTC-compile the per-APC bus kernels. Called
    /// once per APC. `H` is intentionally NOT baked into the kernel source
    /// (the kernel takes it as a runtime arg), so the cache is height-
    /// independent and reusable across proves of any size.
    fn build_codegen_cache(
        &self,
        apc_poly_id_to_index: BTreeMap<u64, usize>,
        var_range_bus_id: u32,
        tuple2_bus_id: u32,
        bitwise_bus_id: u32,
    ) -> std::sync::Arc<CachedBusCodegenKernels> {
        use nvrtc_bus_emit::{
            build_emitter_input, emit_codegen_bitwise, emit_codegen_tuple2, emit_codegen_var_range,
            partition_apc_bus,
        };
        use nvrtc_cache::NvrtcKernelCache;

        let partition = timed_substage!("bus_nvrtc_codegen", {
            let input = build_emitter_input(
                &self.apc.machine.bus_interactions,
                &apc_poly_id_to_index,
                /* height: not baked into source */ 1,
                var_range_bus_id as u64,
                Some(tuple2_bus_id as u64),
                Some(bitwise_bus_id as u64),
            );
            partition_apc_bus(&input)
                .unwrap_or_else(|e| panic!("NVRTC bus partition failed: {}", e))
        });

        // Per-APC capture diagnostics. Counts every interaction in the APC's
        // bus list as exactly one of: codegen-captured (affine or bilinear,
        // by kind), unsupported-bus (memory/exec/program — bytecode-VM
        // ignores them too), or fallback (mult/args don't fit either decoder
        // — bytecode-VM picks them up via the unhandled tail). The metric
        // names match what `reth_capture_stats.py` aggregates.
        let captured_var = partition.var_ops.len() + partition.var_ops_bilinear.len();
        let captured_tup = partition.tuple_ops.len() + partition.tuple_ops_bilinear.len();
        let captured_br = partition.bitwise_range_ops.len()
            + partition.bitwise_range_ops_bilinear.len();
        let captured_bx = partition.bitwise_xor_ops.len()
            + partition.bitwise_xor_ops_bilinear.len();
        let captured = captured_var + captured_tup + captured_br + captured_bx;
        let unhandled = partition.unhandled.len();
        let unsupported = partition.n_unsupported;
        let total = captured + unhandled + unsupported;
        // increment, not absolute — each APC contributes; aggregator sums
        // across all APCs in a run.
        metrics::counter!("apc_bus_total").increment(total as u64);
        metrics::counter!("apc_bus_captured").increment(captured as u64);
        metrics::counter!("apc_bus_captured_var_range").increment(captured_var as u64);
        metrics::counter!("apc_bus_captured_tuple2").increment(captured_tup as u64);
        metrics::counter!("apc_bus_captured_bitwise_range").increment(captured_br as u64);
        metrics::counter!("apc_bus_captured_bitwise_xor").increment(captured_bx as u64);
        metrics::counter!("apc_bus_unhandled").increment(unhandled as u64);
        metrics::counter!("apc_bus_unsupported").increment(unsupported as u64);
        // Hash the partitioned bus shape so each APC can be distinguished
        // in logs even though we don't have a chip name here. Two APCs
        // with the same bus shape collide, but for our analysis purposes
        // (counting unique fallback shapes) that's actually desirable.
        let apc_id = {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut h = DefaultHasher::new();
            self.apc.machine.bus_interactions.len().hash(&mut h);
            for bi in &self.apc.machine.bus_interactions {
                bi.id.hash(&mut h);
                bi.args.len().hash(&mut h);
            }
            format!("{:016x}", h.finish())
        };
        tracing::info!(
            "[apc_bus_partition] apc={} total={} captured={} (var={} tup={} br={} bx={}) unhandled={} unsupported={}",
            apc_id, total, captured, captured_var, captured_tup, captured_br, captured_bx,
            unhandled, unsupported,
        );
        // Log each unhandled interaction's shape so we can analyze what
        // additional capture strategies would unlock. `bus_id` distinguishes
        // var_range / tuple2 / bitwise; `mult` and `args` shapes are what
        // the affine + bilinear decoders couldn't simplify.
        for &idx in &partition.unhandled {
            let bi = &self.apc.machine.bus_interactions[idx];
            let bus_kind = if bi.id as u32 == var_range_bus_id {
                "var_range"
            } else if bi.id as u32 == tuple2_bus_id {
                "tuple2"
            } else if bi.id as u32 == bitwise_bus_id {
                "bitwise"
            } else {
                "other"
            };
            tracing::info!(
                "[apc_bus_unhandled] apc={} idx={} bus={} mult={} args=[{}]",
                apc_id,
                idx,
                bus_kind,
                bi.mult,
                bi.args
                    .iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        let kernels = timed_substage!("bus_nvrtc_emit", {
            (
                emit_codegen_var_range(&partition.var_ops, &partition.var_ops_bilinear),
                emit_codegen_tuple2(&partition.tuple_ops, &partition.tuple_ops_bilinear),
                emit_codegen_bitwise(
                    &partition.bitwise_range_ops,
                    &partition.bitwise_range_ops_bilinear,
                    false,
                ),
                emit_codegen_bitwise(
                    &partition.bitwise_xor_ops,
                    &partition.bitwise_xor_ops_bilinear,
                    true,
                ),
            )
        });

        let compiled = timed_substage!("bus_nvrtc_compile", {
            let kernels_vec: Vec<_> = [&kernels.0, &kernels.1, &kernels.2, &kernels.3]
                .iter()
                .filter_map(|k| (*k).clone())
                .collect();
            let names: Vec<_> = kernels_vec.iter().map(|k| k.name.clone()).collect();
            let result = NvrtcKernelCache::global().get_or_compile_many(&kernels_vec);
            result
                .into_iter()
                .enumerate()
                .map(|(i, r)| {
                    r.unwrap_or_else(|e| panic!("NVRTC compile failed for {}: {:?}", names[i], e))
                })
                .collect::<Vec<_>>()
        });

        // Map compiled-list back to per-kind Optional handles.
        let mut compiled_iter = compiled.into_iter();
        let var_compiled = kernels.0.as_ref().map(|_| compiled_iter.next().unwrap());
        let tup_compiled = kernels.1.as_ref().map(|_| compiled_iter.next().unwrap());
        let br_compiled = kernels.2.as_ref().map(|_| compiled_iter.next().unwrap());
        let bx_compiled = kernels.3.as_ref().map(|_| compiled_iter.next().unwrap());

        std::sync::Arc::new(CachedBusCodegenKernels {
            partition,
            apc_poly_id_to_index,
            var_compiled,
            tup_compiled,
            br_compiled,
            bx_compiled,
        })
    }

    /// Per-APC codegen bus pass. Reads the pre-compiled `codegen_cache`,
    /// launches each kind's kernel, and falls back to the bytecode-VM
    /// path for any interactions that didn't fit the codegen-handled
    /// shape. Substages: `bus_nvrtc_kernel` (codegen launches) +
    /// optionally `bus_compile_h2d` / `bus_kernel` for unhandled tail.
    #[allow(clippy::too_many_arguments)]
    fn launch_nvrtc_bus_codegen(
        &self,
        output: &DeviceMatrix<BabyBear>,
        num_apc_calls: usize,
        height: usize,
        var_range_bus_id: u32,
        var_range_count: &openvm_cuda_common::d_buffer::DeviceBuffer<BabyBear>,
        tuple2_bus_id: u32,
        tuple2_count: &openvm_cuda_common::d_buffer::DeviceBuffer<BabyBear>,
        tuple2_sizes: [u32; 2],
        bitwise_bus_id: u32,
        bitwise_count: &openvm_cuda_common::d_buffer::DeviceBuffer<BabyBear>,
    ) {
        let cached = self
            .codegen_cache
            .get()
            .expect("codegen_cache must be warmed before launch")
            .clone();
        let p = &cached.partition;

        let block_x = 256u32;
        let warps_per_block = 8u32;

        timed_substage!("bus_nvrtc_kernel", {
            if let (Some(comp), n) = (
                cached.var_compiled.as_ref(),
                (p.var_ops.len() + p.var_ops_bilinear.len()) as u32,
            ) {
                let grid_x = n.div_ceil(warps_per_block).max(1);
                let rc = unsafe {
                    cuda_abi::powdr_nvrtc_launch_bus_v4(
                        comp.function(),
                        output.buffer().as_ptr() as *const u32,
                        num_apc_calls as i32,
                        height as u64,
                        var_range_count.as_mut_ptr() as *mut u32,
                        var_range_count.len() as u32,
                        0,
                        0,
                        grid_x,
                        block_x,
                    )
                };
                assert_eq!(rc, 0, "var_range codegen launch rc={}", rc);
            }

            if let (Some(comp), n) = (
                cached.tup_compiled.as_ref(),
                (p.tuple_ops.len() + p.tuple_ops_bilinear.len()) as u32,
            ) {
                let grid_x = n.div_ceil(warps_per_block).max(1);
                let rc = unsafe {
                    cuda_abi::powdr_nvrtc_launch_bus_v4(
                        comp.function(),
                        output.buffer().as_ptr() as *const u32,
                        num_apc_calls as i32,
                        height as u64,
                        tuple2_count.as_mut_ptr() as *mut u32,
                        tuple2_sizes[0],
                        tuple2_sizes[1],
                        1,
                        grid_x,
                        block_x,
                    )
                };
                assert_eq!(rc, 0, "tuple2 codegen launch rc={}", rc);
            }

            if let (Some(comp), n) = (
                cached.br_compiled.as_ref(),
                (p.bitwise_range_ops.len() + p.bitwise_range_ops_bilinear.len()) as u32,
            ) {
                let grid_x = n.div_ceil(warps_per_block).max(1);
                let rc = unsafe {
                    cuda_abi::powdr_nvrtc_launch_bus_v4_bitwise(
                        comp.function(),
                        output.buffer().as_ptr() as *const u32,
                        num_apc_calls as i32,
                        height as u64,
                        bitwise_count.as_mut_ptr() as *mut u32,
                        grid_x,
                        block_x,
                    )
                };
                assert_eq!(rc, 0, "bitwise_range codegen launch rc={}", rc);
            }

            if let (Some(comp), n) = (
                cached.bx_compiled.as_ref(),
                (p.bitwise_xor_ops.len() + p.bitwise_xor_ops_bilinear.len()) as u32,
            ) {
                let grid_x = n.div_ceil(warps_per_block).max(1);
                let rc = unsafe {
                    cuda_abi::powdr_nvrtc_launch_bus_v4_bitwise(
                        comp.function(),
                        output.buffer().as_ptr() as *const u32,
                        num_apc_calls as i32,
                        height as u64,
                        bitwise_count.as_mut_ptr() as *mut u32,
                        grid_x,
                        block_x,
                    )
                };
                assert_eq!(rc, 0, "bitwise_xor codegen launch rc={}", rc);
            }
        });

        // Bytecode-VM fallback for any interactions the codegen path
        // couldn't capture. Re-uses the existing `bus_compile_h2d` /
        // `bus_kernel` substages.
        if !p.unhandled.is_empty() {
            let (bus_interactions, arg_spans, bytecode) = timed_substage!("bus_compile_h2d", {
                let unhandled_interactions: Vec<_> = p
                    .unhandled
                    .iter()
                    .map(|&i| self.apc.machine.bus_interactions[i].clone())
                    .collect();
                let (bus_interactions, arg_spans, bytecode) = compile_bus_to_gpu(
                    &unhandled_interactions,
                    &cached.apc_poly_id_to_index,
                    height,
                );
                (
                    bus_interactions.to_device().unwrap(),
                    arg_spans.to_device().unwrap(),
                    bytecode.to_device().unwrap(),
                )
            });
            timed_substage!("bus_kernel", {
                cuda_abi::apc_apply_bus(
                    output,
                    num_apc_calls,
                    bytecode,
                    bus_interactions,
                    arg_spans,
                    var_range_bus_id,
                    var_range_count,
                    tuple2_bus_id,
                    tuple2_count,
                    tuple2_sizes,
                    bitwise_bus_id,
                    bitwise_count,
                )
                .unwrap();
            });
        }
    }

    fn try_generate_witness(
        &self,
        original_arenas: OriginalArenas<DenseRecordArena>,
    ) -> Option<DeviceMatrix<BabyBear>> {
        let mut original_arenas = match original_arenas {
            OriginalArenas::Initialized(arenas) => arenas,
            OriginalArenas::Uninitialized => {
                // if the arenas are uninitialized, the apc was not called, so we return early
                return None;
            }
        };

        let num_apc_calls = original_arenas.number_of_calls;

        let chip_inventory: ChipInventory<BabyBearSC, DenseRecordArena, GpuBackend> =
            timed_substage!("dummy_chip_inventory", {
                let airs =
                    ISA::create_dummy_airs(self.config.config(), self.periphery.dummy.clone())
                        .expect("Failed to create dummy airs");

                ISA::create_dummy_chip_complex_gpu(
                    self.config.config(),
                    airs,
                    self.periphery.dummy.clone(),
                )
                .expect("Failed to create chip complex")
                .inventory
            });

        let dummy_trace_by_air_name: HashMap<String, DeviceMatrix<BabyBear>> =
            timed_substage!("dummy_trace_gen", {
                chip_inventory
                    .chips()
                    .iter()
                    .enumerate()
                    .rev()
                    .filter_map(|(insertion_idx, chip)| {
                        let air_name = chip_inventory.airs().ext_airs()[insertion_idx].name();

                        let record_arena = {
                            match original_arenas.take_real_arena(&air_name) {
                                Some(ra) => ra,
                                None => return None, // skip this iteration, because we only have record arena for chips that are used
                            }
                        };

                        // We might have initialized an arena for an AIR which ends up having no real records. It gets filtered out here.
                        let ctx = chip.generate_proving_ctx(record_arena);
                        let m = ctx.common_main;
                        use openvm_stark_backend::prover::MatrixDimensions;
                        if m.height() > 0 {
                            Some((air_name, m))
                        } else {
                            None
                        }
                    })
                    .collect()
            });

        // Map from apc poly id to its index in the final apc trace
        let apc_poly_id_to_index: BTreeMap<u64, usize> = self
            .apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        // allocate for apc trace (zero-initialized so columns not covered
        // by substitutions or derived expressions default to zero, matching the CPU path)
        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut output = DeviceMatrix::<BabyBear>::with_capacity(height, width);
        output.buffer().fill_zero().unwrap();

        // Prepare `OriginalAir` and `Subst` arrays
        let (airs, substitutions) = {
            self.apc
                // go through original instructions
                .instructions()
                // along with their substitutions
                .zip_eq(self.apc.subs())
                // map to `(air_name, substitutions)`
                .filter_map(|(instr, subs)| {
                    if subs.is_empty() {
                        None
                    } else {
                        Some((&self.original_airs.opcode_to_air[&instr.inner.opcode], subs))
                    }
                })
                // group by air name. This results in `HashMap<air_name, Vec<subs>>` where the length of the vector is the number of rows which are created in this air, per apc call
                .into_group_map()
                // go through each air and its substitutions
                .iter()
                .enumerate()
                .fold(
                    (Vec::new(), Vec::new()),
                    |(mut airs, mut substitutions), (air_index, (air_name, subs_by_row))| {
                        // Find the substitutions that map to an apc column
                        let new_substitutions: Vec<Subst> = subs_by_row
                            .iter()
                            // enumerate over them to get the row index inside the air block
                            .enumerate()
                            .flat_map(|(row, subs)| {
                                // for each substitution, map to `Subst` struct
                                subs.iter()
                                    .map(move |sub| (row, sub))
                                    .map(|(row, sub)| Subst {
                                        air_index: air_index as i32,
                                        col: sub.original_poly_index as i32,
                                        row: row as i32,
                                        apc_col: apc_poly_id_to_index[&sub.apc_poly_id] as i32,
                                    })
                            })
                            .collect();

                        // get the device dummy trace for this air
                        let dummy_trace = &dummy_trace_by_air_name[*air_name];

                        use openvm_stark_backend::prover::MatrixDimensions;
                        airs.push(OriginalAir {
                            width: dummy_trace.width() as i32,
                            height: dummy_trace.height() as i32,
                            buffer: dummy_trace.buffer().as_ptr(),
                            row_block_size: subs_by_row.len() as i32,
                        });

                        substitutions.extend(new_substitutions);

                        (airs, substitutions)
                    },
                )
        };

        // Send the airs and substitutions to device
        let (airs, substitutions) = timed_substage!("tracegen_h2d", {
            (
                airs.to_device().unwrap(),
                substitutions.to_device().unwrap(),
            )
        });

        timed_substage!("tracegen_kernel", {
            cuda_abi::apc_tracegen(&mut output, airs, substitutions, num_apc_calls).unwrap();
        });

        // Apply derived columns using the GPU expression evaluator
        timed_substage!("derived", {
            let (derived_specs, derived_bc) = compile_derived_to_gpu(
                &self.apc.machine.derived_columns,
                &apc_poly_id_to_index,
                height,
            );
            // In practice `d_specs` is never empty, because we will always have `is_valid`
            let d_specs = derived_specs.to_device().unwrap();
            let d_bc = derived_bc.to_device().unwrap();
            cuda_abi::apc_apply_derived_expr(&mut output, d_specs, d_bc, num_apc_calls).unwrap();
        });

        // Gather GPU inputs for periphery (bus ids, count device buffers)
        let periphery = &self.periphery.real;

        // Range checker
        let var_range_bus_id = self.periphery.bus_ids.range_checker as u32;
        let var_range_count = &periphery.range_checker.count;

        // Tuple checker
        let tuple_range_checker_chip = periphery.tuple_range_checker.as_ref().unwrap();
        let tuple2_bus_id = self.periphery.bus_ids.tuple_range_checker.unwrap() as u32;
        let tuple2_sizes = tuple_range_checker_chip.sizes;
        let tuple2_count_u32 = tuple_range_checker_chip.count.as_ref();

        // Bitwise lookup; NUM_BITS is fixed at 8 in CUDA
        let bitwise_bus_id = self.periphery.bus_ids.bitwise_lookup.unwrap() as u32;
        let bitwise_count_u32 = periphery.bitwise_lookup_8.as_ref().unwrap().count.as_ref();

        // Bus-application: per-APC NVRTC codegen kernels when
        // `POWDR_BUS_CODEGEN=1`, otherwise the bytecode-VM kernel. Both
        // are implicitly serialized after `apc_tracegen` via the default
        // stream — bus evaluation depends on trace results.
        if bus_codegen_enabled() {
            self.launch_nvrtc_bus_codegen(
                &output,
                num_apc_calls,
                height,
                var_range_bus_id,
                var_range_count,
                tuple2_bus_id,
                tuple2_count_u32,
                tuple2_sizes,
                bitwise_bus_id,
                bitwise_count_u32,
            );
        } else {
            // Encode bus interactions for the bytecode-VM path.
            let (bus_interactions, arg_spans, bytecode) = timed_substage!("bus_compile_h2d", {
                let (bus_interactions, arg_spans, bytecode) = compile_bus_to_gpu(
                    &self.apc.machine.bus_interactions,
                    &apc_poly_id_to_index,
                    height,
                );
                (
                    bus_interactions.to_device().unwrap(),
                    arg_spans.to_device().unwrap(),
                    bytecode.to_device().unwrap(),
                )
            });

            timed_substage!("bus_kernel", {
                cuda_abi::apc_apply_bus(
                    &output,
                    num_apc_calls,
                    bytecode,
                    bus_interactions,
                    arg_spans,
                    var_range_bus_id,
                    var_range_count,
                    tuple2_bus_id,
                    tuple2_count_u32,
                    tuple2_sizes,
                    bitwise_bus_id,
                    bitwise_count_u32,
                )
                .unwrap();
            });
        }

        Some(output)
    }
}

impl<R, PB: ProverBackend<Matrix = DeviceMatrix<BabyBear>>, ISA: OpenVmISA> Chip<R, PB>
    for PowdrChipGpu<ISA>
{
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let trace = self
            .trace_generator
            .try_generate_witness(self.record_arena_by_air_name.take())
            .unwrap_or_else(DeviceMatrix::dummy);

        AirProvingContext {
            cached_mains: vec![],
            common_main: trace,
            public_values: vec![],
        }
    }
}

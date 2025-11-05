use crate::{powdr_extension::trace_generator::common::DummyExecutor, PeripheryBusIds};
use openvm_circuit::arch::{
    AirInventory, AirInventoryError, ChipInventory, ChipInventoryError, ExecutorInventoryBuilder,
    ExecutorInventoryError, VmCircuitExtension, VmExecutionExtension, VmProverExtension,
};
use openvm_circuit_primitives::{
    bitwise_op_lookup::{
        BitwiseOperationLookupAir, BitwiseOperationLookupBus, BitwiseOperationLookupChip,
    },
    range_tuple::{RangeTupleCheckerAir, RangeTupleCheckerBus, RangeTupleCheckerChip},
    var_range::VariableRangeCheckerAir,
};
use openvm_stark_backend::{config::StarkGenericConfig, p3_field::PrimeField32};

use crate::BabyBearSC;
use crate::BitwiseOperationLookupChipGPU;
use crate::DenseRecordArena;
use crate::GpuBabyBearPoseidon2Engine;
use crate::GpuBackend;
use crate::RangeTupleCheckerChipGPU;
use crate::VariableRangeCheckerChipGPU;
use openvm_circuit_primitives::var_range::VariableRangeCheckerChip;
use std::sync::Arc;

/// The shared chips which can be used by the PowdrChipGpu.
#[derive(Clone)]
pub struct PowdrPeripheryInstancesGpu {
    /// The real chips used for the main execution.
    pub real: SharedPeripheryChipsGpu,
    /// The dummy chips used for all APCs. They share the range checker but create new instances of the bitwise lookup chip and the tuple range checker.
    pub dummy: SharedPeripheryChipsGpu,
    /// The bus ids of the periphery
    pub bus_ids: PeripheryBusIds,
}

#[derive(Clone)]
pub struct SharedPeripheryChipsGpu {
    pub bitwise_lookup_8: Option<std::sync::Arc<BitwiseOperationLookupChipGPU<8>>>,
    pub range_checker: std::sync::Arc<VariableRangeCheckerChipGPU>,
    pub tuple_range_checker: Option<std::sync::Arc<RangeTupleCheckerChipGPU<2>>>,
}

impl PowdrPeripheryInstancesGpu {
    pub(crate) fn new(
        range_checker: Arc<VariableRangeCheckerChipGPU>,
        bitwise_8: Option<Arc<BitwiseOperationLookupChipGPU<8>>>,
        tuple_range_checker: Option<Arc<RangeTupleCheckerChipGPU<2>>>,
        bus_ids: PeripheryBusIds,
    ) -> Self {
        Self {
            real: SharedPeripheryChipsGpu {
                bitwise_lookup_8: bitwise_8.clone(),
                range_checker: range_checker.clone(),
                tuple_range_checker: tuple_range_checker.clone(),
            },
            dummy: SharedPeripheryChipsGpu {
                // BitwiseLookupChipGPU is always initialized via `hybrid()` with a CPU chip in all available extensions of `SdkVmGpuBuilder::create_chip_complex()`.
                // In case this changes in the future, `cpu_chip.unwrap()` will panic, and we can fix the code.
                bitwise_lookup_8: bitwise_8.map(|bitwise_8| {
                    Arc::new(BitwiseOperationLookupChipGPU::hybrid(Arc::new(
                        BitwiseOperationLookupChip::new(
                            bitwise_8.as_ref().cpu_chip.as_ref().unwrap().bus(),
                        ),
                    )))
                }),
                range_checker: range_checker.clone(),
                // RangeTupleCheckerGPU is always initialized via `new()` without a CPU chip in all available extensions of `SdkVmGpuBuilder::create_chip_complex()`.
                // In case this changes in the future the `Some` matching arm below will catch it.
                tuple_range_checker: tuple_range_checker.map(|tuple_range_checker| {
                    Arc::new({
                        match tuple_range_checker.cpu_chip.as_ref() {
                            // None is the expected case
                            None => RangeTupleCheckerChipGPU::new(tuple_range_checker.sizes),
                            Some(cpu_chip) => RangeTupleCheckerChipGPU::hybrid(Arc::new(
                                RangeTupleCheckerChip::new(*cpu_chip.bus()),
                            )),
                        }
                    })
                }),
            },
            bus_ids,
        }
    }
}

impl<F: PrimeField32> VmExecutionExtension<F> for SharedPeripheryChipsGpu {
    type Executor = DummyExecutor<F>;

    fn extend_execution(
        &self,
        _: &mut ExecutorInventoryBuilder<F, Self::Executor>,
    ) -> Result<(), ExecutorInventoryError> {
        // No executor to add for periphery chips
        Ok(())
    }
}

impl<SC: StarkGenericConfig> VmCircuitExtension<SC> for SharedPeripheryChipsGpu {
    // create dummy airs
    fn extend_circuit(&self, inventory: &mut AirInventory<SC>) -> Result<(), AirInventoryError> {
        // The bus id is for dummy shared periphery isn't guarantee to match that of the real periphery, because:
        // - Dummy periphery bus ids depend on chip insertion order of `create_dummy_chip_complex`.
        // - Real periphery bus ids depend on chip insertion order of `create_chip_complex`.
        // However, none of these matters because the dummy chips are thrown away anyway.
        if self.bitwise_lookup_8.is_some() {
            assert!(inventory
                .find_air::<BitwiseOperationLookupAir<8>>()
                .next()
                .is_none());
            let bus = BitwiseOperationLookupBus::new(inventory.new_bus_idx());
            inventory.add_air(BitwiseOperationLookupAir::<8> { bus });
        }

        if let Some(tuple_range_checker) = &self.tuple_range_checker {
            assert!(inventory
                .find_air::<RangeTupleCheckerAir<2>>()
                .next()
                .is_none());
            let bus = RangeTupleCheckerBus::new(inventory.new_bus_idx(), tuple_range_checker.sizes);
            inventory.add_air(RangeTupleCheckerAir::<2> { bus });
        }

        // The range checker is already present in the builder because it's is used by the system, so we don't add it again.
        assert!(inventory
            .find_air::<VariableRangeCheckerAir>()
            .nth(1)
            .is_none());

        Ok(())
    }
}

pub struct SharedPeripheryChipsGpuProverExt;

impl VmProverExtension<GpuBabyBearPoseidon2Engine, DenseRecordArena, SharedPeripheryChipsGpu>
    for SharedPeripheryChipsGpuProverExt
{
    fn extend_prover(
        &self,
        extension: &SharedPeripheryChipsGpu,
        inventory: &mut ChipInventory<BabyBearSC, DenseRecordArena, GpuBackend>,
    ) -> Result<(), ChipInventoryError> {
        // Sanity check that the shared chips are not already present in the builder.
        if let Some(bitwise_lookup_8) = &extension.bitwise_lookup_8 {
            assert!(inventory
                .find_chip::<Arc<BitwiseOperationLookupChipGPU<8>>>()
                .next()
                .is_none());
            inventory.add_periphery_chip(bitwise_lookup_8.clone());
        }

        if let Some(tuple_checker) = &extension.tuple_range_checker {
            assert!(inventory
                .find_chip::<Arc<RangeTupleCheckerChipGPU<2>>>()
                .next()
                .is_none());
            inventory.add_periphery_chip(tuple_checker.clone());
        }

        // The range checker is already present in the builder because it's is used by the system, so we don't add it again.
        assert!(inventory
            .find_chip::<Arc<VariableRangeCheckerChip>>()
            .nth(1)
            .is_none());

        Ok(())
    }
}

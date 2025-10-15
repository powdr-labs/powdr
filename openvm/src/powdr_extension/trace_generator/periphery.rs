use itertools::Itertools;
use openvm_circuit::arch::{
    AirInventory, AirInventoryError, ChipInventory, ChipInventoryError, ExecutorInventoryBuilder,
    ExecutorInventoryError, RowMajorMatrixArena, VmCircuitExtension, VmExecutionExtension,
    VmProverExtension,
};
use openvm_circuit_primitives::{
    bitwise_op_lookup::{
        BitwiseOperationLookupAir, BitwiseOperationLookupChip, SharedBitwiseOperationLookupChip,
    },
    range_tuple::{
        RangeTupleCheckerAir, RangeTupleCheckerBus, RangeTupleCheckerChip,
        SharedRangeTupleCheckerChip,
    },
    var_range::{
        SharedVariableRangeCheckerChip, VariableRangeCheckerAir, VariableRangeCheckerChip,
    },
};
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::PrimeField32,
    prover::cpu::{CpuBackend, CpuDevice},
};
use openvm_stark_sdk::engine::StarkEngine;

use crate::powdr_extension::trace_generator::inventory::DummyExecutor;
use std::sync::Arc;

cfg_if::cfg_if! {
    if #[cfg(feature = "cuda")] {
        use crate::BitwiseOperationLookupChipGPU;
        use crate::VariableRangeCheckerChipGPU;
        use crate::RangeTupleCheckerChipGPU;
        use crate::GpuBackend;
        use crate::DenseRecordArena;
        use crate::BabyBearSC;
        use crate::GpuBabyBearPoseidon2Engine;
    }
}

/// The shared chips which can be used by the PowdrChip.
#[derive(Clone)]
pub struct PowdrPeripheryInstances {
    /// The real chips used for the main execution.
    pub real: SharedPeripheryChips,
    /// The dummy chips used for all APCs. They share the range checker but create new instances of the bitwise lookup chip and the tuple range checker.
    pub dummy: SharedPeripheryChips,
}

#[cfg(feature = "cuda")]
#[derive(Clone)]
pub struct SharedPeripheryChips {
    pub bitwise_lookup_8: Option<std::sync::Arc<BitwiseOperationLookupChipGPU<8>>>,
    pub range_checker: std::sync::Arc<VariableRangeCheckerChipGPU>,
    pub tuple_range_checker: Option<std::sync::Arc<RangeTupleCheckerChipGPU<2>>>,
}

#[cfg(not(feature = "cuda"))]
#[derive(Clone)]
pub struct SharedPeripheryChips {
    pub bitwise_lookup_8: Option<SharedBitwiseOperationLookupChip<8>>,
    pub range_checker: SharedVariableRangeCheckerChip,
    pub tuple_range_checker: Option<SharedRangeTupleCheckerChip<2>>,
}

#[cfg(feature = "cuda")]
impl PowdrPeripheryInstances {
    pub(crate) fn new(
        range_checker: Arc<VariableRangeCheckerChipGPU>,
        bitwise_8: Option<Arc<BitwiseOperationLookupChipGPU<8>>>,
        tuple_range_checker: Option<Arc<RangeTupleCheckerChipGPU<2>>>,
    ) -> Self {
        Self {
            real: SharedPeripheryChips {
                bitwise_lookup_8: bitwise_8.clone(),
                range_checker: range_checker.clone(),
                tuple_range_checker: tuple_range_checker.clone(),
            },
            dummy: SharedPeripheryChips {
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
        }
    }
}

#[cfg(not(feature = "cuda"))]
impl PowdrPeripheryInstances {
    pub(crate) fn new(
        range_checker: SharedVariableRangeCheckerChip,
        bitwise_8: Option<SharedBitwiseOperationLookupChip<8>>,
        tuple_range_checker: Option<SharedRangeTupleCheckerChip<2>>,
    ) -> Self {
        Self {
            real: SharedPeripheryChips {
                bitwise_lookup_8: bitwise_8.clone(),
                range_checker: range_checker.clone(),
                tuple_range_checker: tuple_range_checker.clone(),
            },
            // Bitwise lookup and tuple range checker do not need to be shared with the main execution:
            // If we did share, we'd have to roll back the side effects of execution and apply the side effects from the apc air onto the main periphery.
            // By not sharing them, we can throw away the dummy ones after execution and only apply the side effects from the apc air onto the main periphery.
            dummy: SharedPeripheryChips {
                bitwise_lookup_8: bitwise_8.map(|bitwise_8| {
                    SharedBitwiseOperationLookupChip::new(BitwiseOperationLookupChip::new(
                        bitwise_8.bus(),
                    ))
                }),
                range_checker: range_checker.clone(),
                tuple_range_checker: tuple_range_checker.map(|tuple_range_checker| {
                    SharedRangeTupleCheckerChip::new(RangeTupleCheckerChip::new(
                        *tuple_range_checker.bus(),
                    ))
                }),
            },
        }
    }
}

impl<F: PrimeField32> VmExecutionExtension<F> for SharedPeripheryChips {
    type Executor = DummyExecutor<F>;

    fn extend_execution(
        &self,
        _: &mut ExecutorInventoryBuilder<F, Self::Executor>,
    ) -> Result<(), ExecutorInventoryError> {
        // No executor to add for periphery chips
        Ok(())
    }
}

#[cfg(feature = "cuda")]
impl<SC: StarkGenericConfig> VmCircuitExtension<SC> for SharedPeripheryChips {
    fn extend_circuit(&self, inventory: &mut AirInventory<SC>) -> Result<(), AirInventoryError> {
        // create dummy airs
        if let Some(bitwise_lookup_8) = &self.bitwise_lookup_8 {
            assert!(inventory
                .find_air::<BitwiseOperationLookupAir<8>>()
                .next()
                .is_none());
            inventory.add_air(BitwiseOperationLookupAir::<8>::new(
                bitwise_lookup_8.cpu_chip.as_ref().unwrap().bus(),
            ));
        }

        if let Some(tuple_range_checker) = &self.tuple_range_checker {
            assert!(inventory
                .find_air::<RangeTupleCheckerAir<2>>()
                .next()
                .is_none());
            // RangeTupleCheckerGPU is always initialized via `new()` without a CPU chip in all available extensions of `SdkVmGpuBuilder::create_chip_complex()`.
            // Therefore we create a new bus index, following a similar scenario in `Rv32M::extend_circuit`.
            let bus = match tuple_range_checker.cpu_chip.as_ref() {
                // None is the expected case
                None => {
                    RangeTupleCheckerBus::new(inventory.new_bus_idx(), tuple_range_checker.sizes)
                }
                Some(cpu_chip) => *cpu_chip.bus(),
            };
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

#[cfg(not(feature = "cuda"))]
impl<SC: StarkGenericConfig> VmCircuitExtension<SC> for SharedPeripheryChips {
    fn extend_circuit(&self, inventory: &mut AirInventory<SC>) -> Result<(), AirInventoryError> {
        // create dummy airs
        if let Some(bitwise_lookup_8) = &self.bitwise_lookup_8 {
            assert!(inventory
                .find_air::<BitwiseOperationLookupAir<8>>()
                .next()
                .is_none());
            inventory.add_air(BitwiseOperationLookupAir::<8>::new(
                bitwise_lookup_8.air.bus,
            ));
        }

        if let Some(tuple_range_checker) = &self.tuple_range_checker {
            assert!(inventory
                .find_air::<RangeTupleCheckerAir<2>>()
                .next()
                .is_none());
            inventory.add_air(RangeTupleCheckerAir::<2> {
                bus: tuple_range_checker.air.bus,
            });
        }

        // The range checker is already present in the builder because it's is used by the system, so we don't add it again.
        assert!(inventory
            .find_air::<VariableRangeCheckerAir>()
            .nth(1)
            .is_none());

        Ok(())
    }
}

#[cfg(feature = "cuda")]
pub struct SharedPeripheryChipsGpuProverExt;

#[cfg(feature = "cuda")]
impl VmProverExtension<GpuBabyBearPoseidon2Engine, DenseRecordArena, SharedPeripheryChips>
    for SharedPeripheryChipsGpuProverExt
{
    fn extend_prover(
        &self,
        extension: &SharedPeripheryChips,
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

#[cfg(not(feature = "cuda"))]
pub struct SharedPeripheryChipsCpuProverExt;

#[cfg(not(feature = "cuda"))]
// We implement an extension to make it easy to pre-load the shared chips into the VM inventory.
// This implementation is specific to CpuBackend because the lookup chips (VariableRangeChecker,
// BitwiseOperationLookupChip) are specific to CpuBackend.
impl<E, SC, RA> VmProverExtension<E, RA, SharedPeripheryChips> for SharedPeripheryChipsCpuProverExt
where
    SC: StarkGenericConfig,
    E: StarkEngine<SC = SC, PB = CpuBackend<SC>, PD = CpuDevice<SC>>,
    RA: RowMajorMatrixArena<Val<SC>>,
    Val<SC>: PrimeField32,
{
    fn extend_prover(
        &self,
        extension: &SharedPeripheryChips,
        inventory: &mut ChipInventory<SC, RA, CpuBackend<SC>>,
    ) -> Result<(), ChipInventoryError> {
        // Sanity check that the shared chips are not already present in the builder.
        if let Some(bitwise_lookup_8) = &extension.bitwise_lookup_8 {
            assert!(inventory
                .find_chip::<SharedBitwiseOperationLookupChip<8>>()
                .next()
                .is_none());
            inventory.add_periphery_chip(bitwise_lookup_8.clone());
        }

        if let Some(tuple_checker) = &extension.tuple_range_checker {
            assert!(inventory
                .find_chip::<SharedRangeTupleCheckerChip<2>>()
                .next()
                .is_none());
            inventory.add_periphery_chip(tuple_checker.clone());
        }

        // The range checker is already present in the builder because it's is used by the system, so we don't add it again.
        assert!(inventory
            .find_chip::<SharedVariableRangeCheckerChip>()
            .nth(1)
            .is_none());

        Ok(())
    }
}

// Note: `apply` implementations are specific to GPU/CPU, so we can't consolidate them
#[cfg(feature = "cuda")]
impl SharedPeripheryChips {
    /// Sends concrete values to the shared chips using a given bus id.
    /// Panics if the bus id doesn't match any of the chips' bus ids.
    pub fn apply(&self, bus_id: u16, mult: u32, mut args: impl Iterator<Item = u32>) {
        match bus_id {
            // TODO: here we assume all GPU chips has a `Some(CPUChip)` field, so it might panic on `unwrap()`.
            // Don't really see a reason why it would be `None` though.
            id if Some(id)
                == self
                    .bitwise_lookup_8
                    .as_ref()
                    .map(|c| c.cpu_chip.as_ref().unwrap().bus().inner.index) =>
            {
                // bitwise operation lookup
                // interpret the arguments, see `Air<AB> for BitwiseOperationLookupAir<NUM_BITS>`
                let [x, y, x_xor_y, selector] = [
                    args.next().unwrap(),
                    args.next().unwrap(),
                    args.next().unwrap(),
                    args.next().unwrap(),
                ];

                for _ in 0..mult {
                    match selector {
                        0 => {
                            self.bitwise_lookup_8
                                .as_ref()
                                .unwrap()
                                .cpu_chip
                                .as_ref()
                                .unwrap()
                                .request_range(x, y);
                        }
                        1 => {
                            let res = self
                                .bitwise_lookup_8
                                .as_ref()
                                .unwrap()
                                .cpu_chip
                                .as_ref()
                                .unwrap()
                                .request_xor(x, y);
                            debug_assert_eq!(res, x_xor_y);
                        }
                        _ => {
                            unreachable!("Invalid selector");
                        }
                    }
                }
            }
            id if id == self.range_checker.cpu_chip.as_ref().unwrap().bus().index() => {
                // interpret the arguments, see `Air<AB> for VariableRangeCheckerAir`
                let [value, max_bits] = [args.next().unwrap(), args.next().unwrap()];

                for _ in 0..mult {
                    self.range_checker
                        .cpu_chip
                        .as_ref()
                        .unwrap()
                        .add_count(value, max_bits as usize);
                }
            }
            id if Some(id)
                == self
                    .tuple_range_checker
                    .as_ref()
                    .map(|c| c.cpu_chip.as_ref().unwrap().bus().inner.index) =>
            {
                // tuple range checker
                // We pass a slice. It is checked inside `add_count`.
                let args = args.collect_vec();
                for _ in 0..mult {
                    self.tuple_range_checker
                        .as_ref()
                        .unwrap()
                        .cpu_chip
                        .as_ref()
                        .unwrap()
                        .add_count(&args);
                }
            }
            0..=2 => {
                // execution bridge, memory, pc lookup
                // do nothing
            }
            _ => {
                unreachable!("Bus interaction {} not implemented", bus_id);
            }
        }
    }
}

#[cfg(not(feature = "cuda"))]
impl SharedPeripheryChips {
    /// Sends concrete values to the shared chips using a given bus id.
    /// Panics if the bus id doesn't match any of the chips' bus ids.
    pub fn apply(&self, bus_id: u16, mult: u32, mut args: impl Iterator<Item = u32>) {
        match bus_id {
            id if Some(id) == self.bitwise_lookup_8.as_ref().map(|c| c.bus().inner.index) => {
                // bitwise operation lookup
                // interpret the arguments, see `Air<AB> for BitwiseOperationLookupAir<NUM_BITS>`
                let [x, y, x_xor_y, selector] = [
                    args.next().unwrap(),
                    args.next().unwrap(),
                    args.next().unwrap(),
                    args.next().unwrap(),
                ];

                for _ in 0..mult {
                    match selector {
                        0 => {
                            self.bitwise_lookup_8.as_ref().unwrap().request_range(x, y);
                        }
                        1 => {
                            let res = self.bitwise_lookup_8.as_ref().unwrap().request_xor(x, y);
                            debug_assert_eq!(res, x_xor_y);
                        }
                        _ => {
                            unreachable!("Invalid selector");
                        }
                    }
                }
            }
            id if id == self.range_checker.bus().index() => {
                // interpret the arguments, see `Air<AB> for VariableRangeCheckerAir`
                let [value, max_bits] = [args.next().unwrap(), args.next().unwrap()];

                for _ in 0..mult {
                    self.range_checker.add_count(value, max_bits as usize);
                }
            }
            id if Some(id)
                == self
                    .tuple_range_checker
                    .as_ref()
                    .map(|c| c.bus().inner.index) =>
            {
                // tuple range checker
                // We pass a slice. It is checked inside `add_count`.
                let args = args.collect_vec();
                for _ in 0..mult {
                    self.tuple_range_checker.as_ref().unwrap().add_count(&args);
                }
            }
            0..=2 => {
                // execution bridge, memory, pc lookup
                // do nothing
            }
            _ => {
                unreachable!("Bus interaction {} not implemented", bus_id);
            }
        }
    }
}

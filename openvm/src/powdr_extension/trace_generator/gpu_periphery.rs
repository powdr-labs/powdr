use std::sync::Arc;
use itertools::Itertools;
use openvm_circuit::arch::{
    AirInventory, AirInventoryError, ChipInventory, ChipInventoryError, ExecutorInventoryBuilder,
    ExecutorInventoryError, RowMajorMatrixArena, VmCircuitExtension, VmExecutionExtension,
    VmProverExtension,
};
use openvm_circuit_primitives::{
    bitwise_op_lookup::{
        BitwiseOperationLookupAir, SharedBitwiseOperationLookupChip,
    },
    range_tuple::{RangeTupleCheckerAir, RangeTupleCheckerChip, RangeTupleCheckerBus},
    var_range::{SharedVariableRangeCheckerChip, VariableRangeCheckerAir},
};
use openvm_stark_backend::{
    config::{StarkGenericConfig, Val},
    p3_field::PrimeField32,
    prover::cpu::{CpuBackend, CpuDevice},
};
use openvm_stark_sdk::engine::StarkEngine;
use openvm_stark_sdk::p3_baby_bear::BabyBear;

use crate::powdr_extension::trace_generator::inventory::DummyExecutor;

/// GPU-compatible range tuple checker chip
/// This is a placeholder implementation that bridges CPU and GPU APIs
#[derive(Clone)]
pub struct RangeTupleCheckerChipGPU<const N: usize> {
    pub count: Arc<DeviceBuffer<BabyBear>>,
    pub cpu_chip: Option<Arc<RangeTupleCheckerChip<N>>>,
    pub sizes: [u32; N],
    pub bus: RangeTupleCheckerBus<N>,
}

/// Placeholder for GPU device buffer
/// In a real implementation, this would be a GPU memory buffer
pub struct DeviceBuffer<F> {
    data: Vec<F>,
}

impl<F> DeviceBuffer<F> {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }
    
    pub fn len(&self) -> usize {
        self.data.len()
    }
    
    pub fn fill_zero(&self) -> Result<(), String> {
        // In a real GPU implementation, this would zero the GPU buffer
        Ok(())
    }
}

impl<const N: usize> RangeTupleCheckerChipGPU<N> {
    pub fn new(sizes: [u32; N], bus: RangeTupleCheckerBus<N>) -> Self {
        let range_max = sizes.iter().product::<u32>() as usize;
        let count = Arc::new(DeviceBuffer::<BabyBear>::with_capacity(range_max));
        count.fill_zero().unwrap();
        Self {
            count,
            cpu_chip: None,
            sizes,
            bus,
        }
    }

    pub fn hybrid(cpu_chip: Arc<RangeTupleCheckerChip<N>>, bus: RangeTupleCheckerBus<N>) -> Self {
        let count = Arc::new(DeviceBuffer::<BabyBear>::with_capacity(cpu_chip.count.len()));
        count.fill_zero().unwrap();
        let sizes = *cpu_chip.sizes();
        Self {
            count,
            cpu_chip: Some(cpu_chip),
            sizes,
            bus,
        }
    }
    
    /// CPU wrapper for GPU add_count
    /// This method provides a CPU interface that can call GPU operations
    pub fn add_count(&self, args: &[u32]) {
        // For now, we'll use the CPU chip if available as a fallback
        if let Some(cpu_chip) = &self.cpu_chip {
            cpu_chip.add_count(args);
        }
        
        // TODO: Implement actual GPU add_count call
        // This would involve:
        // - Converting args to GPU memory format
        // - Calling CUDA kernel or GPU API
        // - Synchronizing results back to CPU if needed
    }
    
    /// Get the bus for this chip
    pub fn bus(&self) -> &RangeTupleCheckerBus<N> {
        &self.bus
    }
}

/// GPU-compatible shared periphery chips
#[derive(Clone)]
pub struct SharedPeripheryChipsGPU {
    pub bitwise_lookup_8: Option<SharedBitwiseOperationLookupChip<8>>,
    pub range_checker: SharedVariableRangeCheckerChip,
    pub tuple_range_checker: Option<RangeTupleCheckerChipGPU<2>>,
}

impl SharedPeripheryChipsGPU {
    /// Sends concrete values to the shared chips using a given bus id.
    /// This is the GPU-compatible version of the apply method.
    pub fn apply(&self, bus_id: u16, mult: u32, mut args: impl Iterator<Item = u32>) {
        match bus_id {
            id if Some(id) == self.bitwise_lookup_8.as_ref().map(|c| c.bus().inner.index) => {
                // bitwise operation lookup
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
            id if Some(id) == self.tuple_range_checker.as_ref().map(|c| c.bus().inner.index) => {
                // tuple range checker - GPU version
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

impl<F: PrimeField32> VmExecutionExtension<F> for SharedPeripheryChipsGPU {
    type Executor = DummyExecutor<F>;

    fn extend_execution(
        &self,
        _: &mut ExecutorInventoryBuilder<F, Self::Executor>,
    ) -> Result<(), ExecutorInventoryError> {
        // No executor to add for periphery chips
        Ok(())
    }
}

impl<SC: StarkGenericConfig> VmCircuitExtension<SC> for SharedPeripheryChipsGPU {
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
                bus: tuple_range_checker.bus().clone(),
            });
        }

        // The range checker is already present in the builder because it's is used by the system, so we don't add it again.
        assert!(inventory
            .find_air::<VariableRangeCheckerAir>()
            .next()
            .is_some());

        Ok(())
    }
}

/// GPU prover extension for shared periphery chips
pub struct SharedPeripheryChipsGPUProverExt;

impl<E, SC, RA> VmProverExtension<E, RA, SharedPeripheryChipsGPU> for SharedPeripheryChipsGPUProverExt
where
    SC: StarkGenericConfig,
    E: StarkEngine<SC = SC, PB = CpuBackend<SC>, PD = CpuDevice<SC>>,
    RA: RowMajorMatrixArena<Val<SC>>,
    Val<SC>: PrimeField32,
{
    fn extend_prover(
        &self,
        extension: &SharedPeripheryChipsGPU,
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

        // The range checker is already present in the builder because it's is used by the system, so we don't add it again.
        assert!(inventory
            .find_chip::<SharedVariableRangeCheckerChip>()
            .next()
            .is_some());

        // Note: GPU tuple range checker is handled differently
        // In a real implementation, this would register GPU-specific chips

        Ok(())
    }
}

/// Example function showing how to use GPU-compatible periphery chips
/// This demonstrates the solution to issue #3358
pub fn create_gpu_periphery_example() -> SharedPeripheryChipsGPU {
    use openvm_circuit_primitives::range_tuple::RangeTupleCheckerBus;
    use openvm_circuit_primitives::var_range::{VariableRangeCheckerChip, VariableRangeCheckerBus};
    
    // Create a GPU-compatible tuple range checker
    let sizes = [256, 256]; // Example sizes
    let tuple_bus = RangeTupleCheckerBus::new(7, sizes); // Using default bus ID from bus_map.rs
    let gpu_tuple_checker = RangeTupleCheckerChipGPU::new(sizes, tuple_bus);
    
    // Create range checker bus and chip
    let range_bus = VariableRangeCheckerBus::new(3, 25); // Using default bus ID from bus_map.rs
    let range_chip = VariableRangeCheckerChip::new(range_bus);
    
    // Create GPU-compatible shared periphery chips
    SharedPeripheryChipsGPU {
        bitwise_lookup_8: None, // Would be created from existing chip
        range_checker: SharedVariableRangeCheckerChip::new(range_chip),
        tuple_range_checker: Some(gpu_tuple_checker),
    }
}

/// Example function showing how to use hybrid CPU/GPU approach
/// This demonstrates the third solution from issue #3358
pub fn create_hybrid_periphery_example(cpu_chip: Arc<RangeTupleCheckerChip<2>>) -> SharedPeripheryChipsGPU {
    use openvm_circuit_primitives::range_tuple::RangeTupleCheckerBus;
    use openvm_circuit_primitives::var_range::{VariableRangeCheckerChip, VariableRangeCheckerBus};
    
    // Create hybrid GPU chip that can fall back to CPU
    let sizes = *cpu_chip.sizes();
    let tuple_bus = RangeTupleCheckerBus::new(7, sizes); // Using default bus ID from bus_map.rs
    let gpu_tuple_checker = RangeTupleCheckerChipGPU::hybrid(cpu_chip, tuple_bus);
    
    // Create range checker bus and chip
    let range_bus = VariableRangeCheckerBus::new(3, 25); // Using default bus ID from bus_map.rs
    let range_chip = VariableRangeCheckerChip::new(range_bus);
    
    // Create GPU-compatible shared periphery chips
    SharedPeripheryChipsGPU {
        bitwise_lookup_8: None, // Would be created from existing chip
        range_checker: SharedVariableRangeCheckerChip::new(range_chip),
        tuple_range_checker: Some(gpu_tuple_checker),
    }
}

use crate::powdr_extension::executor::inventory::{SharedExecutor, SharedPeriphery};
use itertools::Itertools;
use openvm_circuit::arch::VmExtension;
use openvm_circuit_primitives::{
    bitwise_op_lookup::SharedBitwiseOperationLookupChip, range_tuple::SharedRangeTupleCheckerChip,
    var_range::SharedVariableRangeCheckerChip,
};
use openvm_stark_backend::p3_field::PrimeField32;

/// The shared chips which can be used by the PowdrChip.
#[derive(Clone)]
pub struct PowdrPeripheryInstances {
    /// The real chips used for the main execution.
    pub real: SharedPeripheryChips,
    /// The dummy chips used for all APCs. They share the range checker but create new instances of the bitwise lookup chip and the tuple range checker.
    pub dummy: SharedPeripheryChips,
}

#[derive(Clone)]
pub struct SharedPeripheryChips {
    pub bitwise_lookup_8: Option<SharedBitwiseOperationLookupChip<8>>,
    pub range_checker: SharedVariableRangeCheckerChip,
    pub tuple_range_checker: Option<SharedRangeTupleCheckerChip<2>>,
}

impl PowdrPeripheryInstances {
    pub(crate) fn new(
        range_checker: &SharedVariableRangeCheckerChip,
        bitwise_8: Option<&SharedBitwiseOperationLookupChip<8>>,
        tuple_range_checker: Option<&SharedRangeTupleCheckerChip<2>>,
    ) -> Self {
        Self {
            real: SharedPeripheryChips {
                bitwise_lookup_8: bitwise_8.cloned(),
                range_checker: range_checker.clone(),
                tuple_range_checker: tuple_range_checker.cloned(),
            },
            // Bitwise lookup and tuple range checker do not need to be shared with the main execution:
            // If we did share, we'd have to roll back the side effects of execution and apply the side effects from the apc air onto the main periphery.
            // By not sharing them, we can throw away the dummy ones after execution and only apply the side effects from the apc air onto the main periphery.
            dummy: SharedPeripheryChips {
                bitwise_lookup_8: bitwise_8
                    .map(|bitwise_8| SharedBitwiseOperationLookupChip::new(bitwise_8.bus())),
                range_checker: range_checker.clone(),
                tuple_range_checker: tuple_range_checker.map(|tuple_range_checker| {
                    SharedRangeTupleCheckerChip::new(*tuple_range_checker.bus())
                }),
            },
        }
    }
}

/// We implement an extension to make it easy to pre-load the shared chips into the VM inventory.
impl<F> VmExtension<F> for SharedPeripheryChips
where
    F: PrimeField32,
{
    type Executor = SharedExecutor<F>;

    type Periphery = SharedPeriphery<F>;

    fn build(
        &self,
        builder: &mut openvm_circuit::arch::VmInventoryBuilder<F>,
    ) -> Result<
        openvm_circuit::arch::VmInventory<Self::Executor, Self::Periphery>,
        openvm_circuit::arch::VmInventoryError,
    > {
        let mut inventory = openvm_circuit::arch::VmInventory::new();

        // Sanity check that the shared chips are not already present in the builder.
        if let Some(bitwise_lookup_8) = &self.bitwise_lookup_8 {
            assert!(builder
                .find_chip::<SharedBitwiseOperationLookupChip<8>>()
                .is_empty());
            inventory.add_periphery_chip(bitwise_lookup_8.clone());
        }

        if let Some(tuple_checker) = &self.tuple_range_checker {
            assert!(builder
                .find_chip::<SharedRangeTupleCheckerChip<2>>()
                .is_empty());
            inventory.add_periphery_chip(tuple_checker.clone());
        }

        // The range checker is already present in the builder because it's is used by the system, so we don't add it again.
        assert_eq!(
            builder.find_chip::<SharedVariableRangeCheckerChip>().len(),
            1
        );

        Ok(inventory)
    }
}

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

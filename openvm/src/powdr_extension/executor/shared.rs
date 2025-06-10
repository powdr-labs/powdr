use crate::powdr_extension::executor::inventory::{SharedExecutor, SharedPeriphery};
use itertools::Itertools;
use openvm_circuit::arch::VmExtension;
use openvm_circuit_primitives::{
    bitwise_op_lookup::{BitwiseOperationLookupBus, SharedBitwiseOperationLookupChip},
    range_tuple::{RangeTupleCheckerBus, SharedRangeTupleCheckerChip},
    var_range::SharedVariableRangeCheckerChip,
};
use openvm_stark_backend::p3_field::PrimeField32;

/// The shared chips which can be used by the PowdrChip.
#[derive(Clone)]
pub struct SharedChips {
    pub bitwise_lookup_8: SharedBitwiseOperationLookupChip<8>,
    pub range_checker: SharedVariableRangeCheckerChip,
    pub tuple_range_checker: Option<SharedRangeTupleCheckerChip<2>>,
}

impl<F> VmExtension<F> for &SharedChips
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
        assert!(builder
            .find_chip::<SharedBitwiseOperationLookupChip<8>>()
            .is_empty());
        inventory.add_periphery_chip(self.bitwise_lookup_8.clone());

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

impl SharedChips {
    /// Creates a new instance of `SharedChips` with a range checker.
    /// The other chips are instanciated here based on the busses to be shared by all APCs.
    /// The reasoning is that the range checker is used by memory so we need to use the same instance as the main execution
    /// The other chips are only used in the APCs and then thrown away, so it's fine to create them here.
    // TODO: Do we really need the same instance of the range checker?
    pub(crate) fn new(
        range_checker: SharedVariableRangeCheckerChip,
        bitwise_bus: BitwiseOperationLookupBus,
        range_tuple_checker_bus: Option<RangeTupleCheckerBus<2>>,
    ) -> SharedChips {
        SharedChips {
            bitwise_lookup_8: SharedBitwiseOperationLookupChip::new(bitwise_bus),
            range_checker,
            tuple_range_checker: range_tuple_checker_bus.map(SharedRangeTupleCheckerChip::new),
        }
    }
}

impl SharedChips {
    /// Sends concrete values to the shared chips using a given bus id.
    /// Panics if the bus id doesn't match any of the chips' bus ids.
    pub fn apply(&self, bus_id: u16, mult: u32, mut args: impl Iterator<Item = u32>) {
        match bus_id {
            id if id == self.bitwise_lookup_8.bus().inner.index => {
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
                            self.bitwise_lookup_8.request_range(x, y);
                        }
                        1 => {
                            let res = self.bitwise_lookup_8.request_xor(x, y);
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

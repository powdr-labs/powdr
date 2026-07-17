use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;

use itertools::Itertools;
use powdr_constraint_solver::constraint_system::{
    AlgebraicConstraint, BusInteraction, ConstraintSystem,
};
use powdr_constraint_solver::grouped_expression::GroupedExpression;
use powdr_constraint_solver::solver::Solver;
use powdr_number::FieldElement;

use crate::symbolic_machine::MemoryDropKind;

/// Optimizes bus sends that correspond to general-purpose memory read and write operations.
/// It works best if all read-write-operation addresses are fixed offsets relative to some
/// symbolic base address. If stack and heap access operations are mixed, this is usually violated.
pub fn optimize_memory<
    T: FieldElement,
    V: Hash + Eq + Clone + Ord + Display,
    M: MemoryBusInteraction<T, V>,
>(
    mut system: ConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    memory_bus_id: Option<u64>,
) -> ConstraintSystem<T, V> {
    // In the absence of memory bus, we return the system unchanged
    let memory_bus_id = match memory_bus_id {
        Some(id) => id,
        None => {
            return system;
        }
    };

    // TODO use the solver here.
    let (to_remove, new_constraints) =
        redundant_memory_interactions_indices::<T, V, M>(&system, solver, memory_bus_id);
    let to_remove = to_remove.into_iter().collect::<HashSet<_>>();
    system.bus_interactions = system
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus)| (!to_remove.contains(&i)).then_some(bus))
        .collect();
    solver.add_algebraic_constraints(new_constraints.iter().cloned());
    // TODO perform substitutions instead
    system.algebraic_constraints.extend(new_constraints);

    system
}

/// A decoded liveness hint used to match a dropped memory slot.
struct MemoryDropMatcher<T, V> {
    kind: MemoryDropKind,
    address_space: GroupedExpression<T, V>,
    address: GroupedExpression<T, V>,
    timestamp: GroupedExpression<T, V>,
    /// Concrete fp-relative offset of the hint's base slot (`address = fp +
    /// fp_offset`), if recorded at lowering time. Drops whose hints lack it are
    /// never applied: the runtime could not identify the slot to elide.
    fp_offset: Option<u64>,
}

/// A memory slot removed from the machine by
/// [`drop_internal_memory_accesses`], identified in runtime terms so the APC
/// executor can elide the slot's accesses from the offline memory argument
/// (the AIR no longer carries its boundary bus interactions).
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct DroppedMemorySlot {
    /// The address space of the slot.
    pub address_space: u32,
    /// The slot's address relative to the frame pointer at block entry
    /// (`address = fp + fp_offset`).
    pub fp_offset: u64,
}

/// Removes the memory access for slots whose access is *fully internal* to the
/// circuit, driven by the liveness hints carried in `system.hints`.
///
/// A slot's access is fully internal when:
/// 1. the previous value read at the slot is used nowhere else in the system (so
///    the value present before the block does not matter), and
/// 2. the slot's last value is dropped by a hint whose timestamp is at or after
///    the surviving write (so the value present after the block escapes nowhere).
///
/// For such a slot both surviving memory bus interactions -- the boundary
/// `GetPrevious` and `SetNew` -- are removed. No constraints are added: the read
/// value becomes unreferenced and the written value dead, both pruned by the
/// later disconnected-column removal.
///
/// This must run *after* [`optimize_memory`] has paired up the internal accesses,
/// so that each live slot is reduced to a single `GetPrevious`/`SetNew` boundary
/// pair. Slots left in any other shape (e.g. where address disambiguation
/// prevented pairing) are conservatively skipped, never compromising soundness.
/// Everything is keyed per address space. Redundant or unmatched hints are
/// ignored.
///
/// Returns the modified system together with the list of slots whose boundary
/// pair was removed. The caller must ensure the APC executor elides exactly
/// these slots' accesses from the memory argument at runtime, or the memory
/// bus will not balance.
pub fn drop_internal_memory_accesses<
    T: FieldElement,
    V: Hash + Eq + Clone + Ord + Display,
    M: MemoryBusInteraction<T, V>,
>(
    mut system: ConstraintSystem<T, V>,
    memory_bus_id: Option<u64>,
) -> (ConstraintSystem<T, V>, Vec<DroppedMemorySlot>) {
    let Some(memory_bus_id) = memory_bus_id else {
        return (system, Vec::new());
    };

    // Decode the drop hints. Each must carry the three argument expressions
    // `(address_space, address, timestamp)`, optionally followed by a constant
    // fp-relative offset. Malformed hints panic.
    let drops = system
        .hints
        .iter()
        .filter_map(|hint| {
            let kind = MemoryDropKind::try_from(hint.kind).ok()?;
            let (exprs, fp_offset) = match &hint.args[..] {
                [address_space, address, timestamp] => ([address_space, address, timestamp], None),
                [address_space, address, timestamp, offset] => (
                    [address_space, address, timestamp],
                    Some(
                        offset
                            .try_to_number()
                            .expect("Memory drop hint fp_offset must be a constant")
                            .to_arbitrary_integer()
                            .try_into()
                            .expect("Memory drop hint fp_offset must fit in u64"),
                    ),
                ),
                args => panic!(
                    "Malformed memory drop hint: expected 3 or 4 arguments, got {}",
                    args.len()
                ),
            };
            let [address_space, address, timestamp] = exprs;
            Some(MemoryDropMatcher {
                kind,
                address_space: address_space.clone(),
                address: address.clone(),
                timestamp: timestamp.clone(),
                fp_offset,
            })
        })
        .collect::<Vec<_>>();
    if drops.is_empty() {
        return (system, Vec::new());
    }

    // Count, per variable, in how many distinct items (algebraic constraints and
    // bus interactions) it occurs. A `GetPrevious`'s read value is "used nowhere
    // else" exactly when each of its variables occurs in a single item: that very
    // interaction.
    let mut usage: HashMap<V, usize> = HashMap::new();
    for constraint in &system.algebraic_constraints {
        for var in constraint.referenced_unknown_variables().unique() {
            *usage.entry(var.clone()).or_default() += 1;
        }
    }
    for bus_interaction in &system.bus_interactions {
        for var in bus_interaction.referenced_unknown_variables().unique() {
            *usage.entry(var.clone()).or_default() += 1;
        }
    }

    // Group the memory bus interactions by address. Non-memory interactions are
    // ignored; interactions with unknown multiplicity simply leave their slot
    // ungrouped, so it is skipped below.
    let mut slots: HashMap<Address<T, V>, Vec<(usize, M)>> = HashMap::new();
    for (index, bus_interaction) in system.bus_interactions.iter().enumerate() {
        if let Ok(Some(mem_int)) = M::try_from_bus_interaction(bus_interaction, memory_bus_id) {
            let addr = Address::from(mem_int.addr());
            slots.entry(addr).or_default().push((index, mem_int));
        }
    }

    let mut to_remove: HashSet<usize> = HashSet::new();
    let mut dropped_slots: Vec<DroppedMemorySlot> = Vec::new();
    // Iteration order is irrelevant: we only accumulate into the `to_remove` set
    // (and `dropped_slots`, which is sorted below).
    #[allow(clippy::iter_over_hash_type)]
    for (addr, interactions) in &slots {
        // We only act on the clean boundary shape: exactly one `GetPrevious` and
        // one `SetNew`. Anything else is skipped conservatively.
        let (Ok(get_previous), Ok(set_new)) = (
            interactions
                .iter()
                .filter(|(_, m)| matches!(m.op(), MemoryOp::GetPrevious))
                .exactly_one(),
            interactions
                .iter()
                .filter(|(_, m)| matches!(m.op(), MemoryOp::SetNew))
                .exactly_one(),
        ) else {
            continue;
        };
        let (get_previous_index, get_previous) = (get_previous.0, &get_previous.1);
        let (set_new_index, set_new) = (set_new.0, &set_new.1);

        // Condition 2: the slot's last value is dropped, matched by address (per
        // address space) and by a timestamp at or after the surviving write. The
        // match also yields the slot's runtime identity (fp-relative offset).
        let Some(dropped_slot) = slot_is_dropped(addr, set_new.timestamp_limbs(), &drops) else {
            continue;
        };

        // Condition 1: the previous value is used nowhere else.
        let previous_value_used = get_previous
            .data()
            .iter()
            .flat_map(|expr| expr.referenced_unknown_variables())
            .any(|var| usage.get(var).copied().unwrap_or(0) > 1);
        if previous_value_used {
            continue;
        }

        to_remove.insert(get_previous_index);
        to_remove.insert(set_new_index);
        dropped_slots.push(dropped_slot);
    }

    log::debug!(
        "Dropping {} fully-internal memory interactions",
        to_remove.len()
    );

    system.bus_interactions = system
        .bus_interactions
        .into_iter()
        .enumerate()
        .filter_map(|(i, bus)| (!to_remove.contains(&i)).then_some(bus))
        .collect();

    // Deterministic order (the loop above iterates a HashMap).
    dropped_slots.sort_unstable_by_key(|slot| (slot.address_space, slot.fp_offset));

    (system, dropped_slots)
}

/// Returns whether `slot_addr`'s last value is dropped by one of the `drops`.
/// The drop must match the slot's address space and address (exactly, or
/// `>=` for [`MemoryDropKind::MemorySlotDropFrom`]), and its timestamp must be at
/// or after `set_new_timestamp` so that it refers to the surviving (last) write
/// rather than to an earlier, since-overwritten value.
/// On a match, returns the slot's runtime identity as a [`DroppedMemorySlot`];
/// slots matched only by hints without a recorded `fp_offset` are not dropped
/// (the runtime could not identify them for elision).
fn slot_is_dropped<T: FieldElement, V: Clone + Ord + Eq + Hash>(
    slot_addr: &Address<T, V>,
    set_new_timestamp: &[GroupedExpression<T, V>],
    drops: &[MemoryDropMatcher<T, V>],
) -> Option<DroppedMemorySlot> {
    // We expect a two-component address `[address_space, address]` and a single
    // timestamp limb; anything else is skipped conservatively.
    let [slot_address_space, slot_address] = &slot_addr.0[..] else {
        return None;
    };
    let [set_new_timestamp] = set_new_timestamp else {
        return None;
    };
    let address_space: u32 = slot_address_space
        .try_to_number()?
        .to_arbitrary_integer()
        .try_into()
        .ok()?;
    drops.iter().find_map(|drop| {
        if &drop.address_space != slot_address_space {
            return None;
        }
        let base_offset = drop.fp_offset?;
        let fp_offset = match drop.kind {
            MemoryDropKind::MemorySlotDrop => {
                (&drop.address == slot_address).then_some(base_offset)?
            }
            // `slot_address >= drop.address`, i.e. their difference is a known
            // non-negative constant (same base, so the base cancels). The
            // slot's own offset is the hint's base offset plus that difference.
            MemoryDropKind::MemorySlotDropFrom => {
                let delta = (slot_address.clone() - drop.address.clone()).try_to_number()?;
                if !delta.is_in_lower_half() {
                    return None;
                }
                base_offset
                    + u64::try_from(delta.to_arbitrary_integer())
                        .expect("non-negative address delta must fit in u64")
            }
        };
        // The drop must refer to the surviving (last) write: `drop.timestamp >=
        // set_new_timestamp`. This rejects a drop for an earlier value that was
        // later overwritten by a live write.
        is_known_non_negative(&(drop.timestamp.clone() - set_new_timestamp.clone())).then_some(
            DroppedMemorySlot {
                address_space,
                fp_offset,
            },
        )
    })
}

/// Whether `expr` is a known constant that is non-negative when interpreted as a
/// signed field value (i.e. it lies in the lower half of the field). Returns
/// `false` (conservative) if `expr` is not a known constant.
fn is_known_non_negative<T: FieldElement, V: Clone + Ord + Eq + Hash>(
    expr: &GroupedExpression<T, V>,
) -> bool {
    expr.try_to_number()
        .is_some_and(|value| value.is_in_lower_half())
}

#[derive(Debug, Copy, Clone)]
/// The type of the memory bus interaction.
pub enum MemoryOp {
    /// Get the previous value from memory.
    GetPrevious,
    /// Set the new value in memory.
    SetNew,
}

/// A recoverable error when trying to convert a bus interaction to a memory bus interaction.
/// For example, it might be that we don't know the bus ID or multiplicity yet.
pub struct MemoryBusInteractionConversionError;

/// A bus interaction that corresponds to half of a memory operation,
/// i.e. either a "get previous" or a "set new" operation.
/// Note that the order of memory bus interactions as they appear in the constraint system
/// is assumed to be chronological.
pub trait MemoryBusInteraction<T, V>: Sized {
    /// The address type of the memory bus interaction.
    /// We assume that it can be represented as a list of expressions of a *static* size, i.e.,
    /// `addr.into_iter().count()` should always return the same value.
    /// If there are different memories (e.g. register memory and heap memory), this type can be
    /// a composite address.
    type Address: IntoIterator<Item = GroupedExpression<T, V>>;

    /// Tries to convert a `BusInteraction` to a `MemoryBusInteraction`.
    ///
    /// Returns `Ok(None)` if we know that the bus interaction is not a memory bus interaction.
    /// Returns `Err(_)` if the bus interaction is a memory bus interaction but could not be converted properly
    /// (usually because the multiplicity is not -1 or 1).
    /// Otherwise returns `Ok(Some(memory_bus_interaction))`
    fn try_from_bus_interaction(
        bus_interaction: &BusInteraction<GroupedExpression<T, V>>,
        memory_bus_id: u64,
    ) -> Result<Option<Self>, MemoryBusInteractionConversionError>;

    /// Returns the address of the memory bus interaction.
    fn addr(&self) -> Self::Address;

    /// Returns the data part of the memory bus interaction.
    fn data(&self) -> &[GroupedExpression<T, V>];

    /// Returns the timestamp part of the memory bus interaction.
    fn timestamp_limbs(&self) -> &[GroupedExpression<T, V>];

    /// Returns the operation of the memory bus interaction.
    fn op(&self) -> MemoryOp;
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
/// A memory address, represented as a list of expressions.
/// By converting from `MemoryBusInteraction::Address` to `Address<T, V>`,
/// we can make sure that its `Eq` implementation is the expected one: Two addresses
/// are equal if all their parts are equal.
struct Address<T, V>(Vec<GroupedExpression<T, V>>);

impl<I, T, V> From<I> for Address<T, V>
where
    I: IntoIterator<Item = GroupedExpression<T, V>>,
{
    fn from(exprs: I) -> Self {
        Self(exprs.into_iter().collect())
    }
}

struct MemoryContent<T, V> {
    bus_index: usize,
    data: Vec<GroupedExpression<T, V>>,
    timestamp_limbs: Vec<GroupedExpression<T, V>>,
}

impl<T: Clone, V: Clone> MemoryContent<T, V> {
    fn from_bus_interaction<M: MemoryBusInteraction<T, V>>(bus_index: usize, mem_int: M) -> Self {
        Self {
            bus_index,
            data: mem_int.data().to_vec(),
            timestamp_limbs: mem_int.timestamp_limbs().to_vec(),
        }
    }
}

/// Tries to find indices of bus interactions that can be removed in the given machine
/// and also returns a set of new constraints to be added.
fn redundant_memory_interactions_indices<
    T: FieldElement,
    V: Ord + Clone + Hash + Display,
    M: MemoryBusInteraction<T, V>,
>(
    system: &ConstraintSystem<T, V>,
    solver: &mut impl Solver<T, V>,
    memory_bus_id: u64,
) -> (
    Vec<usize>,
    Vec<AlgebraicConstraint<GroupedExpression<T, V>>>,
) {
    let mut new_constraints = Vec::new();

    // Track memory contents by memory type while we go through bus interactions.
    // This maps an address to the index of the previous send on that address, the
    // data currently stored there and the timestamp used in the last send.
    let mut memory_contents: HashMap<Address<T, V>, MemoryContent<T, V>> = Default::default();
    let mut to_remove: Vec<usize> = Default::default();

    // TODO we assume that memory interactions are sorted by timestamp.
    for (index, bus_int) in system.bus_interactions.iter().enumerate() {
        let mem_int = match M::try_from_bus_interaction(bus_int, memory_bus_id) {
            Ok(Some(mem_int)) => mem_int,
            Ok(None) => continue,
            Err(_) => {
                // This interaction might be going to memory, but we do not know
                // the multiplicity. Delete all knowledge.
                // TODO If we can still clearly determine the memory type, we could
                // only clear the knowledge for that memory type.
                memory_contents.clear();
                continue;
            }
        };

        let addr = mem_int.addr().into();

        match mem_int.op() {
            MemoryOp::GetPrevious => {
                // If there is an unconsumed send to this address, consume it.
                // In that case, we can replace both bus interactions with equality constraints
                // between the data that would have been sent and received.
                if let Some(existing) = memory_contents.remove(&addr) {
                    for (existing, new) in existing.data.iter().zip_eq(mem_int.data().iter()) {
                        new_constraints.push(AlgebraicConstraint::assert_zero(
                            existing.clone() - new.clone(),
                        ));
                    }
                    for (existing_timestamp_limb, new_timestamp_limb) in existing
                        .timestamp_limbs
                        .iter()
                        .zip_eq(mem_int.timestamp_limbs().iter())
                    {
                        new_constraints.push(AlgebraicConstraint::assert_zero(
                            existing_timestamp_limb.clone() - new_timestamp_limb.clone(),
                        ));
                    }
                    to_remove.extend([index, existing.bus_index]);
                }
            }
            MemoryOp::SetNew => {
                // We can only retain knowledge about addresses where we can prove
                // that this send operation does not interfere with it, i.e.
                // if we can prove that the two addresses differ by at least a word size.
                memory_contents.retain(|other_addr, _| {
                    addr.0
                        .iter()
                        .zip_eq(other_addr.0.iter())
                        // Two addresses are different if they differ in at least one component.
                        .any(|(a, b)| solver.are_expressions_known_to_be_different(a, b))
                });
                memory_contents.insert(
                    addr.clone(),
                    MemoryContent::from_bus_interaction(index, mem_int),
                );
            }
        }
    }

    log::debug!(
        "Removing {} memory interactions and adding {} new constraints",
        to_remove.len(),
        new_constraints.len()
    );

    (to_remove, new_constraints)
}

#[cfg(test)]
mod tests {
    use super::*;
    use powdr_constraint_solver::constraint_system::Hint;
    use powdr_number::GoldilocksField;

    type V = &'static str;
    type Ge = GroupedExpression<GoldilocksField, V>;

    const MEM_BUS: u64 = 7;

    fn num(n: u64) -> Ge {
        Ge::from_number(GoldilocksField::from(n))
    }

    fn var(name: V) -> Ge {
        Ge::from_unknown_variable(name)
    }

    /// A minimal memory bus interaction for the tests, with payload layout
    /// `[address_space, address, data, timestamp]` and `multiplicity` `+1` =
    /// `SetNew`, `-1` = `GetPrevious`.
    struct TestMem {
        op: MemoryOp,
        address_space: Ge,
        address: Ge,
        data: Vec<Ge>,
        timestamp: Vec<Ge>,
    }

    impl MemoryBusInteraction<GoldilocksField, V> for TestMem {
        type Address = Vec<Ge>;

        fn try_from_bus_interaction(
            bus_interaction: &BusInteraction<Ge>,
            memory_bus_id: u64,
        ) -> Result<Option<Self>, MemoryBusInteractionConversionError> {
            match bus_interaction.bus_id.try_to_number() {
                Some(id) if id == GoldilocksField::from(memory_bus_id) => {}
                Some(_) => return Ok(None),
                None => return Err(MemoryBusInteractionConversionError),
            }
            let op = match bus_interaction.multiplicity.try_to_number() {
                Some(n) if n == GoldilocksField::from(1) => MemoryOp::SetNew,
                Some(n) if n == -GoldilocksField::from(1) => MemoryOp::GetPrevious,
                _ => return Err(MemoryBusInteractionConversionError),
            };
            let [address_space, address, data @ .., timestamp] = &bus_interaction.payload[..]
            else {
                panic!("unexpected memory payload");
            };
            Ok(Some(TestMem {
                op,
                address_space: address_space.clone(),
                address: address.clone(),
                data: data.to_vec(),
                timestamp: vec![timestamp.clone()],
            }))
        }

        fn addr(&self) -> Vec<Ge> {
            vec![self.address_space.clone(), self.address.clone()]
        }

        fn data(&self) -> &[Ge] {
            &self.data
        }

        fn timestamp_limbs(&self) -> &[Ge] {
            &self.timestamp
        }

        fn op(&self) -> MemoryOp {
            self.op
        }
    }

    fn mem(
        mult: i32,
        address_space: u64,
        address: Ge,
        data: Ge,
        timestamp: Ge,
    ) -> BusInteraction<Ge> {
        let multiplicity = if mult >= 0 {
            num(mult as u64)
        } else {
            -num((-mult) as u64)
        };
        BusInteraction {
            bus_id: num(MEM_BUS),
            multiplicity,
            payload: vec![num(address_space), address, data, timestamp],
        }
    }

    /// A drop hint whose base slot is at `fp + fp_offset` (the `address`
    /// expression must be consistent with `fp_offset`, as the lowering
    /// guarantees in production).
    fn drop_hint(
        kind: MemoryDropKind,
        address_space: u64,
        address: Ge,
        timestamp: Ge,
        fp_offset: u64,
    ) -> Hint<Ge> {
        Hint::new(
            kind as u32,
            vec![num(address_space), address, timestamp, num(fp_offset)],
        )
    }

    #[test]
    fn drops_only_fully_internal_slots() {
        let fp = var("fp");
        let t = var("t");

        // Five slots, all in address space 1, addressed `fp + offset`.
        let system = ConstraintSystem {
            // `old_b` is read and used by a constraint, so slot B's read is not a ghost.
            algebraic_constraints: vec![AlgebraicConstraint::assert_zero(var("old_b"))],
            bus_interactions: vec![
                // A (fp+8): ghost read, dropped, drop timestamp == write timestamp -> REMOVED.
                mem(-1, 1, fp.clone() + num(8), var("old_a"), t.clone()),
                mem(1, 1, fp.clone() + num(8), var("new_a"), t.clone() + num(1)),
                // B (fp+12): previous value used -> KEPT.
                mem(-1, 1, fp.clone() + num(12), var("old_b"), t.clone()),
                mem(1, 1, fp.clone() + num(12), var("new_b"), t.clone() + num(1)),
                // C (fp+16): only drop is before the last write -> KEPT.
                mem(-1, 1, fp.clone() + num(16), var("old_c"), t.clone()),
                mem(1, 1, fp.clone() + num(16), var("new_c"), t.clone() + num(5)),
                // D (fp+20): ghost read, in a DropFrom range (>= fp+4) -> REMOVED.
                mem(-1, 1, fp.clone() + num(20), var("old_d"), t.clone()),
                mem(1, 1, fp.clone() + num(20), var("new_d"), t.clone() + num(1)),
                // E (fp+0): below the DropFrom range (< fp+4) -> KEPT.
                mem(-1, 1, fp.clone() + num(0), var("old_e"), t.clone()),
                mem(1, 1, fp.clone() + num(0), var("new_e"), t.clone() + num(1)),
            ],
            derived_variables: vec![],
            hints: vec![
                drop_hint(
                    MemoryDropKind::MemorySlotDrop,
                    1,
                    fp.clone() + num(8),
                    t.clone() + num(1),
                    8,
                ),
                drop_hint(
                    MemoryDropKind::MemorySlotDrop,
                    1,
                    fp.clone() + num(12),
                    t.clone() + num(1),
                    12,
                ),
                // C's drop is at t+2, but its write is at t+5: must not match.
                drop_hint(
                    MemoryDropKind::MemorySlotDrop,
                    1,
                    fp.clone() + num(16),
                    t.clone() + num(2),
                    16,
                ),
                // Covers every slot at offset >= 4 in address space 1.
                drop_hint(
                    MemoryDropKind::MemorySlotDropFrom,
                    1,
                    fp.clone() + num(4),
                    t.clone() + num(1),
                    4,
                ),
            ],
        };

        let (result, dropped_slots) =
            drop_internal_memory_accesses::<GoldilocksField, V, TestMem>(system, Some(MEM_BUS));

        let remaining: HashSet<Ge> = result
            .bus_interactions
            .iter()
            .map(|b| b.payload[2].clone())
            .collect();

        // Only A and D (both fully internal) are removed; everything else stays.
        assert_eq!(result.bus_interactions.len(), 6);
        for removed in ["old_a", "new_a", "old_d", "new_d"] {
            assert!(
                !remaining.contains(&var(removed)),
                "{removed} should be removed"
            );
        }
        for kept in ["old_b", "new_b", "old_c", "new_c", "old_e", "new_e"] {
            assert!(remaining.contains(&var(kept)), "{kept} should be kept");
        }

        // The dropped slots are reported with their runtime identity: A at
        // fp+8 (exact hint), D at fp+20 (DropFrom base 4 + delta 16).
        assert_eq!(
            dropped_slots,
            vec![
                DroppedMemorySlot {
                    address_space: 1,
                    fp_offset: 8,
                },
                DroppedMemorySlot {
                    address_space: 1,
                    fp_offset: 20,
                },
            ]
        );
    }

    /// A hint without a recorded fp_offset (legacy 3-arg encoding) must not
    /// drop: the runtime could not identify the slot to elide.
    #[test]
    fn does_not_drop_without_fp_offset() {
        let fp = var("fp");
        let t = var("t");
        let system = ConstraintSystem {
            algebraic_constraints: vec![],
            bus_interactions: vec![
                mem(-1, 1, fp.clone() + num(8), var("old_a"), t.clone()),
                mem(1, 1, fp.clone() + num(8), var("new_a"), t.clone() + num(1)),
            ],
            derived_variables: vec![],
            hints: vec![Hint::new(
                MemoryDropKind::MemorySlotDrop as u32,
                vec![num(1), fp.clone() + num(8), t.clone() + num(1)],
            )],
        };
        let (result, dropped_slots) =
            drop_internal_memory_accesses::<GoldilocksField, V, TestMem>(system, Some(MEM_BUS));
        assert_eq!(result.bus_interactions.len(), 2);
        assert!(dropped_slots.is_empty());
    }

    #[test]
    fn no_hints_is_a_no_op() {
        let fp = var("fp");
        let t = var("t");
        let system = ConstraintSystem {
            algebraic_constraints: vec![],
            bus_interactions: vec![
                mem(-1, 1, fp.clone() + num(8), var("old_a"), t.clone()),
                mem(1, 1, fp.clone() + num(8), var("new_a"), t.clone() + num(1)),
            ],
            derived_variables: vec![],
            hints: vec![],
        };
        let (result, dropped_slots) =
            drop_internal_memory_accesses::<GoldilocksField, V, TestMem>(system, Some(MEM_BUS));
        assert_eq!(result.bus_interactions.len(), 2);
        assert!(dropped_slots.is_empty());
    }
}

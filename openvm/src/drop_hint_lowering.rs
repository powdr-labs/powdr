//! Lowering of an ISA's abstract per-instruction liveness hints
//! ([`DropHint`]) into concrete memory-slot drops ([`MemoryDrop`]) attached to
//! the block's symbolic machine.
//!
//! The whole lowering for the hint-providing ISA lives behind the
//! `Adapter::lower_memory_drops` hook; this module holds the ISA-agnostic part
//! (the FP-epoch model and the `DropHint` → `MemoryDrop` mapping), driven by
//! per-instruction data the ISA extracts from each instruction's AIR.

use powdr_autoprecompiles::{
    drop_hints::DropHint,
    expression::AlgebraicExpression,
    symbolic_machine::{MemoryDrop, MemoryDropKind, SymbolicMachine},
};
use powdr_expression::AlgebraicBinaryOperator;
use powdr_number::FieldElement;

/// ISA-level constants needed to lower drop hints. Returned by the ISA; `None`
/// means the ISA does not emit liveness hints, so lowering is skipped entirely.
/// These are pure ISA data — all machine introspection happens in this module.
#[derive(Clone, Copy, Debug)]
pub struct DropHintConfig {
    /// Address space of the FP-relative register slots the hints refer to.
    pub register_address_space: u32,
    /// Multiplier mapping a hint's register index `n` to its address offset
    /// (`address = fp + n * stride`). Must match how the ISA's load/store AIRs
    /// encode FP-relative offsets, or the drop addresses won't unify with the
    /// slot accesses.
    pub stride: u64,
    /// Address space in which the FP register itself is stored.
    pub fp_address_space: u32,
    /// Fixed address of the FP register (the FP value is read from here).
    pub fp_address: u64,
}

/// Extracts, from a single instruction's (globalized) symbolic machine, the data
/// needed to anchor its drop hints: the FP value read at the start (if any),
/// whether the instruction writes FP, and the begin/after execution timestamps.
///
/// This relies only on the (ISA-independent) OpenVM bus structure: the
/// ExecutionBridge carries `(pc, timestamp)`, and a memory interaction carries
/// `[address_space, addr, data.., timestamp]`. Returns `None` if the machine
/// doesn't have the expected pair of ExecutionBridge interactions.
pub fn instruction_drop_context<F: FieldElement>(
    machine: &SymbolicMachine<F>,
    config: &DropHintConfig,
    memory_bus_id: u64,
    execution_bridge_bus_id: u64,
) -> Option<InstructionDropContext<F>> {
    // Timestamps: the two ExecutionBridge interactions are the receive (start)
    // followed by the send (after); each payload is `(pc, timestamp)`.
    let exec = machine
        .bus_interactions
        .iter()
        .filter(|b| b.id == execution_bridge_bus_id)
        .collect::<Vec<_>>();
    let [receive, send] = exec[..] else {
        return None;
    };
    let timestamp_begin = receive.args.get(1)?.clone();
    let timestamp_after = send.args.get(1)?.clone();

    // FP read / write: a memory interaction at the FP register's fixed address.
    // Payload is `[address_space, addr, data.., timestamp]`, FP is a single limb.
    let is_number = |expr: &AlgebraicExpression<F>, value: F| matches!(expr, AlgebraicExpression::Number(n) if *n == value);
    let fp_address_space = F::from(config.fp_address_space as u64);
    let fp_address = F::from(config.fp_address);
    let mut fp_begin = None;
    let mut writes_fp = false;
    for interaction in &machine.bus_interactions {
        if interaction.id != memory_bus_id || interaction.args.len() < 4 {
            continue;
        }
        if !is_number(&interaction.args[0], fp_address_space)
            || !is_number(&interaction.args[1], fp_address)
        {
            continue;
        }
        if multiplicity_is_negative(&interaction.mult) {
            // GetPrevious: the FP value read at the start.
            fp_begin.get_or_insert_with(|| interaction.args[2].clone());
        } else if !is_number(&interaction.mult, F::zero()) {
            // SetNew: the instruction updates FP, ending the FP epoch.
            writes_fp = true;
        }
    }

    Some(InstructionDropContext {
        fp_begin,
        writes_fp,
        timestamp_begin,
        timestamp_after,
    })
}

/// Whether a memory multiplicity is negative — a read (`GetPrevious`); a
/// positive one is a write (`SetNew`). Handles:
/// - folded literal: `Number(-1)`
/// - unary negation: `UnaryOperation(Minus, x)` → `-x`
/// - OpenVM's `NEG_ONE * direction`: `BinaryOperation(Mul, Number(-1), x)`
fn multiplicity_is_negative<F: FieldElement>(mult: &AlgebraicExpression<F>) -> bool {
    match mult {
        AlgebraicExpression::Number(n) => !n.is_in_lower_half(),
        AlgebraicExpression::UnaryOperation(op) => !multiplicity_is_negative(op.expr.as_ref()),
        AlgebraicExpression::BinaryOperation(op) if op.op == AlgebraicBinaryOperator::Mul => {
            // NEG_ONE * x  or  x * NEG_ONE
            let left_neg = multiplicity_is_negative(&op.left);
            let right_neg = multiplicity_is_negative(&op.right);
            left_neg ^ right_neg
        }
        _ => false,
    }
}

/// Per-instruction data extracted from an instruction's AIR, needed to anchor
/// its drop hints. The expressions reference the instruction's (globalized)
/// machine columns.
pub struct InstructionDropContext<F> {
    /// FP value read at the start of the instruction. `None` if the instruction
    /// doesn't fetch FP; then the FP is borrowed from another instruction in the
    /// same epoch.
    pub fp_begin: Option<AlgebraicExpression<F>>,
    /// Whether the instruction writes FP, i.e. an FP epoch boundary follows it.
    pub writes_fp: bool,
    /// Execution timestamp at the start of the instruction.
    pub timestamp_begin: AlgebraicExpression<F>,
    /// Execution timestamp after the instruction executes.
    pub timestamp_after: AlgebraicExpression<F>,
}

/// Lowers a block's per-instruction drop hints into [`MemoryDrop`]s.
///
/// `per_instruction` is index-aligned with the block's instructions; each entry
/// is the instruction's hints together with its extracted context (`None` if
/// the instruction's AIR couldn't be decoded).
///
/// FP-epoch model: instructions are split into epochs at each FP write; FP is
/// constant within an epoch, so a hint's address is anchored to *any* FP read in
/// its instruction's epoch. All hints use the FP at the instruction's start;
/// the `*Before*` variants use the start timestamp and `RelDropAfter` the after
/// timestamp. A hint is skipped (soundly, just a missed optimization) when its
/// epoch has no FP read at all (it touches no FP-relative slot) or its context
/// couldn't be extracted.
pub fn lower_memory_drops<F: FieldElement>(
    per_instruction: &[(Vec<DropHint>, Option<InstructionDropContext<F>>)],
    register_address_space: F,
    register_stride: u64,
) -> Vec<MemoryDrop<F>> {
    if per_instruction.is_empty() {
        return Vec::new();
    }

    // Pass 1: assign an epoch id to each instruction (a new epoch starts after
    // every FP write) and record, per epoch, the FP read of any instruction in
    // it.
    let mut epoch_of = Vec::with_capacity(per_instruction.len());
    let mut epoch_fp: Vec<Option<AlgebraicExpression<F>>> = vec![None];
    let mut current = 0usize;
    for (_, context) in per_instruction {
        epoch_of.push(current);
        if let Some(context) = context {
            if epoch_fp[current].is_none() {
                epoch_fp[current].clone_from(&context.fp_begin);
            }
            if context.writes_fp {
                current += 1;
                epoch_fp.push(None);
            }
        }
    }

    // Pass 2: build the drops.
    let address_space = AlgebraicExpression::Number(register_address_space);
    let mut drops = Vec::new();
    for (i, (hints, context)) in per_instruction.iter().enumerate() {
        if hints.is_empty() {
            continue;
        }
        let Some(context) = context else { continue };
        let Some(fp) = &epoch_fp[epoch_of[i]] else {
            // The epoch has no FP read, so it touches no FP-relative slot and
            // the hint is vacuous; skip.
            continue;
        };
        for hint in hints {
            let (kind, index, timestamp) = match hint {
                DropHint::RelDropBefore(n) => {
                    (MemoryDropKind::MemorySlotDrop, *n, &context.timestamp_begin)
                }
                DropHint::RelDropBeforeFrom(n) => (
                    MemoryDropKind::MemorySlotDropFrom,
                    *n,
                    &context.timestamp_begin,
                ),
                DropHint::RelDropAfter(n) => {
                    (MemoryDropKind::MemorySlotDrop, *n, &context.timestamp_after)
                }
            };
            let offset = F::from(index as u64 * register_stride);
            drops.push(MemoryDrop {
                kind,
                address_space: address_space.clone(),
                address: fp.clone() + AlgebraicExpression::Number(offset),
                timestamp: timestamp.clone(),
            });
        }
    }
    drops
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use powdr_autoprecompiles::expression::AlgebraicReference;
    use powdr_autoprecompiles::symbolic_machine::SymbolicBusInteraction;
    use powdr_number::BabyBearField;

    use super::*;

    type Ge = AlgebraicExpression<BabyBearField>;

    const STRIDE: u64 = 4;
    const REG_AS: u64 = 9;
    const MEM_BUS: u64 = 1;
    const EXEC_BUS: u64 = 0;
    const FP_AS: u32 = 5;
    const FP_ADDR: u64 = 40;

    fn col(name: &str, id: u64) -> Ge {
        AlgebraicExpression::Reference(AlgebraicReference {
            name: Arc::new(name.to_string()),
            id,
        })
    }

    fn num(n: u64) -> Ge {
        AlgebraicExpression::Number(BabyBearField::from(n))
    }

    fn ctx(
        fp_begin: Option<Ge>,
        writes_fp: bool,
        ts_begin: Ge,
        ts_after: Ge,
    ) -> InstructionDropContext<BabyBearField> {
        InstructionDropContext {
            fp_begin,
            writes_fp,
            timestamp_begin: ts_begin,
            timestamp_after: ts_after,
        }
    }

    fn address(fp: &Ge, n: u64) -> Ge {
        fp.clone() + AlgebraicExpression::Number(BabyBearField::from(n * STRIDE))
    }

    #[test]
    fn fp_epoch_lowering() {
        let fp0 = col("fp0", 0);
        let fp1 = col("fp1", 10);

        let per_instruction = vec![
            // i0: fetches FP (fp0), epoch 0, drop-before(2).
            (
                vec![DropHint::RelDropBefore(2)],
                Some(ctx(Some(fp0.clone()), false, col("t0", 1), col("t0a", 2))),
            ),
            // i1: no FP fetch, epoch 0 -> borrows fp0. drop-after(3) + drop-before-from(5).
            (
                vec![DropHint::RelDropAfter(3), DropHint::RelDropBeforeFrom(5)],
                Some(ctx(None, false, col("t1", 3), col("t1a", 4))),
            ),
            // i2: writes FP -> epoch boundary after it. No hints.
            (
                vec![],
                Some(ctx(Some(fp1.clone()), true, col("t2", 5), col("t2a", 6))),
            ),
            // i3: epoch 1, no FP read in this epoch -> hint skipped.
            (
                vec![DropHint::RelDropBefore(1)],
                Some(ctx(None, false, col("t3", 7), col("t3a", 8))),
            ),
        ];

        let drops = lower_memory_drops(&per_instruction, BabyBearField::from(REG_AS), STRIDE);

        let address_space = AlgebraicExpression::Number(BabyBearField::from(REG_AS));

        // i0 -> 1 drop; i1 -> 2 drops; i3 skipped (vacuous epoch).
        assert_eq!(drops.len(), 3);

        // i0: drop-before(2) at fp0, start timestamp.
        assert_eq!(drops[0].kind, MemoryDropKind::MemorySlotDrop);
        assert_eq!(drops[0].address_space, address_space);
        assert_eq!(drops[0].address, address(&fp0, 2));
        assert_eq!(drops[0].timestamp, col("t0", 1));

        // i1: drop-after(3) borrows fp0, uses the after timestamp.
        assert_eq!(drops[1].kind, MemoryDropKind::MemorySlotDrop);
        assert_eq!(drops[1].address, address(&fp0, 3));
        assert_eq!(drops[1].timestamp, col("t1a", 4));

        // i1: drop-before-from(5) borrows fp0, uses the start timestamp.
        assert_eq!(drops[2].kind, MemoryDropKind::MemorySlotDropFrom);
        assert_eq!(drops[2].address, address(&fp0, 5));
        assert_eq!(drops[2].timestamp, col("t1", 3));
    }

    #[test]
    fn no_context_is_skipped() {
        let per_instruction = vec![(vec![DropHint::RelDropBefore(0)], None)];
        let drops = lower_memory_drops(&per_instruction, BabyBearField::from(REG_AS), STRIDE);
        assert!(drops.is_empty());
    }

    fn exec(timestamp: Ge) -> SymbolicBusInteraction<BabyBearField> {
        SymbolicBusInteraction {
            id: EXEC_BUS,
            mult: num(1),
            args: vec![col("pc", 100), timestamp],
        }
    }

    fn mem(
        mult: i64,
        address_space: u64,
        address: u64,
        data: Ge,
    ) -> SymbolicBusInteraction<BabyBearField> {
        let mult = if mult >= 0 {
            num(mult as u64)
        } else {
            -num((-mult) as u64)
        };
        SymbolicBusInteraction {
            id: MEM_BUS,
            mult,
            args: vec![num(address_space), num(address), data, col("mem_ts", 200)],
        }
    }

    fn config() -> DropHintConfig {
        DropHintConfig {
            register_address_space: REG_AS as u32,
            stride: STRIDE,
            fp_address_space: FP_AS,
            fp_address: FP_ADDR,
        }
    }

    #[test]
    fn extracts_fp_read_and_timestamps() {
        let machine = SymbolicMachine {
            constraints: vec![],
            bus_interactions: vec![
                // receive (start) then send (after).
                exec(col("ts_begin", 1)),
                exec(col("ts_after", 2)),
                // an unrelated register read (different address) - ignored.
                mem(-1, FP_AS as u64, 4, col("other", 3)),
                // the FP read.
                mem(-1, FP_AS as u64, FP_ADDR, col("fp_val", 4)),
            ],
            derived_columns: vec![],
            memory_drops: vec![],
        };
        let context = instruction_drop_context(&machine, &config(), MEM_BUS, EXEC_BUS).unwrap();
        assert_eq!(context.fp_begin, Some(col("fp_val", 4)));
        assert!(!context.writes_fp);
        assert_eq!(context.timestamp_begin, col("ts_begin", 1));
        assert_eq!(context.timestamp_after, col("ts_after", 2));
    }

    #[test]
    fn detects_fp_write() {
        let machine = SymbolicMachine {
            constraints: vec![],
            bus_interactions: vec![
                exec(col("ts_begin", 1)),
                exec(col("ts_after", 2)),
                // FP write (SetNew at the FP address).
                mem(1, FP_AS as u64, FP_ADDR, col("new_fp", 4)),
            ],
            derived_columns: vec![],
            memory_drops: vec![],
        };
        let context = instruction_drop_context(&machine, &config(), MEM_BUS, EXEC_BUS).unwrap();
        assert!(context.writes_fp);
        assert_eq!(context.fp_begin, None);
    }

    #[test]
    fn no_exec_bridge_pair_yields_none() {
        let machine = SymbolicMachine {
            constraints: vec![],
            bus_interactions: vec![exec(col("ts_begin", 1))],
            derived_columns: vec![],
            memory_drops: vec![],
        };
        assert!(instruction_drop_context(&machine, &config(), MEM_BUS, EXEC_BUS).is_none());
    }
}

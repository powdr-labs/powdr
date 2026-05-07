//! NVRTC emitter: turns surviving-column descriptors into CUDA source for a
//! per-APC trace-gen kernel.
//!
//! The host pre-folds opcodes (each APC instruction has a fixed opcode), so
//! the emitted code never dispatches on `record[opcode_byte_offset]` for
//! ALU/Shift/LoadStore — instead it emits the specific arm directly.
//!
//! Output kernel signature:
//!
//! ```text
//! extern "C" __global__ void <name>(
//!     uint32_t*       d_output,        // column-major, Montgomery words
//!     unsigned long long H,            // trace height (power of 2)
//!     int             N,               // number of valid rows
//!     const uint8_t*  d_arena,         // concatenated arena bytes
//!     uint32_t        range_max_bits);
//! ```

use std::collections::hash_map::DefaultHasher;
use std::fmt::Write as _;
use std::hash::{Hash, Hasher};

/// Bumped whenever the emitter's output format changes — forces cache
/// invalidation across emitter revisions.
const EMITTER_VERSION: u32 = 3;

/// One column to materialize from this instruction's record bytes.
///
/// Each variant maps to a closed expression over `rec + offset` byte slots,
/// with all opcodes / shift directions / branch directions already folded by
/// the host (so the emitted code does not switch on opcode bytes at runtime).
#[derive(Clone, Debug, Hash)]
pub enum EmitterColumn {
    /// Direct `*(uint32_t*)(rec + off)`.
    DirectU32 { apc_col: u16, off: u16 },
    /// Direct `(uint32_t)rec[off]`.
    DirectU8 { apc_col: u16, off: u16 },
    /// Direct `*(uint16_t*)(rec + off)`.
    DirectU16 { apc_col: u16, off: u16 },
    /// Compile-time constant value (already canonical, < P).
    Constant { apc_col: u16, value: u32 },
    /// `1` if the host-known instruction opcode equals `expected_opcode`,
    /// else `0`. The host has already resolved the comparison so this is
    /// emitted as a literal `0` or `1`.
    BoolFromOpcodeFolded { apc_col: u16, value: u8 },
    /// Conditional wrapper: if `record[cond_off] != 0`, materialize `inner`,
    /// else write `0`.
    Conditional {
        cond_off: u16,
        inner: Box<EmitterColumn>,
    },
    /// One limb of `decompose((curr_ts + delta) - prev_ts - 1)` against
    /// `range_max_bits` (kernel arg).
    TimestampDecomp {
        apc_col: u16,
        curr_off: u16,
        prev_off: u16,
        delta: u16,
        limb_index: u16,
    },
    /// One u16 limb of `(val + sign_extend(imm, sign))`.
    PointerLimb {
        apc_col: u16,
        val_off: u16,
        imm_off: u16,
        imm_sign_off: u16,
        limb_index: u16,
    },
    /// One byte of `run_alu(opcode, b, c)`.
    AluResult {
        apc_col: u16,
        opcode_off: u16,
        b_off: u16,
        c_off: u16,
        limb_index: u16,
    },
    /// One byte of `run_shift(opcode, b, c)`.
    ShiftResult {
        apc_col: u16,
        opcode_off: u16,
        b_off: u16,
        c_off: u16,
        limb_index: u16,
    },
    /// `is_sll ? (1 << bit_shift) : 0`. `is_sll` from opcode==0; bit shift from `c[0]`.
    ShiftBitMulLeft { apc_col: u16, opcode_off: u16, c_off: u16 },
    /// `is_sll ? 0 : (1 << bit_shift)`.
    ShiftBitMulRight { apc_col: u16, opcode_off: u16, c_off: u16 },
    /// `is_sra ? (b[3] >> 7) : 0`. `is_sra` from opcode==2.
    ShiftBSign { apc_col: u16, opcode_off: u16, b_off: u16 },
    /// `bit_shift == marker_index`.
    ShiftBitMarker { apc_col: u16, c_off: u16, marker_index: u16 },
    /// `limb_shift == marker_index`.
    ShiftLimbMarker { apc_col: u16, c_off: u16, marker_index: u16 },
    /// Carry between limbs for SLL/SRL. See run_shift in apc_jit_tracegen.cu.
    ShiftBitCarry {
        apc_col: u16,
        opcode_off: u16,
        b_off: u16,
        c_off: u16,
        limb_index: u16,
    },
    /// BranchEqual: cmp_result. `1` if (BEQ and a==b) or (BNE and a!=b), else 0.
    BranchEqualCmpResult {
        apc_col: u16,
        a_off: u16,
        b_off: u16,
        opcode_off: u16,
    },
    /// BranchEqual: diff_inv_marker[marker_index]. Field inverse of the
    /// first differing limb when marker matches, else 0.
    BranchEqualDiffInvMarker {
        apc_col: u16,
        a_off: u16,
        b_off: u16,
        marker_index: u16,
    },
    /// LoadStore: rd_rs2_ptr (0 if 0xFFFFFFFF, else value).
    LoadStoreRdRs2Ptr { apc_col: u16, off: u16 },
    /// LoadStore: needs_write (rd_rs2_ptr != 0xFFFFFFFF).
    LoadStoreNeedsWrite { apc_col: u16, off: u16 },
    /// LoadStore: write_aux.prev_timestamp (gated on needs_write).
    LoadStoreWriteAuxPrevTs {
        apc_col: u16,
        write_prev_ts_off: u16,
        rd_rs2_ptr_off: u16,
    },
    /// LoadStore: write_aux timestamp decomposition (gated on needs_write).
    LoadStoreWriteAuxDecomp {
        apc_col: u16,
        from_ts_off: u16,
        write_prev_ts_off: u16,
        rd_rs2_ptr_off: u16,
        limb_index: u16,
    },
    /// LoadStore: is_load = (opcode <= 2).
    LoadStoreIsLoad { apc_col: u16, opcode_off: u16 },
    /// LoadStore: flags[flag_index] from opcode + shift.
    LoadStoreFlag {
        apc_col: u16,
        opcode_off: u16,
        shift_off: u16,
        flag_index: u16,
    },
    /// LoadStore: write_data byte for one limb, op-dependent.
    LoadStoreWriteData {
        apc_col: u16,
        opcode_off: u16,
        shift_off: u16,
        read_data_off: u16,
        prev_data_off: u16,
        limb_index: u16,
    },
}

impl EmitterColumn {
    /// APC column slot this emitter writes to. For `Conditional`, it's
    /// the inner's apc_col.
    fn apc_col(&self) -> u16 {
        match self {
            EmitterColumn::DirectU32 { apc_col, .. }
            | EmitterColumn::DirectU8 { apc_col, .. }
            | EmitterColumn::DirectU16 { apc_col, .. }
            | EmitterColumn::Constant { apc_col, .. }
            | EmitterColumn::BoolFromOpcodeFolded { apc_col, .. }
            | EmitterColumn::TimestampDecomp { apc_col, .. }
            | EmitterColumn::PointerLimb { apc_col, .. }
            | EmitterColumn::AluResult { apc_col, .. }
            | EmitterColumn::ShiftResult { apc_col, .. }
            | EmitterColumn::ShiftBitMulLeft { apc_col, .. }
            | EmitterColumn::ShiftBitMulRight { apc_col, .. }
            | EmitterColumn::ShiftBSign { apc_col, .. }
            | EmitterColumn::ShiftBitMarker { apc_col, .. }
            | EmitterColumn::ShiftLimbMarker { apc_col, .. }
            | EmitterColumn::ShiftBitCarry { apc_col, .. }
            | EmitterColumn::BranchEqualCmpResult { apc_col, .. }
            | EmitterColumn::BranchEqualDiffInvMarker { apc_col, .. }
            | EmitterColumn::LoadStoreRdRs2Ptr { apc_col, .. }
            | EmitterColumn::LoadStoreNeedsWrite { apc_col, .. }
            | EmitterColumn::LoadStoreWriteAuxPrevTs { apc_col, .. }
            | EmitterColumn::LoadStoreWriteAuxDecomp { apc_col, .. }
            | EmitterColumn::LoadStoreIsLoad { apc_col, .. }
            | EmitterColumn::LoadStoreFlag { apc_col, .. }
            | EmitterColumn::LoadStoreWriteData { apc_col, .. } => *apc_col,
            EmitterColumn::Conditional { inner, .. } => inner.apc_col(),
        }
    }
}

/// One instruction within an APC.
#[derive(Clone, Debug, Hash)]
pub struct EmitterInstruction {
    pub arena_offset: u32,
    pub record_stride: u32,
    pub record_offset: u32,
    pub columns: Vec<EmitterColumn>,
}

/// Emitter input: per-APC-chip data the codegen needs.
#[derive(Clone, Debug, Hash)]
pub struct EmitterInput {
    pub instructions: Vec<EmitterInstruction>,
}

#[derive(Clone, Debug)]
pub struct EmittedKernel {
    pub source: String,
    pub name: String,
    pub source_hash: u64,
}

/// Emit a CUDA source string for the given `EmitterInput`. The kernel name
/// is `apc_kernel_<hash16>` where `<hash16>` is a hex rendering of the
/// input's structural hash. That same hash is the cache key.
pub fn emit_jit_kernel_source(input: &EmitterInput) -> EmittedKernel {
    let mut hasher = DefaultHasher::new();
    EMITTER_VERSION.hash(&mut hasher);
    input.hash(&mut hasher);
    let source_hash = hasher.finish();
    let name = format!("apc_kernel_{:016x}", source_hash);

    let mut s = String::new();
    s.push_str(PRELUDE);

    writeln!(s, "extern \"C\" __global__ void {}(", name).unwrap();
    s.push_str("    unsigned int* __restrict__ d_output,\n");
    s.push_str("    unsigned long long H,\n");
    s.push_str("    int N,\n");
    s.push_str("    const unsigned char* __restrict__ d_arena,\n");
    s.push_str("    unsigned int range_max_bits) {\n");
    s.push_str(
        "    const unsigned long long total = (unsigned long long)gridDim.x * blockDim.x;\n",
    );
    s.push_str(
        "    const unsigned long long tid   = (unsigned long long)blockIdx.x * blockDim.x + threadIdx.x;\n",
    );
    s.push_str("    for (unsigned long long r = tid; r < (unsigned long long)N; r += total) {\n");

    for (i, instr) in input.instructions.iter().enumerate() {
        writeln!(s, "        // === Instruction {} ===", i).unwrap();
        writeln!(
            s,
            "        const unsigned char* rec{} = d_arena + {}u + {}u + r * {}u;",
            i, instr.arena_offset, instr.record_offset, instr.record_stride
        )
        .unwrap();
        emit_instruction_columns(&mut s, i, &instr.columns);
    }

    s.push_str("    }\n");
    s.push_str("}\n");

    EmittedKernel {
        source: s,
        name,
        source_hash,
    }
}

/// Emit one instruction's columns with multi-limb fusion. Columns that share
/// a record signature (e.g. four `AluResult` columns differing only in
/// `limb_index`) collapse into a single `powdr_run_alu` call followed by one
/// store per limb apc_col. Non-fusable columns emit individually.
///
/// Fusion is conservative: bare (non-Conditional) `AluResult`, `ShiftResult`,
/// and `TimestampDecomp` columns participate. Conditional-wrapped variants
/// pass through unfused because the conditional guard differs.
fn emit_instruction_columns(s: &mut String, i: usize, cols: &[EmitterColumn]) {
    use std::collections::BTreeMap;

    // Fusion buckets keyed by the shared-signature tuple. Vec preserves first
    // appearance order so emit order remains deterministic regardless of
    // BTreeMap iteration.
    let mut alu_groups: BTreeMap<(u16, u16, u16), Vec<(u16, u16)>> = BTreeMap::new();
    let mut shift_groups: BTreeMap<(u16, u16, u16), Vec<(u16, u16)>> = BTreeMap::new();
    let mut ts_groups: BTreeMap<(u16, u16, u16), Vec<(u16, u16)>> = BTreeMap::new();
    // Non-fusable columns retain original order.
    let mut others: Vec<&EmitterColumn> = Vec::new();

    for col in cols {
        match col {
            EmitterColumn::AluResult { apc_col, opcode_off, b_off, c_off, limb_index } => {
                alu_groups
                    .entry((*opcode_off, *b_off, *c_off))
                    .or_default()
                    .push((*apc_col, *limb_index));
            }
            EmitterColumn::ShiftResult { apc_col, opcode_off, b_off, c_off, limb_index } => {
                shift_groups
                    .entry((*opcode_off, *b_off, *c_off))
                    .or_default()
                    .push((*apc_col, *limb_index));
            }
            EmitterColumn::TimestampDecomp { apc_col, curr_off, prev_off, delta, limb_index } => {
                ts_groups
                    .entry((*curr_off, *prev_off, *delta))
                    .or_default()
                    .push((*apc_col, *limb_index));
            }
            _ => others.push(col),
        }
    }

    // Emit fused ALU groups.
    for ((opcode_off, b_off, c_off), limbs) in &alu_groups {
        if limbs.len() == 1 {
            let (apc_col, limb_index) = limbs[0];
            emit_column_body(
                s,
                i,
                &EmitterColumn::AluResult {
                    apc_col,
                    opcode_off: *opcode_off,
                    b_off: *b_off,
                    c_off: *c_off,
                    limb_index,
                },
                8,
            );
        } else {
            writeln!(s, "        {{").unwrap();
            writeln!(s, "            unsigned char a[4];").unwrap();
            writeln!(
                s,
                "            powdr_run_alu(rec{}[{}u], rec{} + {}u, rec{} + {}u, a);",
                i, opcode_off, i, b_off, i, c_off
            ).unwrap();
            for (apc_col, limb_index) in limbs {
                writeln!(
                    s,
                    "            d_output[(unsigned long long){}u * H + r] = to_monty((unsigned int)a[{}u]);",
                    apc_col, limb_index
                ).unwrap();
            }
            writeln!(s, "        }}").unwrap();
        }
    }

    // Emit fused Shift groups.
    for ((opcode_off, b_off, c_off), limbs) in &shift_groups {
        if limbs.len() == 1 {
            let (apc_col, limb_index) = limbs[0];
            emit_column_body(
                s,
                i,
                &EmitterColumn::ShiftResult {
                    apc_col,
                    opcode_off: *opcode_off,
                    b_off: *b_off,
                    c_off: *c_off,
                    limb_index,
                },
                8,
            );
        } else {
            writeln!(s, "        {{").unwrap();
            writeln!(s, "            unsigned char a[4];").unwrap();
            writeln!(
                s,
                "            powdr_run_shift(rec{}[{}u], rec{} + {}u, rec{} + {}u, a);",
                i, opcode_off, i, b_off, i, c_off
            ).unwrap();
            for (apc_col, limb_index) in limbs {
                writeln!(
                    s,
                    "            d_output[(unsigned long long){}u * H + r] = to_monty((unsigned int)a[{}u]);",
                    apc_col, limb_index
                ).unwrap();
            }
            writeln!(s, "        }}").unwrap();
        }
    }

    // Emit fused TimestampDecomp groups (share curr/prev/delta -> one diff).
    for ((curr_off, prev_off, delta), limbs) in &ts_groups {
        if limbs.len() == 1 {
            let (apc_col, limb_index) = limbs[0];
            emit_column_body(
                s,
                i,
                &EmitterColumn::TimestampDecomp {
                    apc_col,
                    curr_off: *curr_off,
                    prev_off: *prev_off,
                    delta: *delta,
                    limb_index,
                },
                8,
            );
        } else {
            writeln!(s, "        {{").unwrap();
            writeln!(s, "            unsigned int curr = *(const unsigned int*)(rec{} + {}u);", i, curr_off).unwrap();
            writeln!(s, "            unsigned int prev = *(const unsigned int*)(rec{} + {}u);", i, prev_off).unwrap();
            writeln!(s, "            unsigned int diff = (curr + {}u) - prev - 1u;", delta).unwrap();
            writeln!(s, "            unsigned int mask = (1u << range_max_bits) - 1u;", ).unwrap();
            for (apc_col, limb_index) in limbs {
                writeln!(
                    s,
                    "            d_output[(unsigned long long){}u * H + r] = to_monty((diff >> (range_max_bits * {}u)) & mask);",
                    apc_col, limb_index
                ).unwrap();
            }
            writeln!(s, "        }}").unwrap();
        }
    }

    // Non-fusable columns last (preserves original relative order among them).
    for col in others {
        emit_column(s, i, col);
    }
}

/// Emit the full statements for one column. Some arms need temporaries; each
/// is wrapped in its own `{ ... }` block to keep names from colliding. The
/// output buffer is pre-zeroed by the caller, so `Conditional` can emit a
/// guarded write with no explicit else branch.
fn emit_column(s: &mut String, i: usize, col: &EmitterColumn) {
    match col {
        EmitterColumn::Conditional { cond_off, inner } => {
            writeln!(
                s,
                "        if (rec{}[{}u] != 0u) {{",
                i, cond_off
            )
            .unwrap();
            emit_column_body(s, i, inner, /* indent */ 12);
            writeln!(s, "        }}").unwrap();
        }
        _ => emit_column_body(s, i, col, /* indent */ 8),
    }
}

fn emit_column_body(s: &mut String, i: usize, col: &EmitterColumn, indent: usize) {
    let pad = " ".repeat(indent);
    match col {
        EmitterColumn::DirectU32 { apc_col, off } => {
            writeln!(
                s,
                "{}d_output[(unsigned long long){}u * H + r] = to_monty(*(const unsigned int*)(rec{} + {}u));",
                pad, apc_col, i, off
            )
            .unwrap();
        }
        EmitterColumn::DirectU8 { apc_col, off } => {
            writeln!(
                s,
                "{}d_output[(unsigned long long){}u * H + r] = to_monty((unsigned int)rec{}[{}u]);",
                pad, apc_col, i, off
            )
            .unwrap();
        }
        EmitterColumn::DirectU16 { apc_col, off } => {
            writeln!(
                s,
                "{}d_output[(unsigned long long){}u * H + r] = to_monty((unsigned int)(*(const unsigned short*)(rec{} + {}u)));",
                pad, apc_col, i, off
            )
            .unwrap();
        }
        EmitterColumn::Constant { apc_col, value } => {
            writeln!(
                s,
                "{}d_output[(unsigned long long){}u * H + r] = 0x{:08x}u; // monty({})",
                pad, apc_col, host_to_monty(*value), value
            )
            .unwrap();
        }
        EmitterColumn::BoolFromOpcodeFolded { apc_col, value } => {
            let v = *value as u32;
            writeln!(
                s,
                "{}d_output[(unsigned long long){}u * H + r] = 0x{:08x}u; // monty({})",
                pad, apc_col, host_to_monty(v), v
            )
            .unwrap();
        }
        EmitterColumn::TimestampDecomp {
            apc_col,
            curr_off,
            prev_off,
            delta,
            limb_index,
        } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    unsigned int curr = *(const unsigned int*)(rec{} + {}u);", pad, i, curr_off).unwrap();
            writeln!(s, "{}    unsigned int prev = *(const unsigned int*)(rec{} + {}u);", pad, i, prev_off).unwrap();
            writeln!(s, "{}    unsigned int diff = (curr + {}u) - prev - 1u;", pad, delta).unwrap();
            writeln!(s, "{}    unsigned int mask = (1u << range_max_bits) - 1u;", pad).unwrap();
            writeln!(s, "{}    unsigned int v = (diff >> (range_max_bits * {}u)) & mask;", pad, limb_index).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            )
            .unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::PointerLimb {
            apc_col,
            val_off,
            imm_off,
            imm_sign_off,
            limb_index,
        } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    unsigned int val = *(const unsigned int*)(rec{} + {}u);", pad, i, val_off).unwrap();
            writeln!(s, "{}    unsigned short imm = *(const unsigned short*)(rec{} + {}u);", pad, i, imm_off).unwrap();
            writeln!(s, "{}    unsigned int sign = (unsigned int)rec{}[{}u];", pad, i, imm_sign_off).unwrap();
            writeln!(s, "{}    unsigned int imm_ext = (unsigned int)imm + (sign ? 0xFFFF0000u : 0u);", pad).unwrap();
            writeln!(s, "{}    unsigned int ptr = val + imm_ext;", pad).unwrap();
            writeln!(s, "{}    unsigned int v = (ptr >> (16u * {}u)) & 0xFFFFu;", pad, limb_index).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            )
            .unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::AluResult {
            apc_col,
            opcode_off,
            b_off,
            c_off,
            limb_index,
        } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    unsigned char a[4];", pad).unwrap();
            writeln!(
                s,
                "{}    powdr_run_alu(rec{}[{}u], rec{} + {}u, rec{} + {}u, a);",
                pad, i, opcode_off, i, b_off, i, c_off
            ).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty((unsigned int)a[{}u]);",
                pad, apc_col, limb_index
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::ShiftResult {
            apc_col,
            opcode_off,
            b_off,
            c_off,
            limb_index,
        } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    unsigned char a[4];", pad).unwrap();
            writeln!(
                s,
                "{}    powdr_run_shift(rec{}[{}u], rec{} + {}u, rec{} + {}u, a);",
                pad, i, opcode_off, i, b_off, i, c_off
            ).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty((unsigned int)a[{}u]);",
                pad, apc_col, limb_index
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::ShiftBitMulLeft { apc_col, opcode_off, c_off } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    bool is_sll = rec{}[{}u] == 0u;", pad, i, opcode_off).unwrap();
            writeln!(s, "{}    int ls, bs; powdr_get_shift_amounts(rec{}[{}u], &ls, &bs);", pad, i, c_off).unwrap();
            writeln!(s, "{}    unsigned int v = is_sll ? (1u << bs) : 0u;", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::ShiftBitMulRight { apc_col, opcode_off, c_off } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    bool is_sll = rec{}[{}u] == 0u;", pad, i, opcode_off).unwrap();
            writeln!(s, "{}    int ls, bs; powdr_get_shift_amounts(rec{}[{}u], &ls, &bs);", pad, i, c_off).unwrap();
            writeln!(s, "{}    unsigned int v = is_sll ? 0u : (1u << bs);", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::ShiftBSign { apc_col, opcode_off, b_off } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    bool is_sra = rec{}[{}u] == 2u;", pad, i, opcode_off).unwrap();
            writeln!(
                s,
                "{}    unsigned int v = is_sra ? (unsigned int)(rec{}[{}u + 3u] >> 7) : 0u;",
                pad, i, b_off
            ).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::ShiftBitMarker { apc_col, c_off, marker_index } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    int ls, bs; powdr_get_shift_amounts(rec{}[{}u], &ls, &bs);", pad, i, c_off).unwrap();
            writeln!(s, "{}    unsigned int v = (bs == {}) ? 1u : 0u;", pad, marker_index).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::ShiftLimbMarker { apc_col, c_off, marker_index } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    int ls, bs; powdr_get_shift_amounts(rec{}[{}u], &ls, &bs);", pad, i, c_off).unwrap();
            writeln!(s, "{}    unsigned int v = (ls == {}) ? 1u : 0u;", pad, marker_index).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::ShiftBitCarry { apc_col, opcode_off, b_off, c_off, limb_index } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    bool is_sll = rec{}[{}u] == 0u;", pad, i, opcode_off).unwrap();
            writeln!(s, "{}    int ls, bs; powdr_get_shift_amounts(rec{}[{}u], &ls, &bs);", pad, i, c_off).unwrap();
            writeln!(s, "{}    unsigned int v = 0u;", pad).unwrap();
            writeln!(s, "{}    if (bs != 0) {{", pad).unwrap();
            writeln!(
                s,
                "{}        unsigned char bv = rec{}[{}u + {}u];",
                pad, i, b_off, limb_index
            ).unwrap();
            writeln!(
                s,
                "{}        unsigned char carry = is_sll ? (unsigned char)(bv >> (8 - bs)) : (unsigned char)(bv & ((1u << bs) - 1u));",
                pad
            ).unwrap();
            writeln!(s, "{}        v = (unsigned int)carry;", pad).unwrap();
            writeln!(s, "{}    }}", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::BranchEqualCmpResult { apc_col, a_off, b_off, opcode_off } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    const unsigned char* aa = rec{} + {}u;", pad, i, a_off).unwrap();
            writeln!(s, "{}    const unsigned char* bb = rec{} + {}u;", pad, i, b_off).unwrap();
            writeln!(s, "{}    bool is_beq = rec{}[{}u] == 0u;", pad, i, opcode_off).unwrap();
            writeln!(
                s,
                "{}    bool eq = (aa[0]==bb[0] && aa[1]==bb[1] && aa[2]==bb[2] && aa[3]==bb[3]);",
                pad
            ).unwrap();
            writeln!(s, "{}    unsigned int v = (is_beq ? eq : !eq) ? 1u : 0u;", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::BranchEqualDiffInvMarker { apc_col, a_off, b_off, marker_index } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    const unsigned char* aa = rec{} + {}u;", pad, i, a_off).unwrap();
            writeln!(s, "{}    const unsigned char* bb = rec{} + {}u;", pad, i, b_off).unwrap();
            writeln!(s, "{}    int diff_idx = 4;", pad).unwrap();
            writeln!(s, "{}    for (int k = 0; k < 4; ++k) {{ if (aa[k] != bb[k]) {{ diff_idx = k; break; }} }}", pad).unwrap();
            writeln!(s, "{}    if (diff_idx == 4) diff_idx = 0;", pad).unwrap();
            writeln!(
                s,
                "{}    bool all_eq = (aa[0]==bb[0] && aa[1]==bb[1] && aa[2]==bb[2] && aa[3]==bb[3]);",
                pad
            ).unwrap();
            writeln!(s, "{}    unsigned int monty = 0u;", pad).unwrap();
            writeln!(
                s,
                "{}    if (diff_idx == {} && !all_eq) {{",
                pad, marker_index
            ).unwrap();
            // av = Monty(aa[diff_idx]); bv = Monty(bb[diff_idx]); diff_monty = av - bv (mod P).
            // Then monty(inv) = Monty(canonical_inv(canonical(diff_monty))).
            writeln!(
                s,
                "{}        unsigned int av = to_monty_canonical((unsigned int)aa[diff_idx]);",
                pad
            ).unwrap();
            writeln!(
                s,
                "{}        unsigned int bv = to_monty_canonical((unsigned int)bb[diff_idx]);",
                pad
            ).unwrap();
            writeln!(s, "{}        constexpr unsigned int P = 0x78000001u;", pad).unwrap();
            writeln!(
                s,
                "{}        unsigned int diff_monty = (av >= bv) ? (av - bv) : (P - (bv - av));",
                pad
            ).unwrap();
            // Convert diff_monty (Monty) → canonical via monty_reduce(diff_monty * 1).
            writeln!(
                s,
                "{}        unsigned int diff_canon = monty_reduce((unsigned long long)diff_monty);",
                pad
            ).unwrap();
            writeln!(s, "{}        unsigned int inv_canon = powdr_canonical_inv(diff_canon);", pad).unwrap();
            writeln!(s, "{}        monty = to_monty_canonical(inv_canon);", pad).unwrap();
            writeln!(s, "{}    }}", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = monty;",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::LoadStoreRdRs2Ptr { apc_col, off } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(
                s,
                "{}    unsigned int v = *(const unsigned int*)(rec{} + {}u);",
                pad, i, off
            ).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = (v == 0xFFFFFFFFu) ? 0u : to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::LoadStoreNeedsWrite { apc_col, off } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(
                s,
                "{}    unsigned int v = *(const unsigned int*)(rec{} + {}u);",
                pad, i, off
            ).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v != 0xFFFFFFFFu ? 1u : 0u);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::LoadStoreWriteAuxPrevTs { apc_col, write_prev_ts_off, rd_rs2_ptr_off } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(
                s,
                "{}    unsigned int rd = *(const unsigned int*)(rec{} + {}u);",
                pad, i, rd_rs2_ptr_off
            ).unwrap();
            writeln!(s, "{}    unsigned int monty = 0u;", pad).unwrap();
            writeln!(s, "{}    if (rd != 0xFFFFFFFFu) {{", pad).unwrap();
            writeln!(
                s,
                "{}        unsigned int v = *(const unsigned int*)(rec{} + {}u);",
                pad, i, write_prev_ts_off
            ).unwrap();
            writeln!(s, "{}        monty = to_monty(v);", pad).unwrap();
            writeln!(s, "{}    }}", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = monty;",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::LoadStoreWriteAuxDecomp {
            apc_col,
            from_ts_off,
            write_prev_ts_off,
            rd_rs2_ptr_off,
            limb_index,
        } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(
                s,
                "{}    unsigned int rd = *(const unsigned int*)(rec{} + {}u);",
                pad, i, rd_rs2_ptr_off
            ).unwrap();
            writeln!(s, "{}    unsigned int monty = 0u;", pad).unwrap();
            writeln!(s, "{}    if (rd != 0xFFFFFFFFu) {{", pad).unwrap();
            writeln!(
                s,
                "{}        unsigned int curr = *(const unsigned int*)(rec{} + {}u);",
                pad, i, from_ts_off
            ).unwrap();
            writeln!(
                s,
                "{}        unsigned int prev = *(const unsigned int*)(rec{} + {}u);",
                pad, i, write_prev_ts_off
            ).unwrap();
            writeln!(s, "{}        unsigned int diff = (curr + 2u) - prev - 1u;", pad).unwrap();
            writeln!(s, "{}        unsigned int mask = (1u << range_max_bits) - 1u;", pad).unwrap();
            writeln!(
                s,
                "{}        unsigned int v = (diff >> (range_max_bits * {}u)) & mask;",
                pad, limb_index
            ).unwrap();
            writeln!(s, "{}        monty = to_monty(v);", pad).unwrap();
            writeln!(s, "{}    }}", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = monty;",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::LoadStoreIsLoad { apc_col, opcode_off } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(
                s,
                "{}    unsigned int v = (rec{}[{}u] <= 2u) ? 1u : 0u;",
                pad, i, opcode_off
            ).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(v);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::LoadStoreFlag { apc_col, opcode_off, shift_off, flag_index } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    unsigned char op = rec{}[{}u];", pad, i, opcode_off).unwrap();
            writeln!(s, "{}    unsigned char sh = rec{}[{}u];", pad, i, shift_off).unwrap();
            writeln!(s, "{}    unsigned char flags[4] = {{0u, 0u, 0u, 0u}};", pad).unwrap();
            writeln!(s, "{}    switch (op) {{", pad).unwrap();
            writeln!(s, "{}        case 0u: flags[0]=2u; break;", pad).unwrap();
            writeln!(s, "{}        case 2u: if (sh==0u) flags[1]=2u; else if (sh==2u) flags[2]=2u; break;", pad).unwrap();
            writeln!(s, "{}        case 1u: switch(sh){{case 0u:flags[3]=2u;break;case 1u:flags[0]=1u;break;case 2u:flags[1]=1u;break;case 3u:flags[2]=1u;break;}} break;", pad).unwrap();
            writeln!(s, "{}        case 3u: flags[3]=1u; break;", pad).unwrap();
            writeln!(s, "{}        case 4u: if (sh==0u){{flags[0]=1u;flags[1]=1u;}} else if (sh==2u){{flags[0]=1u;flags[2]=1u;}} break;", pad).unwrap();
            writeln!(s, "{}        case 5u: switch(sh){{case 0u:flags[0]=1u;flags[3]=1u;break;case 1u:flags[1]=1u;flags[2]=1u;break;case 2u:flags[1]=1u;flags[3]=1u;break;case 3u:flags[2]=1u;flags[3]=1u;break;}} break;", pad).unwrap();
            writeln!(s, "{}    }}", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty((unsigned int)flags[{}u]);",
                pad, apc_col, flag_index
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        EmitterColumn::LoadStoreWriteData {
            apc_col,
            opcode_off,
            shift_off,
            read_data_off,
            prev_data_off,
            limb_index,
        } => {
            writeln!(s, "{}{{", pad).unwrap();
            writeln!(s, "{}    unsigned char op = rec{}[{}u];", pad, i, opcode_off).unwrap();
            writeln!(s, "{}    int sh = (int)rec{}[{}u];", pad, i, shift_off).unwrap();
            writeln!(s, "{}    const unsigned char* rd = rec{} + {}u;", pad, i, read_data_off).unwrap();
            writeln!(s, "{}    int idx = {};", pad, limb_index).unwrap();
            writeln!(
                s,
                "{}    unsigned int prev = *(const unsigned int*)(rec{} + {}u + idx*4);",
                pad, i, prev_data_off
            ).unwrap();
            writeln!(s, "{}    unsigned int wd = 0u;", pad).unwrap();
            writeln!(s, "{}    switch (op) {{", pad).unwrap();
            writeln!(s, "{}        case 0u: case 3u: wd = (unsigned int)rd[idx]; break;", pad).unwrap();
            writeln!(s, "{}        case 2u: wd = (idx < 2) ? (unsigned int)rd[idx + sh] : 0u; break;", pad).unwrap();
            writeln!(s, "{}        case 1u: wd = (idx == 0) ? (unsigned int)rd[sh] : 0u; break;", pad).unwrap();
            writeln!(s, "{}        case 4u: wd = (idx >= sh && idx < 2 + sh) ? (unsigned int)rd[idx - sh] : prev; break;", pad).unwrap();
            writeln!(s, "{}        case 5u: wd = (idx == sh) ? (unsigned int)rd[0] : prev; break;", pad).unwrap();
            writeln!(s, "{}    }}", pad).unwrap();
            writeln!(
                s,
                "{}    d_output[(unsigned long long){}u * H + r] = to_monty(wd);",
                pad, apc_col
            ).unwrap();
            writeln!(s, "{}}}", pad).unwrap();
        }
        // Conditional handled at the wrapping level in emit_column.
        EmitterColumn::Conditional { .. } => unreachable!("Conditional handled in emit_column"),
    }
}

/// Compute the BabyBear Montgomery encoding host-side. Mirrors the kernel's
/// `to_monty` exactly so verifier and unit tests compare cleanly.
pub(crate) fn host_to_monty(v: u32) -> u32 {
    const M: u32 = 0x8800_0001;
    const P: u32 = 0x7800_0001;
    const R2: u32 = 1_172_168_163;
    let x = (v as u64) * (R2 as u64);
    let t = x.wrapping_mul(M as u64) & 0xFFFF_FFFF;
    let u = t.wrapping_mul(P as u64);
    let (x_sub_u, overflow) = x.overflowing_sub(u);
    let hi = (x_sub_u >> 32) as u32;
    hi + if overflow { P } else { 0 }
}

const PRELUDE: &str = r#"// Auto-generated by powdr nvrtc_emit. Do not edit.

__device__ __forceinline__ unsigned int monty_reduce(unsigned long long x) {
    constexpr unsigned int M = 0x88000001u;
    constexpr unsigned int P = 0x78000001u;
    unsigned long long t = (x * (unsigned long long)M) & 0xFFFFFFFFull;
    unsigned long long u = t * (unsigned long long)P;
    unsigned long long x_sub_u = x - u;
    bool overflow = x < u;
    unsigned int hi = (unsigned int)(x_sub_u >> 32);
    return hi + (overflow ? P : 0u);
}

__device__ __forceinline__ unsigned int to_monty(unsigned int v) {
    constexpr unsigned int R2 = 1172168163u;
    return monty_reduce((unsigned long long)v * (unsigned long long)R2);
}

// Helpers ported from openvm/cuda/src/apc_jit_tracegen.cu so per-column emit
// stays small. With constant offsets at every callsite, ptxas inlines and
// folds these aggressively.

__device__ __forceinline__ void powdr_run_alu(
    unsigned char opcode,
    const unsigned char* b,
    const unsigned char* c,
    unsigned char* a
) {
    if (opcode == 0u) { // ADD
        unsigned int carry = 0u;
        for (int i = 0; i < 4; i++) {
            unsigned int s = (unsigned int)b[i] + (unsigned int)c[i] + carry;
            a[i] = (unsigned char)(s & 0xFFu);
            carry = s >> 8;
        }
    } else if (opcode == 1u) { // SUB
        unsigned int borrow = 0u;
        for (int i = 0; i < 4; i++) {
            unsigned int rhs = (unsigned int)c[i] + borrow;
            if ((unsigned int)b[i] >= rhs) {
                a[i] = (unsigned char)(b[i] - rhs);
                borrow = 0u;
            } else {
                a[i] = (unsigned char)(256u + b[i] - rhs);
                borrow = 1u;
            }
        }
    } else if (opcode == 2u) {
        for (int i = 0; i < 4; i++) a[i] = b[i] ^ c[i];
    } else if (opcode == 3u) {
        for (int i = 0; i < 4; i++) a[i] = b[i] | c[i];
    } else if (opcode == 4u) {
        for (int i = 0; i < 4; i++) a[i] = b[i] & c[i];
    } else {
        for (int i = 0; i < 4; i++) a[i] = 0u;
    }
}

__device__ __forceinline__ void powdr_get_shift_amounts(
    unsigned char c0, int* limb_shift, int* bit_shift
) {
    int shift = c0 % 32;
    *limb_shift = shift / 8;
    *bit_shift  = shift % 8;
}

// Montgomery field inverse. Input/output are in canonical form (< P). Uses
// Fermat's little theorem: x^(P-2) mod P. Hot enough only in BranchEqualDiffInv.
__device__ __forceinline__ unsigned int powdr_canonical_inv(unsigned int x) {
    constexpr unsigned int P = 0x78000001u;
    if (x == 0u) return 0u;
    // P - 2 = 0x77FFFFFF. Square-and-multiply, MSB to LSB.
    unsigned int e = 0x77FFFFFFu;
    unsigned long long acc = 1ull;
    unsigned long long base = (unsigned long long)x;
    while (e != 0u) {
        if (e & 1u) acc = (acc * base) % (unsigned long long)P;
        base = (base * base) % (unsigned long long)P;
        e >>= 1;
    }
    return (unsigned int)acc;
}

// to_monty_canonical: produce Monty form of a canonical (< P) value.
// Equivalent to to_monty for inputs already in [0, P).
__device__ __forceinline__ unsigned int to_monty_canonical(unsigned int v) {
    return to_monty(v);
}

__device__ __forceinline__ void powdr_run_shift(
    unsigned char opcode,
    const unsigned char* b,
    const unsigned char* c,
    unsigned char* a
) {
    int ls, bs; powdr_get_shift_amounts(c[0], &ls, &bs);
    if (opcode == 0u) { // SLL
        for (int i = 0; i < 4; i++) a[i] = 0u;
        for (int i = ls; i < 4; i++) {
            unsigned short high = (unsigned short)b[i - ls] << bs;
            unsigned short low  = (i > ls) ? ((unsigned short)b[i - ls - 1] >> (8 - bs)) : 0;
            a[i] = (unsigned char)((high | low) & 0xFFu);
        }
    } else { // SRL / SRA
        unsigned char msb = b[3] >> 7;
        unsigned char fill = (opcode == 1u) ? 0u : (unsigned char)(0xFFu * msb);
        for (int i = 0; i < 4; i++) a[i] = fill;
        for (int i = 0; i < 4 - ls; i++) {
            unsigned short p1 = (unsigned short)(b[i + ls] >> bs);
            unsigned short p2v = (i + ls + 1 < 4) ? b[i + ls + 1] : fill;
            unsigned short p2 = (unsigned short)p2v << (8 - bs);
            a[i] = (unsigned char)((p1 | p2) & 0xFFu);
        }
    }
}

"#;

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_input() -> EmitterInput {
        EmitterInput {
            instructions: vec![EmitterInstruction {
                arena_offset: 0,
                record_stride: 64,
                record_offset: 0,
                columns: vec![
                    EmitterColumn::DirectU32 { apc_col: 5, off: 16 },
                    EmitterColumn::DirectU32 { apc_col: 6, off: 20 },
                ],
            }],
        }
    }

    #[test]
    fn emit_is_deterministic() {
        let a = emit_jit_kernel_source(&sample_input());
        let b = emit_jit_kernel_source(&sample_input());
        assert_eq!(a.source, b.source);
        assert_eq!(a.name, b.name);
        assert_eq!(a.source_hash, b.source_hash);
    }

    #[test]
    fn emit_contains_expected_pieces() {
        let k = emit_jit_kernel_source(&sample_input());
        assert!(k.source.contains("to_monty"));
        assert!(k.source.contains(&k.name));
        assert!(k.source.contains("rec0 + 16u"));
        assert!(k.source.contains("rec0 + 20u"));
    }

    #[test]
    fn fusion_groups_alu_limbs_into_single_run_alu_call() {
        let input = EmitterInput {
            instructions: vec![EmitterInstruction {
                arena_offset: 0,
                record_stride: 64,
                record_offset: 0,
                columns: vec![
                    EmitterColumn::AluResult { apc_col: 10, opcode_off: 0, b_off: 16, c_off: 20, limb_index: 0 },
                    EmitterColumn::AluResult { apc_col: 11, opcode_off: 0, b_off: 16, c_off: 20, limb_index: 1 },
                    EmitterColumn::AluResult { apc_col: 12, opcode_off: 0, b_off: 16, c_off: 20, limb_index: 2 },
                    EmitterColumn::AluResult { apc_col: 13, opcode_off: 0, b_off: 16, c_off: 20, limb_index: 3 },
                    EmitterColumn::DirectU32 { apc_col: 5, off: 24 },
                ],
            }],
        };
        let k = emit_jit_kernel_source(&input);
        // Find the kernel body and count run_alu invocations there (excluding
        // the prelude definition).
        let body = k.source.split("__global__").nth(1).expect("kernel body");
        let n_alu_calls = body.matches("powdr_run_alu(rec").count();
        assert_eq!(n_alu_calls, 1, "expected fused single ALU call, got {}\n--- source ---\n{}", n_alu_calls, k.source);
        // Four stores following the call (one per limb), all from the same a[].
        let n_stores_after = body.matches("to_monty((unsigned int)a[").count();
        assert_eq!(n_stores_after, 4);
    }

    #[test]
    fn host_to_monty_matches_known_values() {
        assert_eq!(host_to_monty(0), 0);
        assert_eq!(host_to_monty(1), 268_435_454);
        let p: u32 = 0x7800_0001;
        let r_mod_p: u32 = 268_435_454;
        assert_eq!(host_to_monty(p - 1), p - r_mod_p);
    }
}

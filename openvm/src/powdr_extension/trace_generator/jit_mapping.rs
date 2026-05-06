//! Column-to-record mapping tables for JIT trace generation.
//!
//! Each instruction type (AIR) has a mapping from trace column indices to
//! computations on the raw record bytes stored in the MatrixRecordArena.
//! This allows us to compute only the surviving APC columns directly from
//! record data, bypassing the full `fill_trace_row` pipeline.

use openvm_stark_backend::p3_field::PrimeField32;

/// How to compute a single trace column value from raw record bytes.
#[derive(Debug, Clone)]
pub enum ColumnComputation {
    /// Read a u32 field at the given byte offset in the record.
    /// Produces `F::from_u32(*(u32*)(record + offset))`.
    DirectU32 { record_byte_offset: usize },

    /// Read a single byte at the given byte offset in the record.
    /// Produces `F::from_u8(record[offset])`.
    DirectU8 { record_byte_offset: usize },

    /// Timestamp decomposition: computes one limb of
    /// `decompose((record[curr_ts_offset] + delta) - record[prev_ts_offset] - 1, max_bits)`.
    /// `max_bits` comes from the VariableRangeChecker configuration at runtime.
    /// `limb_index` selects which limb (0 or 1).
    /// `delta` accounts for the timestamp increment within the instruction
    /// (e.g., 0 for first read, 1 for second read, 2 for write).
    TimestampDecomp {
        /// Byte offset of the "current" timestamp (u32) in the record.
        curr_ts_byte_offset: usize,
        /// Delta added to the current timestamp before decomposing.
        curr_ts_delta: u32,
        /// Byte offset of the "previous" timestamp (u32) in the record.
        prev_ts_byte_offset: usize,
        /// Which limb of the decomposition (0 = low, 1 = high).
        limb_index: usize,
    },

    /// ALU result: computes `run_alu(opcode, b, c)[limb_index]`.
    /// `opcode_byte_offset` points to the local_opcode u8 field.
    /// `b_byte_offset` and `c_byte_offset` point to the start of the b[4] and c[4] arrays.
    AluResult {
        opcode_byte_offset: usize,
        b_byte_offset: usize,
        c_byte_offset: usize,
        limb_index: usize,
    },

    /// Boolean from opcode: `F::from_bool(record[opcode_byte_offset] == expected_opcode)`.
    BoolFromOpcode {
        opcode_byte_offset: usize,
        expected_opcode: u8,
    },

    /// Conditional: if `record[condition_byte_offset] != 0`, use `then_comp`; else `F::ZERO`.
    Conditional {
        condition_byte_offset: usize,
        then_comp: Box<ColumnComputation>,
    },

    /// Read a u16 field at the given byte offset in the record.
    DirectU16 { record_byte_offset: usize },

    /// Pointer limb: computes `uint16_t limb of (val + sign_extend(imm, imm_sign))`.
    PointerLimb {
        val_byte_offset: usize,
        imm_byte_offset: usize,
        imm_sign_byte_offset: usize,
        limb_index: usize,
    },

    /// LoadStore: rd_rs2_ptr (0 if UINT32_MAX, otherwise the value)
    LoadStoreRdRs2Ptr { rd_rs2_ptr_byte_offset: usize },

    /// LoadStore: needs_write (rd_rs2_ptr != UINT32_MAX)
    LoadStoreNeedsWrite { rd_rs2_ptr_byte_offset: usize },

    /// LoadStore: write_base_aux.prev_timestamp (conditional on needs_write)
    LoadStoreWriteAuxPrevTs {
        write_prev_ts_byte_offset: usize,
        rd_rs2_ptr_byte_offset: usize,
    },

    /// LoadStore: write_base_aux timestamp decomposition (conditional on needs_write)
    LoadStoreWriteAuxDecomp {
        from_ts_byte_offset: usize,
        write_prev_ts_byte_offset: usize,
        rd_rs2_ptr_byte_offset: usize,
        limb_index: usize,
    },

    /// LoadStore: is_load flag
    LoadStoreIsLoad { opcode_byte_offset: usize },

    /// LoadStore: flags[4] based on opcode and shift
    LoadStoreFlag {
        opcode_byte_offset: usize,
        shift_byte_offset: usize,
        flag_index: usize,
    },

    /// LoadStore: write_data computed from opcode, shift, read_data, prev_data
    LoadStoreWriteData {
        opcode_byte_offset: usize,
        shift_byte_offset: usize,
        read_data_byte_offset: usize,
        prev_data_byte_offset: usize,
        limb_index: usize,
    },

    /// Shift result: a[limb_index] from shift operation
    ShiftResult { opcode_byte_offset: usize, b_byte_offset: usize, c_byte_offset: usize, limb_index: usize },
    /// Shift: bit_multiplier_left = is_sll ? (1 << bit_shift) : 0
    ShiftBitMulLeft { opcode_byte_offset: usize, c_byte_offset: usize },
    /// Shift: bit_multiplier_right = !is_sll ? (1 << bit_shift) : 0
    ShiftBitMulRight { opcode_byte_offset: usize, c_byte_offset: usize },
    /// Shift: b_sign = is_sra ? (b[3] >> 7) : 0
    ShiftBSign { opcode_byte_offset: usize, b_byte_offset: usize },
    /// Shift: bit_shift_marker[marker_index] = (bit_shift == marker_index)
    ShiftBitMarker { c_byte_offset: usize, marker_index: usize },
    /// Shift: limb_shift_marker[marker_index] = (limb_shift == marker_index)
    ShiftLimbMarker { c_byte_offset: usize, marker_index: usize },
    /// Shift: bit_shift_carry[limb_index]
    ShiftBitCarry { opcode_byte_offset: usize, b_byte_offset: usize, c_byte_offset: usize, limb_index: usize },

    /// BranchEqual: cmp_result
    BranchEqualCmpResult { a_byte_offset: usize, b_byte_offset: usize, opcode_byte_offset: usize },
    /// BranchEqual: diff_inv_marker[marker_index]
    BranchEqualDiffInvMarker { a_byte_offset: usize, b_byte_offset: usize, opcode_byte_offset: usize, marker_index: usize },

    // ── LessThanCoreAir<4, 8> (slt / sltu / slti / sltiu) ────────────────────
    // All five arms share the (opcode, b, c) byte-offset signature so a future
    // NVRTC fusion pass can recognise them as a single group and emit one
    // run_less_than per row, indexing into its result.
    /// LessThan: cmp_result (0 or 1).
    LessThanCmpResult { opcode_byte_offset: usize, b_byte_offset: usize, c_byte_offset: usize },
    /// LessThan: diff_val (canonical-form field value).
    LessThanDiffVal { opcode_byte_offset: usize, b_byte_offset: usize, c_byte_offset: usize },
    /// LessThan: diff_marker[marker_index].
    LessThanDiffMarker { opcode_byte_offset: usize, b_byte_offset: usize, c_byte_offset: usize, marker_index: usize },
    /// LessThan: b_msb_f (signed-aware MSB encoding of b[3]).
    LessThanBMsbF { opcode_byte_offset: usize, b_byte_offset: usize },
    /// LessThan: c_msb_f.
    LessThanCMsbF { opcode_byte_offset: usize, c_byte_offset: usize },

    // ── Rv32AuipcCoreAir (auipc) — Rv32RdWriteAdapter ───────────────────────
    /// AUIPC rd_data limb: byte `limb_index` of `(pc + (imm << 8))`.
    /// pc and imm are stored as u32 in the record.
    AuipcRdLimb { pc_byte_offset: usize, imm_byte_offset: usize, limb_index: usize },

    // ── Rv32JalrCoreAir (jalr) — Rv32JalrAdapter ────────────────────────────
    /// JALR `to_pc_least_sig_bit`: `(rs1 + imm + (imm_sign ? 0xFFFF_0000 : 0)) & 1`.
    JalrToPcLsb {
        rs1_byte_offset: usize,
        imm_byte_offset: usize,
        imm_sign_byte_offset: usize,
    },
    /// JALR `to_pc_limbs[limb_index]`. Layout:
    /// - limb 0: `(to_pc & 0xFFFF) >> 1`
    /// - limb 1: `to_pc >> 16`
    JalrToPcLimb {
        rs1_byte_offset: usize,
        imm_byte_offset: usize,
        imm_sign_byte_offset: usize,
        limb_index: usize,
    },
    /// JALR rd_data limb: byte `limb_index + 1` of `(pc + 4)` for the top 3
    /// limbs the chip writes (`limb_index ∈ 0..3`).
    JalrRdLimb { pc_byte_offset: usize, limb_index: usize },

    /// Like [`Conditional`] but the gate is "u32 at `ptr_byte_offset`
    /// is not `u32::MAX`" — used for adapter rd_aux fields that are only
    /// populated when the chip actually performs a write.
    ConditionalNotMaxU32 {
        ptr_byte_offset: usize,
        then_comp: Box<ColumnComputation>,
    },

    // ── BranchLessThanCoreAir<4, 8> (blt / bltu / bge / bgeu) ────────────────
    /// BranchLt: cmp_result (1 if branch taken).
    BranchLtCmpResult { opcode_byte_offset: usize, a_byte_offset: usize, b_byte_offset: usize },
    /// BranchLt: cmp_lt (1 if a < b irrespective of bge/blt).
    BranchLtCmpLt { opcode_byte_offset: usize, a_byte_offset: usize, b_byte_offset: usize },
    /// BranchLt: diff_val.
    BranchLtDiffVal { opcode_byte_offset: usize, a_byte_offset: usize, b_byte_offset: usize },
    /// BranchLt: diff_marker[marker_index].
    BranchLtDiffMarker { opcode_byte_offset: usize, a_byte_offset: usize, b_byte_offset: usize, marker_index: usize },
    /// BranchLt: a_msb_f.
    BranchLtAMsbF { opcode_byte_offset: usize, a_byte_offset: usize },
    /// BranchLt: b_msb_f.
    BranchLtBMsbF { opcode_byte_offset: usize, b_byte_offset: usize },

    /// Constant value.
    Constant(u32),
}

/// A single column mapping entry: which column index maps to which computation.
#[derive(Debug, Clone)]
pub struct ColumnMapping {
    /// The column index in the original AIR trace (= `original_poly_index` in Substitution).
    pub col_index: usize,
    /// How to compute this column from the record bytes.
    pub computation: ColumnComputation,
}

/// Complete mapping table for one AIR type.
#[derive(Debug, Clone)]
pub struct AirColumnMapping {
    /// Human-readable AIR name for debugging.
    pub air_name: &'static str,
    /// Total width (number of columns) of this AIR.
    pub width: usize,
    /// Size of the record in bytes (adapter + core, including padding).
    pub record_byte_size: usize,
    /// Mapping for each column. Indexed by column index.
    pub columns: Vec<ColumnMapping>,
}

/// Evaluate a column computation given the raw record bytes.
pub fn eval_column<F: PrimeField32>(comp: &ColumnComputation, record: &[u8], range_max_bits: u32) -> F {
    match comp {
        ColumnComputation::DirectU32 { record_byte_offset } => {
            let bytes = &record[*record_byte_offset..*record_byte_offset + 4];
            let val = u32::from_le_bytes(bytes.try_into().unwrap());
            F::from_u32(val)
        }
        ColumnComputation::DirectU8 { record_byte_offset } => {
            F::from_u8(record[*record_byte_offset])
        }
        ColumnComputation::TimestampDecomp {
            curr_ts_byte_offset,
            curr_ts_delta,
            prev_ts_byte_offset,
            limb_index,
        } => {
            let curr = u32::from_le_bytes(
                record[*curr_ts_byte_offset..*curr_ts_byte_offset + 4]
                    .try_into()
                    .unwrap(),
            );
            let prev = u32::from_le_bytes(
                record[*prev_ts_byte_offset..*prev_ts_byte_offset + 4]
                    .try_into()
                    .unwrap(),
            );
            let diff = (curr + curr_ts_delta).wrapping_sub(prev).wrapping_sub(1);
            let mask = (1u32 << range_max_bits) - 1;
            let limb = (diff >> (range_max_bits * *limb_index as u32)) & mask;
            F::from_u32(limb)
        }
        ColumnComputation::AluResult {
            opcode_byte_offset,
            b_byte_offset,
            c_byte_offset,
            limb_index,
        } => {
            let opcode = record[*opcode_byte_offset];
            let b = &record[*b_byte_offset..*b_byte_offset + 4];
            let c = &record[*c_byte_offset..*c_byte_offset + 4];
            let result = run_alu_byte(opcode, b, c);
            F::from_u8(result[*limb_index])
        }
        ColumnComputation::BoolFromOpcode {
            opcode_byte_offset,
            expected_opcode,
        } => F::from_bool(record[*opcode_byte_offset] == *expected_opcode),
        ColumnComputation::Conditional {
            condition_byte_offset,
            then_comp,
        } => {
            if record[*condition_byte_offset] != 0 {
                eval_column(then_comp, record, range_max_bits)
            } else {
                F::ZERO
            }
        }
        ColumnComputation::DirectU16 { record_byte_offset } => {
            let bytes = &record[*record_byte_offset..*record_byte_offset + 2];
            let val = u16::from_le_bytes(bytes.try_into().unwrap());
            F::from_u32(val as u32)
        }
        ColumnComputation::PointerLimb {
            val_byte_offset,
            imm_byte_offset,
            imm_sign_byte_offset,
            limb_index,
        } => {
            let val = u32::from_le_bytes(
                record[*val_byte_offset..*val_byte_offset + 4].try_into().unwrap(),
            );
            let imm = u16::from_le_bytes(
                record[*imm_byte_offset..*imm_byte_offset + 2].try_into().unwrap(),
            ) as u32;
            let sign = record[*imm_sign_byte_offset];
            let imm_ext = imm + if sign != 0 { 0xFFFF0000u32 } else { 0 };
            let ptr = val.wrapping_add(imm_ext);
            let limb = ((ptr >> (16 * *limb_index as u32)) & 0xFFFF) as u32;
            F::from_u32(limb)
        }
        ColumnComputation::LoadStoreRdRs2Ptr { rd_rs2_ptr_byte_offset } => {
            let val = u32::from_le_bytes(
                record[*rd_rs2_ptr_byte_offset..*rd_rs2_ptr_byte_offset + 4]
                    .try_into().unwrap(),
            );
            if val == u32::MAX { F::ZERO } else { F::from_u32(val) }
        }
        ColumnComputation::LoadStoreNeedsWrite { rd_rs2_ptr_byte_offset } => {
            let val = u32::from_le_bytes(
                record[*rd_rs2_ptr_byte_offset..*rd_rs2_ptr_byte_offset + 4]
                    .try_into().unwrap(),
            );
            F::from_bool(val != u32::MAX)
        }
        ColumnComputation::LoadStoreWriteAuxPrevTs {
            write_prev_ts_byte_offset,
            rd_rs2_ptr_byte_offset,
        } => {
            let rd_rs2 = u32::from_le_bytes(
                record[*rd_rs2_ptr_byte_offset..*rd_rs2_ptr_byte_offset + 4]
                    .try_into().unwrap(),
            );
            if rd_rs2 == u32::MAX {
                F::ZERO
            } else {
                let val = u32::from_le_bytes(
                    record[*write_prev_ts_byte_offset..*write_prev_ts_byte_offset + 4]
                        .try_into().unwrap(),
                );
                F::from_u32(val)
            }
        }
        ColumnComputation::LoadStoreWriteAuxDecomp {
            from_ts_byte_offset,
            write_prev_ts_byte_offset,
            rd_rs2_ptr_byte_offset,
            limb_index,
        } => {
            let rd_rs2 = u32::from_le_bytes(
                record[*rd_rs2_ptr_byte_offset..*rd_rs2_ptr_byte_offset + 4]
                    .try_into().unwrap(),
            );
            if rd_rs2 == u32::MAX {
                F::ZERO
            } else {
                let curr = u32::from_le_bytes(
                    record[*from_ts_byte_offset..*from_ts_byte_offset + 4]
                        .try_into().unwrap(),
                );
                let prev = u32::from_le_bytes(
                    record[*write_prev_ts_byte_offset..*write_prev_ts_byte_offset + 4]
                        .try_into().unwrap(),
                );
                let diff = (curr + 2).wrapping_sub(prev).wrapping_sub(1);
                let mask = (1u32 << range_max_bits) - 1;
                let limb = (diff >> (range_max_bits * *limb_index as u32)) & mask;
                F::from_u32(limb)
            }
        }
        ColumnComputation::LoadStoreIsLoad { opcode_byte_offset } => {
            let opcode = record[*opcode_byte_offset];
            // LOADW=0, LOADBU=1, LOADHU=2 are loads
            F::from_bool(opcode <= 2)
        }
        ColumnComputation::LoadStoreFlag {
            opcode_byte_offset,
            shift_byte_offset,
            flag_index,
        } => {
            let opcode = record[*opcode_byte_offset];
            let shift = record[*shift_byte_offset];
            let flags = compute_loadstore_flags(opcode, shift);
            F::from_u32(flags[*flag_index] as u32)
        }
        ColumnComputation::LoadStoreWriteData {
            opcode_byte_offset,
            shift_byte_offset,
            read_data_byte_offset,
            prev_data_byte_offset,
            limb_index,
        } => {
            let opcode = record[*opcode_byte_offset];
            let shift = record[*shift_byte_offset] as usize;
            let rd = &record[*read_data_byte_offset..*read_data_byte_offset + 4];
            let prev = [
                u32::from_le_bytes(record[*prev_data_byte_offset..*prev_data_byte_offset + 4].try_into().unwrap()),
                u32::from_le_bytes(record[*prev_data_byte_offset + 4..*prev_data_byte_offset + 8].try_into().unwrap()),
                u32::from_le_bytes(record[*prev_data_byte_offset + 8..*prev_data_byte_offset + 12].try_into().unwrap()),
                u32::from_le_bytes(record[*prev_data_byte_offset + 12..*prev_data_byte_offset + 16].try_into().unwrap()),
            ];
            let wd = compute_loadstore_write_data(opcode, shift, rd, &prev);
            F::from_u32(wd[*limb_index])
        }
        ColumnComputation::ShiftResult { opcode_byte_offset, b_byte_offset, c_byte_offset, limb_index } => {
            let opcode = record[*opcode_byte_offset];
            let b: [u8; 4] = record[*b_byte_offset..*b_byte_offset + 4].try_into().unwrap();
            let c: [u8; 4] = record[*c_byte_offset..*c_byte_offset + 4].try_into().unwrap();
            let a = run_shift(opcode, &b, &c);
            F::from_u8(a[*limb_index])
        }
        ColumnComputation::ShiftBitMulLeft { opcode_byte_offset, c_byte_offset } => {
            let is_sll = record[*opcode_byte_offset] == 0;
            let (_, bit_shift) = get_shift_amounts(record[*c_byte_offset]);
            if is_sll { F::from_u32(1u32 << bit_shift) } else { F::ZERO }
        }
        ColumnComputation::ShiftBitMulRight { opcode_byte_offset, c_byte_offset } => {
            let is_sll = record[*opcode_byte_offset] == 0;
            let (_, bit_shift) = get_shift_amounts(record[*c_byte_offset]);
            if !is_sll { F::from_u32(1u32 << bit_shift) } else { F::ZERO }
        }
        ColumnComputation::ShiftBSign { opcode_byte_offset, b_byte_offset } => {
            let is_sra = record[*opcode_byte_offset] == 2;
            if is_sra { F::from_u32((record[*b_byte_offset + 3] >> 7) as u32) } else { F::ZERO }
        }
        ColumnComputation::ShiftBitMarker { c_byte_offset, marker_index } => {
            let (_, bit_shift) = get_shift_amounts(record[*c_byte_offset]);
            F::from_bool(bit_shift == *marker_index)
        }
        ColumnComputation::ShiftLimbMarker { c_byte_offset, marker_index } => {
            let (limb_shift, _) = get_shift_amounts(record[*c_byte_offset]);
            F::from_bool(limb_shift == *marker_index)
        }
        ColumnComputation::ShiftBitCarry { opcode_byte_offset, b_byte_offset, c_byte_offset, limb_index } => {
            let is_sll = record[*opcode_byte_offset] == 0;
            let (_, bit_shift) = get_shift_amounts(record[*c_byte_offset]);
            if bit_shift == 0 {
                F::ZERO
            } else {
                let b_val = record[*b_byte_offset + *limb_index];
                let carry = if is_sll {
                    b_val >> (8 - bit_shift as u8)
                } else {
                    b_val & ((1u8 << bit_shift as u8) - 1)
                };
                F::from_u8(carry)
            }
        }
        ColumnComputation::BranchEqualCmpResult { a_byte_offset, b_byte_offset, opcode_byte_offset } => {
            let a = &record[*a_byte_offset..*a_byte_offset + 4];
            let b = &record[*b_byte_offset..*b_byte_offset + 4];
            let is_beq = record[*opcode_byte_offset] == 0;
            let are_equal = a == b;
            F::from_bool(if is_beq { are_equal } else { !are_equal })
        }
        ColumnComputation::BranchEqualDiffInvMarker { a_byte_offset, b_byte_offset, opcode_byte_offset, marker_index } => {
            let a = &record[*a_byte_offset..*a_byte_offset + 4];
            let b = &record[*b_byte_offset..*b_byte_offset + 4];
            // Find first differing limb
            let mut diff_idx = 4usize; // = NUM_LIMBS means equal
            for i in 0..4 {
                if a[i] != b[i] { diff_idx = i; break; }
            }
            if diff_idx == 4 { diff_idx = 0; } // when equal, marker at index 0
            if *marker_index == diff_idx && a != b {
                // diff_inv = inv(a[diff_idx] - b[diff_idx]) in the field
                let a_val = F::from_u8(a[diff_idx]);
                let b_val = F::from_u8(b[diff_idx]);
                (a_val - b_val).inverse()
            } else {
                F::ZERO
            }
        }
        ColumnComputation::LessThanCmpResult { opcode_byte_offset, b_byte_offset, c_byte_offset } => {
            let is_slt = record[*opcode_byte_offset] == 0;
            let b: &[u8] = &record[*b_byte_offset..*b_byte_offset + 4];
            let c: &[u8] = &record[*c_byte_offset..*c_byte_offset + 4];
            let (cmp_result, _, _, _) = run_less_than_host(is_slt, b, c);
            F::from_bool(cmp_result)
        }
        ColumnComputation::LessThanDiffVal { opcode_byte_offset, b_byte_offset, c_byte_offset } => {
            let is_slt = record[*opcode_byte_offset] == 0;
            let b: &[u8] = &record[*b_byte_offset..*b_byte_offset + 4];
            let c: &[u8] = &record[*c_byte_offset..*c_byte_offset + 4];
            let (cmp_result, diff_idx, b_sign, c_sign) = run_less_than_host(is_slt, b, c);
            less_than_diff_val::<F>(cmp_result, diff_idx, b, c, b_sign, c_sign)
        }
        ColumnComputation::LessThanDiffMarker { opcode_byte_offset, b_byte_offset, c_byte_offset, marker_index } => {
            let is_slt = record[*opcode_byte_offset] == 0;
            let b: &[u8] = &record[*b_byte_offset..*b_byte_offset + 4];
            let c: &[u8] = &record[*c_byte_offset..*c_byte_offset + 4];
            let (_, diff_idx, _, _) = run_less_than_host(is_slt, b, c);
            // marker[diff_idx] = 1 only when a != b (diff_idx != 4); else all zero.
            if diff_idx != 4 && diff_idx == *marker_index { F::ONE } else { F::ZERO }
        }
        ColumnComputation::LessThanBMsbF { opcode_byte_offset, b_byte_offset } => {
            let is_slt = record[*opcode_byte_offset] == 0;
            let b3 = record[*b_byte_offset + 3];
            // b_sign = signed-mode AND high bit set.
            let b_sign = is_slt && (b3 >> 7) == 1;
            if b_sign {
                // -F::from_u16((1 << 8) - b3)
                F::ZERO - F::from_u16(256 - b3 as u16)
            } else {
                F::from_u8(b3)
            }
        }
        ColumnComputation::LessThanCMsbF { opcode_byte_offset, c_byte_offset } => {
            let is_slt = record[*opcode_byte_offset] == 0;
            let c3 = record[*c_byte_offset + 3];
            let c_sign = is_slt && (c3 >> 7) == 1;
            if c_sign {
                F::ZERO - F::from_u16(256 - c3 as u16)
            } else {
                F::from_u8(c3)
            }
        }
        ColumnComputation::AuipcRdLimb { pc_byte_offset, imm_byte_offset, limb_index } => {
            let pc = read_u32(record, *pc_byte_offset);
            let imm = read_u32(record, *imm_byte_offset);
            let rd = pc.wrapping_add(imm << 8);
            F::from_u8(rd.to_le_bytes()[*limb_index])
        }
        ColumnComputation::JalrToPcLsb { rs1_byte_offset, imm_byte_offset, imm_sign_byte_offset } => {
            let rs1 = read_u32(record, *rs1_byte_offset);
            let imm = read_u16(record, *imm_byte_offset) as u32;
            let sign = record[*imm_sign_byte_offset] != 0;
            let to_pc = rs1.wrapping_add(imm + if sign { 0xFFFF_0000 } else { 0 });
            F::from_u8((to_pc & 1) as u8)
        }
        ColumnComputation::JalrToPcLimb { rs1_byte_offset, imm_byte_offset, imm_sign_byte_offset, limb_index } => {
            let rs1 = read_u32(record, *rs1_byte_offset);
            let imm = read_u16(record, *imm_byte_offset) as u32;
            let sign = record[*imm_sign_byte_offset] != 0;
            let to_pc = rs1.wrapping_add(imm + if sign { 0xFFFF_0000 } else { 0 });
            let v = match *limb_index {
                0 => (to_pc & 0xFFFF) >> 1,
                1 => to_pc >> 16,
                _ => panic!("JalrToPcLimb: invalid limb_index {limb_index}"),
            };
            F::from_u32(v)
        }
        ColumnComputation::JalrRdLimb { pc_byte_offset, limb_index } => {
            let pc = read_u32(record, *pc_byte_offset);
            // Chip stores top 3 limbs of (pc + 4) — limb_index ∈ 0..3
            // mapping to byte (limb_index + 1) of the little-endian encoding.
            let rd = pc.wrapping_add(4);
            F::from_u8(rd.to_le_bytes()[*limb_index + 1])
        }
        ColumnComputation::ConditionalNotMaxU32 { ptr_byte_offset, then_comp } => {
            let v = read_u32(record, *ptr_byte_offset);
            if v == u32::MAX {
                F::ZERO
            } else {
                eval_column(then_comp, record, range_max_bits)
            }
        }
        ColumnComputation::BranchLtCmpResult { opcode_byte_offset, a_byte_offset, b_byte_offset } => {
            let op = record[*opcode_byte_offset];
            let a: &[u8] = &record[*a_byte_offset..*a_byte_offset + 4];
            let b: &[u8] = &record[*b_byte_offset..*b_byte_offset + 4];
            let (cmp_result, _, _, _) = run_branch_lt_host(op, a, b);
            F::from_bool(cmp_result)
        }
        ColumnComputation::BranchLtCmpLt { opcode_byte_offset, a_byte_offset, b_byte_offset } => {
            let op = record[*opcode_byte_offset];
            let a: &[u8] = &record[*a_byte_offset..*a_byte_offset + 4];
            let b: &[u8] = &record[*b_byte_offset..*b_byte_offset + 4];
            let (cmp_result, _, _, _) = run_branch_lt_host(op, a, b);
            // ge_op: BGE=2 or BGEU=3
            let ge_op = op == 2 || op == 3;
            F::from_bool(cmp_result ^ ge_op)
        }
        ColumnComputation::BranchLtDiffVal { opcode_byte_offset, a_byte_offset, b_byte_offset } => {
            let op = record[*opcode_byte_offset];
            let a: &[u8] = &record[*a_byte_offset..*a_byte_offset + 4];
            let b: &[u8] = &record[*b_byte_offset..*b_byte_offset + 4];
            let (cmp_result, diff_idx, a_sign, b_sign) = run_branch_lt_host(op, a, b);
            let ge_op = op == 2 || op == 3;
            let cmp_lt = cmp_result ^ ge_op;
            branch_lt_diff_val::<F>(cmp_lt, diff_idx, a, b, a_sign, b_sign)
        }
        ColumnComputation::BranchLtDiffMarker { opcode_byte_offset, a_byte_offset, b_byte_offset, marker_index } => {
            let op = record[*opcode_byte_offset];
            let a: &[u8] = &record[*a_byte_offset..*a_byte_offset + 4];
            let b: &[u8] = &record[*b_byte_offset..*b_byte_offset + 4];
            let (_, diff_idx, _, _) = run_branch_lt_host(op, a, b);
            if diff_idx != 4 && diff_idx == *marker_index { F::ONE } else { F::ZERO }
        }
        ColumnComputation::BranchLtAMsbF { opcode_byte_offset, a_byte_offset } => {
            let op = record[*opcode_byte_offset];
            let signed = op == 0 || op == 2; // BLT or BGE
            let a3 = record[*a_byte_offset + 3];
            let a_sign = signed && (a3 >> 7) == 1;
            if a_sign {
                F::ZERO - F::from_u16(256 - a3 as u16)
            } else {
                F::from_u8(a3)
            }
        }
        ColumnComputation::BranchLtBMsbF { opcode_byte_offset, b_byte_offset } => {
            let op = record[*opcode_byte_offset];
            let signed = op == 0 || op == 2;
            let b3 = record[*b_byte_offset + 3];
            let b_sign = signed && (b3 >> 7) == 1;
            if b_sign {
                F::ZERO - F::from_u16(256 - b3 as u16)
            } else {
                F::from_u8(b3)
            }
        }
        ColumnComputation::Constant(val) => F::from_u32(*val),
    }
}

/// Get shift amounts from c[0]: (limb_shift, bit_shift)
fn get_shift_amounts(c0: u8) -> (usize, usize) {
    let max_bits = 4 * 8; // NUM_LIMBS * CELL_BITS = 32
    let shift = (c0 as usize) % max_bits;
    (shift / 8, shift % 8)
}

/// Run shift operation, returning result a[4]
fn run_shift(opcode: u8, b: &[u8; 4], c: &[u8; 4]) -> [u8; 4] {
    let (limb_shift, bit_shift) = get_shift_amounts(c[0]);
    let mut a = [0u8; 4];

    if opcode == 0 {
        // SLL
        for i in limb_shift..4 {
            if i > limb_shift {
                let high = (b[i - limb_shift] as u16) << bit_shift as u16;
                let low = (b[i - limb_shift - 1] as u16) >> (8 - bit_shift) as u16;
                a[i] = ((high | low) & 0xFF) as u8;
            } else {
                a[i] = (((b[i - limb_shift] as u16) << bit_shift as u16) & 0xFF) as u8;
            }
        }
    } else {
        // SRL or SRA
        let is_logical = opcode == 1;
        let msb = b[3] >> 7;
        let fill: u8 = if is_logical { 0 } else { 0xFF * msb };
        for i in 0..4 { a[i] = fill; }
        let limit = 4 - limb_shift;
        for i in 0..limit {
            let part1 = (b[i + limb_shift] >> bit_shift as u8) as u16;
            let part2_val = if i + limb_shift + 1 < 4 { b[i + limb_shift + 1] } else { fill };
            let part2 = (part2_val as u16) << (8 - bit_shift) as u16;
            a[i] = ((part1 | part2) & 0xFF) as u8;
        }
    }
    a
}

/// Read a little-endian u32 from `record[offset..offset+4]`.
#[inline]
fn read_u32(record: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes(record[offset..offset + 4].try_into().unwrap())
}

/// Read a little-endian u16 from `record[offset..offset+2]`.
#[inline]
fn read_u16(record: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes(record[offset..offset + 2].try_into().unwrap())
}

/// Mirror of `LessThanCoreAir::run_less_than` for `NUM_LIMBS=4, LIMB_BITS=8`.
/// Returns `(cmp_result, diff_idx, b_sign, c_sign)`. `diff_idx` is the most
/// significant index where `b[i] != c[i]` (walking from MSB), or 4 when equal.
fn run_less_than_host(is_slt: bool, b: &[u8], c: &[u8]) -> (bool, usize, bool, bool) {
    let b_sign = is_slt && (b[3] >> 7) == 1;
    let c_sign = is_slt && (c[3] >> 7) == 1;
    for i in (0..4).rev() {
        if b[i] != c[i] {
            return ((b[i] < c[i]) ^ b_sign ^ c_sign, i, b_sign, c_sign);
        }
    }
    (false, 4, b_sign, c_sign)
}

/// Mirror of `BranchLessThanCoreAir::run_cmp` for `NUM_LIMBS=4, LIMB_BITS=8`.
/// Local opcodes: BLT=0, BLTU=1, BGE=2, BGEU=3.
fn run_branch_lt_host(local_opcode: u8, a: &[u8], b: &[u8]) -> (bool, usize, bool, bool) {
    let signed = local_opcode == 0 /* BLT */ || local_opcode == 2 /* BGE */;
    let ge_op  = local_opcode == 2 /* BGE */ || local_opcode == 3 /* BGEU */;
    let a_sign = signed && (a[3] >> 7) == 1;
    let b_sign = signed && (b[3] >> 7) == 1;
    for i in (0..4).rev() {
        if a[i] != b[i] {
            return ((a[i] < b[i]) ^ a_sign ^ b_sign ^ ge_op, i, a_sign, b_sign);
        }
    }
    (ge_op, 4, a_sign, b_sign)
}

/// Compute LessThan's `diff_val` field-element value given the precomputed
/// `(cmp_result, diff_idx, b_sign, c_sign)` from `run_less_than_host`.
fn less_than_diff_val<F: PrimeField32>(
    cmp_result: bool,
    diff_idx: usize,
    b: &[u8],
    c: &[u8],
    b_sign: bool,
    c_sign: bool,
) -> F {
    if diff_idx == 4 {
        return F::ZERO;
    }
    if diff_idx == 3 {
        // Use signed-aware MSb encodings
        let b_msb = if b_sign { F::ZERO - F::from_u16(256 - b[3] as u16) } else { F::from_u8(b[3]) };
        let c_msb = if c_sign { F::ZERO - F::from_u16(256 - c[3] as u16) } else { F::from_u8(c[3]) };
        if cmp_result { c_msb - b_msb } else { b_msb - c_msb }
    } else if cmp_result {
        F::from_u8(c[diff_idx] - b[diff_idx])
    } else {
        F::from_u8(b[diff_idx] - c[diff_idx])
    }
}

/// Compute BranchLt's `diff_val` field-element value.
fn branch_lt_diff_val<F: PrimeField32>(
    cmp_lt: bool,
    diff_idx: usize,
    a: &[u8],
    b: &[u8],
    a_sign: bool,
    b_sign: bool,
) -> F {
    if diff_idx == 4 {
        return F::ZERO;
    }
    if diff_idx == 3 {
        let a_msb = if a_sign { F::ZERO - F::from_u16(256 - a[3] as u16) } else { F::from_u8(a[3]) };
        let b_msb = if b_sign { F::ZERO - F::from_u16(256 - b[3] as u16) } else { F::from_u8(b[3]) };
        if cmp_lt { b_msb - a_msb } else { a_msb - b_msb }
    } else if cmp_lt {
        F::from_u8(b[diff_idx] - a[diff_idx])
    } else {
        F::from_u8(a[diff_idx] - b[diff_idx])
    }
}

/// Compute LoadStore flags[4] based on opcode and shift.
fn compute_loadstore_flags(opcode: u8, shift: u8) -> [u8; 4] {
    let mut flags = [0u8; 4];
    match opcode {
        0 => flags[0] = 2,                                      // LOADW
        2 => match shift { 0 => flags[1] = 2, 2 => flags[2] = 2, _ => {} } // LOADHU
        1 => match shift {                                       // LOADBU
            0 => flags[3] = 2, 1 => flags[0] = 1, 2 => flags[1] = 1, 3 => flags[2] = 1, _ => {}
        }
        3 => flags[3] = 1,                                      // STOREW
        4 => match shift {                                       // STOREH
            0 => { flags[0] = 1; flags[1] = 1; }
            2 => { flags[0] = 1; flags[2] = 1; }
            _ => {}
        }
        5 => match shift {                                       // STOREB
            0 => { flags[0] = 1; flags[3] = 1; }
            1 => { flags[1] = 1; flags[2] = 1; }
            2 => { flags[1] = 1; flags[3] = 1; }
            3 => { flags[2] = 1; flags[3] = 1; }
            _ => {}
        }
        _ => {} // LOADB=6, LOADH=7 (sign extension variants)
    }
    flags
}

/// Compute LoadStore write_data[4] based on opcode, shift, read_data, prev_data.
fn compute_loadstore_write_data(opcode: u8, shift: usize, rd: &[u8], prev: &[u32; 4]) -> [u32; 4] {
    let mut wd = [0u32; 4];
    match opcode {
        0 => { // LOADW
            for i in 0..4 { wd[i] = rd[i] as u32; }
        }
        2 => { // LOADHU
            for i in 0..2 { wd[i] = rd[i + shift] as u32; }
        }
        1 => { // LOADBU
            wd[0] = rd[shift] as u32;
        }
        3 => { // STOREW
            for i in 0..4 { wd[i] = rd[i] as u32; }
        }
        4 => { // STOREH
            for i in 0..4 {
                if i >= shift && i < 2 + shift {
                    wd[i] = rd[i - shift] as u32;
                } else {
                    wd[i] = prev[i];
                }
            }
        }
        5 => { // STOREB
            for i in 0..4 { wd[i] = prev[i]; }
            wd[shift] = rd[0] as u32;
        }
        _ => {} // LOADB, LOADH (sign extension) — TODO
    }
    wd
}

/// Run ALU operation on 4-byte limbs, returning the result.
fn run_alu_byte(opcode: u8, b: &[u8], c: &[u8]) -> [u8; 4] {
    let mut a = [0u8; 4];
    match opcode {
        0 => {
            // ADD
            let mut carry = 0u32;
            for i in 0..4 {
                let sum = b[i] as u32 + c[i] as u32 + carry;
                a[i] = (sum & 0xFF) as u8;
                carry = sum >> 8;
            }
        }
        1 => {
            // SUB
            let mut borrow = 0u32;
            for i in 0..4 {
                let rhs = c[i] as u32 + borrow;
                if (b[i] as u32) >= rhs {
                    a[i] = (b[i] as u32 - rhs) as u8;
                    borrow = 0;
                } else {
                    a[i] = (256 + b[i] as u32 - rhs) as u8;
                    borrow = 1;
                }
            }
        }
        2 => {
            // XOR
            for i in 0..4 {
                a[i] = b[i] ^ c[i];
            }
        }
        3 => {
            // OR
            for i in 0..4 {
                a[i] = b[i] | c[i];
            }
        }
        4 => {
            // AND
            for i in 0..4 {
                a[i] = b[i] & c[i];
            }
        }
        _ => {}
    }
    a
}

/// Arena type determines how records are laid out in memory.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArenaType {
    /// CPU arena: core starts at `adapter_cols_width * sizeof(F)`.
    Matrix,
    /// GPU arena: core starts at `aligned_adapter_record_size`.
    Dense,
}

/// Core byte offsets per AIR type per arena type.
fn core_byte_offset(air: &str, arena: ArenaType) -> usize {
    match (air, arena) {
        ("BaseAlu", ArenaType::Matrix) => 19 * 4,  // adapter_cols=19
        ("BaseAlu", ArenaType::Dense) => 40,        // aligned_adapter_size=40
        ("Shift", ArenaType::Matrix) => 19 * 4,     // same adapter as BaseAlu
        ("Shift", ArenaType::Dense) => 40,
        ("LessThan", ArenaType::Matrix) => 19 * 4,  // shares Rv32BaseAluAdapter
        ("LessThan", ArenaType::Dense) => 40,
        ("LoadStore", ArenaType::Matrix) => 23 * 4,  // adapter_cols=23
        ("LoadStore", ArenaType::Dense) => 36,       // aligned_adapter_size=36
        ("BranchEqual", ArenaType::Matrix) => 10 * 4, // adapter_cols=10
        ("BranchEqual", ArenaType::Dense) => 24,      // aligned_adapter_size=24
        ("BranchLt", ArenaType::Matrix) => 10 * 4,    // shares Rv32BranchAdapter
        ("BranchLt", ArenaType::Dense) => 24,
        ("Auipc", ArenaType::Matrix) => 10 * 4,    // Rv32RdWriteAdapter (10 cols)
        ("Auipc", ArenaType::Dense) => 20,         // 5 u32s in adapter record
        ("Jalr", ArenaType::Matrix) => 15 * 4,     // Rv32JalrAdapter (15 cols)
        ("Jalr", ArenaType::Dense) => 28,          // 7 u32s in adapter record
        _ => panic!("Unknown AIR type: {air}"),
    }
}

/// Record stride for DenseRecordArena (= aligned_adapter_size + aligned_core_size).
pub fn dense_record_stride(air: &str) -> usize {
    match air {
        "BaseAlu" | "Shift" | "LessThan" => 52,
        "LoadStore" => 60,
        "BranchEqual" | "BranchLt" => 40,
        "Auipc" => 28,    // adapter(20) + core(8: from_pc + imm)
        "Jalr"  => 44,    // adapter(28) + core(16: imm:u16 + 2 pad + from_pc:u32 + rs1_val:u32 + imm_sign:bool + 3 pad)
        _ => panic!("Unknown AIR type: {air}"),
    }
}

// ============================================================================
// Mapping table for BaseAlu (width=36)
// ============================================================================

/// Build the mapping table for Rv32BaseAlu (adapter width=19, core width=17, total=36).
/// Uses MatrixRecordArena layout by default (for CPU path).
pub fn base_alu_mapping() -> AirColumnMapping {
    base_alu_mapping_for(ArenaType::Matrix)
}

/// Build BaseAlu mapping for a specific arena type.
pub fn base_alu_mapping_for(arena: ArenaType) -> AirColumnMapping {
    use ColumnComputation::*;

    // Adapter record byte offsets (within the adapter section, starting at byte 0)
    let from_pc: usize = 0;
    let from_timestamp: usize = 4;
    let rd_ptr: usize = 8;
    let rs1_ptr: usize = 12;
    let rs2: usize = 16;
    let rs2_as: usize = 20; // u8, 1 byte + 3 padding
    let reads_aux_0_prev_ts: usize = 24;
    let reads_aux_1_prev_ts: usize = 28;
    let writes_aux_prev_ts: usize = 32;
    let writes_aux_prev_data_0: usize = 36;

    // Core record byte offsets (within the arena row, starting at core offset)
    let core = core_byte_offset("BaseAlu", arena);
    let b_0: usize = core;
    let c_0: usize = core + 4;
    let local_opcode: usize = core + 8;

    // Timestamp delta: adapter uses from_timestamp, writes at +2, reads_aux[1] at +1
    let columns = vec![
        // Adapter columns (0-18)
        ColumnMapping { col_index: 0, computation: DirectU32 { record_byte_offset: from_pc } },
        ColumnMapping { col_index: 1, computation: DirectU32 { record_byte_offset: from_timestamp } },
        ColumnMapping { col_index: 2, computation: DirectU32 { record_byte_offset: rd_ptr } },
        ColumnMapping { col_index: 3, computation: DirectU32 { record_byte_offset: rs1_ptr } },
        ColumnMapping { col_index: 4, computation: DirectU32 { record_byte_offset: rs2 } },
        ColumnMapping { col_index: 5, computation: DirectU8 { record_byte_offset: rs2_as } },
        // reads_aux[0]: prev_timestamp + decompose(from_ts - prev_ts - 1)
        ColumnMapping { col_index: 6, computation: DirectU32 { record_byte_offset: reads_aux_0_prev_ts } },
        ColumnMapping {
            col_index: 7,
            computation: TimestampDecomp {
                curr_ts_byte_offset: from_timestamp,
                curr_ts_delta: 0,
                prev_ts_byte_offset: reads_aux_0_prev_ts,
                limb_index: 0,
            },
        },
        ColumnMapping {
            col_index: 8,
            computation: TimestampDecomp {
                curr_ts_byte_offset: from_timestamp,
                curr_ts_delta: 0,
                prev_ts_byte_offset: reads_aux_0_prev_ts,
                limb_index: 1,
            },
        },
        // reads_aux[1]: conditional on rs2_as
        ColumnMapping {
            col_index: 9,
            computation: Conditional {
                condition_byte_offset: rs2_as,
                then_comp: Box::new(DirectU32 { record_byte_offset: reads_aux_1_prev_ts }),
            },
        },
        ColumnMapping {
            col_index: 10,
            computation: Conditional {
                condition_byte_offset: rs2_as,
                then_comp: Box::new(TimestampDecomp {
                    curr_ts_byte_offset: from_timestamp,
                    curr_ts_delta: 1,
                    prev_ts_byte_offset: reads_aux_1_prev_ts,
                    limb_index: 0,
                }),
            },
        },
        ColumnMapping {
            col_index: 11,
            computation: Conditional {
                condition_byte_offset: rs2_as,
                then_comp: Box::new(TimestampDecomp {
                    curr_ts_byte_offset: from_timestamp,
                    curr_ts_delta: 1,
                    prev_ts_byte_offset: reads_aux_1_prev_ts,
                    limb_index: 1,
                }),
            },
        },
        // writes_aux: prev_timestamp + decompose(from_ts+2 - prev_ts - 1)
        ColumnMapping { col_index: 12, computation: DirectU32 { record_byte_offset: writes_aux_prev_ts } },
        ColumnMapping {
            col_index: 13,
            computation: TimestampDecomp {
                curr_ts_byte_offset: from_timestamp,
                curr_ts_delta: 2,
                prev_ts_byte_offset: writes_aux_prev_ts,
                limb_index: 0,
            },
        },
        ColumnMapping {
            col_index: 14,
            computation: TimestampDecomp {
                curr_ts_byte_offset: from_timestamp,
                curr_ts_delta: 2,
                prev_ts_byte_offset: writes_aux_prev_ts,
                limb_index: 1,
            },
        },
        // writes_aux.prev_data[0..3]
        ColumnMapping { col_index: 15, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 } },
        ColumnMapping { col_index: 16, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 1 } },
        ColumnMapping { col_index: 17, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 2 } },
        ColumnMapping { col_index: 18, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 3 } },
        // Core columns (19-35)
        // a[0..3] = run_alu(opcode, b, c)
        ColumnMapping { col_index: 19, computation: AluResult { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0, limb_index: 0 } },
        ColumnMapping { col_index: 20, computation: AluResult { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0, limb_index: 1 } },
        ColumnMapping { col_index: 21, computation: AluResult { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0, limb_index: 2 } },
        ColumnMapping { col_index: 22, computation: AluResult { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0, limb_index: 3 } },
        // b[0..3]
        ColumnMapping { col_index: 23, computation: DirectU8 { record_byte_offset: b_0 } },
        ColumnMapping { col_index: 24, computation: DirectU8 { record_byte_offset: b_0 + 1 } },
        ColumnMapping { col_index: 25, computation: DirectU8 { record_byte_offset: b_0 + 2 } },
        ColumnMapping { col_index: 26, computation: DirectU8 { record_byte_offset: b_0 + 3 } },
        // c[0..3]
        ColumnMapping { col_index: 27, computation: DirectU8 { record_byte_offset: c_0 } },
        ColumnMapping { col_index: 28, computation: DirectU8 { record_byte_offset: c_0 + 1 } },
        ColumnMapping { col_index: 29, computation: DirectU8 { record_byte_offset: c_0 + 2 } },
        ColumnMapping { col_index: 30, computation: DirectU8 { record_byte_offset: c_0 + 3 } },
        // opcode flags
        ColumnMapping { col_index: 31, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 0 } },
        ColumnMapping { col_index: 32, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 1 } },
        ColumnMapping { col_index: 33, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 2 } },
        ColumnMapping { col_index: 34, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 3 } },
        ColumnMapping { col_index: 35, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 4 } },
    ];

    AirColumnMapping {
        air_name: "BaseAlu",
        width: 36,
        record_byte_size: 40 + 12, // adapter(40) + core(12), but need to check padding
        columns,
    }
}

// ============================================================================
// Mapping table for LoadStore (width=41)
// ============================================================================

/// Byte offset of the core record start within the arena row for LoadStore.
/// The adapter cols have width 23, so the core bytes start at byte offset 23 * 4 = 92.
const LOADSTORE_CORE_BYTE_OFFSET: usize = 23 * 4;

/// Build the mapping table for Rv32LoadStore.
pub fn loadstore_mapping() -> AirColumnMapping {
    loadstore_mapping_for(ArenaType::Matrix)
}

pub fn loadstore_mapping_for(arena: ArenaType) -> AirColumnMapping {
    use ColumnComputation::*;

    // Adapter record byte offsets
    let from_pc: usize = 0;
    let from_timestamp: usize = 4;
    let rs1_ptr: usize = 8;
    let rs1_val: usize = 12; // u32 — decomposed into 4 byte limbs for rs1_data
    let rs1_aux_prev_ts: usize = 16;
    let rd_rs2_ptr: usize = 20;
    let read_data_aux_prev_ts: usize = 24;
    let imm: usize = 28; // u16
    let imm_sign: usize = 30; // bool/u8
    let mem_as: usize = 31; // u8
    let write_prev_ts: usize = 32; // u32

    // Core record byte offsets
    let core = core_byte_offset("LoadStore", arena);
    let local_opcode: usize = core;
    let shift_amount: usize = core + 1;
    let read_data: usize = core + 2; // u8[4]
    let prev_data: usize = core + 8; // u32[4] (after 2 bytes padding at core+6)

    let columns = vec![
        // Adapter columns (0-22)
        ColumnMapping { col_index: 0, computation: DirectU32 { record_byte_offset: from_pc } },
        ColumnMapping { col_index: 1, computation: DirectU32 { record_byte_offset: from_timestamp } },
        ColumnMapping { col_index: 2, computation: DirectU32 { record_byte_offset: rs1_ptr } },
        // rs1_data[0..3]: byte decomposition of rs1_val
        ColumnMapping { col_index: 3, computation: DirectU8 { record_byte_offset: rs1_val } },
        ColumnMapping { col_index: 4, computation: DirectU8 { record_byte_offset: rs1_val + 1 } },
        ColumnMapping { col_index: 5, computation: DirectU8 { record_byte_offset: rs1_val + 2 } },
        ColumnMapping { col_index: 6, computation: DirectU8 { record_byte_offset: rs1_val + 3 } },
        // rs1_aux_cols
        ColumnMapping { col_index: 7, computation: DirectU32 { record_byte_offset: rs1_aux_prev_ts } },
        ColumnMapping {
            col_index: 8,
            computation: TimestampDecomp {
                curr_ts_byte_offset: from_timestamp,
                curr_ts_delta: 0,
                prev_ts_byte_offset: rs1_aux_prev_ts,
                limb_index: 0,
            },
        },
        ColumnMapping {
            col_index: 9,
            computation: TimestampDecomp {
                curr_ts_byte_offset: from_timestamp,
                curr_ts_delta: 0,
                prev_ts_byte_offset: rs1_aux_prev_ts,
                limb_index: 1,
            },
        },
        // rd_rs2_ptr: conditional (UINT32_MAX means no write => 0)
        ColumnMapping {
            col_index: 10,
            computation: LoadStoreRdRs2Ptr { rd_rs2_ptr_byte_offset: rd_rs2_ptr },
        },
        // read_data_aux
        ColumnMapping { col_index: 11, computation: DirectU32 { record_byte_offset: read_data_aux_prev_ts } },
        ColumnMapping {
            col_index: 12,
            computation: TimestampDecomp {
                curr_ts_byte_offset: from_timestamp,
                curr_ts_delta: 1,
                prev_ts_byte_offset: read_data_aux_prev_ts,
                limb_index: 0,
            },
        },
        ColumnMapping {
            col_index: 13,
            computation: TimestampDecomp {
                curr_ts_byte_offset: from_timestamp,
                curr_ts_delta: 1,
                prev_ts_byte_offset: read_data_aux_prev_ts,
                limb_index: 1,
            },
        },
        // imm (u16)
        ColumnMapping { col_index: 14, computation: DirectU16 { record_byte_offset: imm } },
        // imm_sign
        ColumnMapping { col_index: 15, computation: DirectU8 { record_byte_offset: imm_sign } },
        // mem_ptr_limbs: computed from rs1_val + sign_extend(imm, imm_sign)
        ColumnMapping {
            col_index: 16,
            computation: PointerLimb {
                val_byte_offset: rs1_val,
                imm_byte_offset: imm,
                imm_sign_byte_offset: imm_sign,
                limb_index: 0,
            },
        },
        ColumnMapping {
            col_index: 17,
            computation: PointerLimb {
                val_byte_offset: rs1_val,
                imm_byte_offset: imm,
                imm_sign_byte_offset: imm_sign,
                limb_index: 1,
            },
        },
        // mem_as
        ColumnMapping { col_index: 18, computation: DirectU8 { record_byte_offset: mem_as } },
        // write_base_aux: conditional on needs_write
        ColumnMapping {
            col_index: 19,
            computation: LoadStoreWriteAuxPrevTs {
                write_prev_ts_byte_offset: write_prev_ts,
                rd_rs2_ptr_byte_offset: rd_rs2_ptr,
            },
        },
        ColumnMapping {
            col_index: 20,
            computation: LoadStoreWriteAuxDecomp {
                from_ts_byte_offset: from_timestamp,
                write_prev_ts_byte_offset: write_prev_ts,
                rd_rs2_ptr_byte_offset: rd_rs2_ptr,
                limb_index: 0,
            },
        },
        ColumnMapping {
            col_index: 21,
            computation: LoadStoreWriteAuxDecomp {
                from_ts_byte_offset: from_timestamp,
                write_prev_ts_byte_offset: write_prev_ts,
                rd_rs2_ptr_byte_offset: rd_rs2_ptr,
                limb_index: 1,
            },
        },
        // needs_write
        ColumnMapping {
            col_index: 22,
            computation: LoadStoreNeedsWrite { rd_rs2_ptr_byte_offset: rd_rs2_ptr },
        },
        // Core columns (23-40)
        // flags[0..3]: opcode-dependent
        ColumnMapping { col_index: 23, computation: LoadStoreFlag { opcode_byte_offset: local_opcode, shift_byte_offset: shift_amount, flag_index: 0 } },
        ColumnMapping { col_index: 24, computation: LoadStoreFlag { opcode_byte_offset: local_opcode, shift_byte_offset: shift_amount, flag_index: 1 } },
        ColumnMapping { col_index: 25, computation: LoadStoreFlag { opcode_byte_offset: local_opcode, shift_byte_offset: shift_amount, flag_index: 2 } },
        ColumnMapping { col_index: 26, computation: LoadStoreFlag { opcode_byte_offset: local_opcode, shift_byte_offset: shift_amount, flag_index: 3 } },
        // is_valid
        ColumnMapping { col_index: 27, computation: Constant(1) },
        // is_load
        ColumnMapping {
            col_index: 28,
            computation: LoadStoreIsLoad { opcode_byte_offset: local_opcode },
        },
        // read_data[0..3]
        ColumnMapping { col_index: 29, computation: DirectU8 { record_byte_offset: read_data } },
        ColumnMapping { col_index: 30, computation: DirectU8 { record_byte_offset: read_data + 1 } },
        ColumnMapping { col_index: 31, computation: DirectU8 { record_byte_offset: read_data + 2 } },
        ColumnMapping { col_index: 32, computation: DirectU8 { record_byte_offset: read_data + 3 } },
        // prev_data[0..3]: u32 values from record
        ColumnMapping { col_index: 33, computation: DirectU32 { record_byte_offset: prev_data } },
        ColumnMapping { col_index: 34, computation: DirectU32 { record_byte_offset: prev_data + 4 } },
        ColumnMapping { col_index: 35, computation: DirectU32 { record_byte_offset: prev_data + 8 } },
        ColumnMapping { col_index: 36, computation: DirectU32 { record_byte_offset: prev_data + 12 } },
        // write_data[0..3]: computed from opcode, shift, read_data, prev_data
        ColumnMapping {
            col_index: 37,
            computation: LoadStoreWriteData {
                opcode_byte_offset: local_opcode,
                shift_byte_offset: shift_amount,
                read_data_byte_offset: read_data,
                prev_data_byte_offset: prev_data,
                limb_index: 0,
            },
        },
        ColumnMapping {
            col_index: 38,
            computation: LoadStoreWriteData {
                opcode_byte_offset: local_opcode,
                shift_byte_offset: shift_amount,
                read_data_byte_offset: read_data,
                prev_data_byte_offset: prev_data,
                limb_index: 1,
            },
        },
        ColumnMapping {
            col_index: 39,
            computation: LoadStoreWriteData {
                opcode_byte_offset: local_opcode,
                shift_byte_offset: shift_amount,
                read_data_byte_offset: read_data,
                prev_data_byte_offset: prev_data,
                limb_index: 2,
            },
        },
        ColumnMapping {
            col_index: 40,
            computation: LoadStoreWriteData {
                opcode_byte_offset: local_opcode,
                shift_byte_offset: shift_amount,
                read_data_byte_offset: read_data,
                prev_data_byte_offset: prev_data,
                limb_index: 3,
            },
        },
    ];

    AirColumnMapping {
        air_name: "LoadStore",
        width: 41,
        record_byte_size: 36 + 24, // adapter(36) + core(24)
        columns,
    }
}

// ============================================================================
// Mapping table for Shift (width=53) — uses BaseAlu adapter (width=19)
// ============================================================================

/// Build the mapping table for Rv32Shift (adapter=BaseAlu width=19, core=ShiftCore width=34, total=53).
pub fn shift_mapping() -> AirColumnMapping {
    shift_mapping_for(ArenaType::Matrix)
}

pub fn shift_mapping_for(arena: ArenaType) -> AirColumnMapping {
    use ColumnComputation::*;

    // Adapter record byte offsets — same as BaseAlu adapter
    let from_pc: usize = 0;
    let from_timestamp: usize = 4;
    let rd_ptr: usize = 8;
    let rs1_ptr: usize = 12;
    let rs2: usize = 16;
    let rs2_as: usize = 20;
    let reads_aux_0_prev_ts: usize = 24;
    let reads_aux_1_prev_ts: usize = 28;
    let writes_aux_prev_ts: usize = 32;
    let writes_aux_prev_data_0: usize = 36;

    // Core record byte offsets (at core offset = 19*4 = 76)
    let core = core_byte_offset("Shift", arena);
    let b_0: usize = core; // b[4] at core+0
    let c_0: usize = core + 4; // c[4] at core+4
    let local_opcode: usize = core + 8; // opcode at core+8

    // ShiftCoreCols layout (starting at col 19):
    // a[4], b[4], c[4], sll_flag, srl_flag, sra_flag,
    // bit_mul_left, bit_mul_right, b_sign,
    // bit_shift_marker[8], limb_shift_marker[4], bit_shift_carry[4]

    let mut columns = Vec::with_capacity(53);

    // Adapter columns (0-18) — identical to BaseAlu
    columns.push(ColumnMapping { col_index: 0, computation: DirectU32 { record_byte_offset: from_pc } });
    columns.push(ColumnMapping { col_index: 1, computation: DirectU32 { record_byte_offset: from_timestamp } });
    columns.push(ColumnMapping { col_index: 2, computation: DirectU32 { record_byte_offset: rd_ptr } });
    columns.push(ColumnMapping { col_index: 3, computation: DirectU32 { record_byte_offset: rs1_ptr } });
    columns.push(ColumnMapping { col_index: 4, computation: DirectU32 { record_byte_offset: rs2 } });
    columns.push(ColumnMapping { col_index: 5, computation: DirectU8 { record_byte_offset: rs2_as } });
    columns.push(ColumnMapping { col_index: 6, computation: DirectU32 { record_byte_offset: reads_aux_0_prev_ts } });
    columns.push(ColumnMapping { col_index: 7, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: reads_aux_0_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 8, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: reads_aux_0_prev_ts, limb_index: 1 } });
    columns.push(ColumnMapping { col_index: 9, computation: Conditional { condition_byte_offset: rs2_as, then_comp: Box::new(DirectU32 { record_byte_offset: reads_aux_1_prev_ts }) } });
    columns.push(ColumnMapping { col_index: 10, computation: Conditional { condition_byte_offset: rs2_as, then_comp: Box::new(TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: reads_aux_1_prev_ts, limb_index: 0 }) } });
    columns.push(ColumnMapping { col_index: 11, computation: Conditional { condition_byte_offset: rs2_as, then_comp: Box::new(TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: reads_aux_1_prev_ts, limb_index: 1 }) } });
    columns.push(ColumnMapping { col_index: 12, computation: DirectU32 { record_byte_offset: writes_aux_prev_ts } });
    columns.push(ColumnMapping { col_index: 13, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 2, prev_ts_byte_offset: writes_aux_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 14, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 2, prev_ts_byte_offset: writes_aux_prev_ts, limb_index: 1 } });
    columns.push(ColumnMapping { col_index: 15, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 } });
    columns.push(ColumnMapping { col_index: 16, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 1 } });
    columns.push(ColumnMapping { col_index: 17, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 2 } });
    columns.push(ColumnMapping { col_index: 18, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 3 } });

    // Core columns (19-52): shift-specific
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 19 + i, computation: ShiftResult { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0, limb_index: i } });
    }
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 23 + i, computation: DirectU8 { record_byte_offset: b_0 + i } });
    }
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 27 + i, computation: DirectU8 { record_byte_offset: c_0 + i } });
    }
    columns.push(ColumnMapping { col_index: 31, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 0 } }); // sll
    columns.push(ColumnMapping { col_index: 32, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 1 } }); // srl
    columns.push(ColumnMapping { col_index: 33, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 2 } }); // sra
    // bit_multiplier_left, bit_multiplier_right, b_sign
    columns.push(ColumnMapping { col_index: 34, computation: ShiftBitMulLeft { opcode_byte_offset: local_opcode, c_byte_offset: c_0 } });
    columns.push(ColumnMapping { col_index: 35, computation: ShiftBitMulRight { opcode_byte_offset: local_opcode, c_byte_offset: c_0 } });
    columns.push(ColumnMapping { col_index: 36, computation: ShiftBSign { opcode_byte_offset: local_opcode, b_byte_offset: b_0 } });
    // bit_shift_marker[8]
    for i in 0..8 {
        columns.push(ColumnMapping { col_index: 37 + i, computation: ShiftBitMarker { c_byte_offset: c_0, marker_index: i } });
    }
    // limb_shift_marker[4]
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 45 + i, computation: ShiftLimbMarker { c_byte_offset: c_0, marker_index: i } });
    }
    // bit_shift_carry[4]
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 49 + i, computation: ShiftBitCarry { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0, limb_index: i } });
    }

    AirColumnMapping {
        air_name: "Shift",
        width: 53,
        record_byte_size: 40 + 12,
        columns,
    }
}

// ============================================================================
// Mapping table for BranchEqual (width=26)
// ============================================================================

/// Build the mapping table for BranchEqual (adapter=BranchAdapter width=10, core=BranchEqualCore width=16, total=26).
pub fn branch_equal_mapping() -> AirColumnMapping {
    branch_equal_mapping_for(ArenaType::Matrix)
}

pub fn branch_equal_mapping_for(arena: ArenaType) -> AirColumnMapping {
    use ColumnComputation::*;

    // BranchAdapter record byte offsets
    let from_pc: usize = 0;
    let from_timestamp: usize = 4;
    let rs1_ptr: usize = 8;
    let rs2_ptr: usize = 12;
    let reads_aux_0_prev_ts: usize = 16;
    let reads_aux_1_prev_ts: usize = 20;
    // adapter record = 24 bytes = 6 u32s

    // BranchAdapterCols layout:
    // col 0: from_state.pc, col 1: from_state.timestamp
    // col 2: rs1_ptr, col 3: rs2_ptr
    // col 4: reads_aux_0.prev_timestamp
    // col 5-6: reads_aux_0.lt_decomp[0..1]
    // col 7: reads_aux_1.prev_timestamp
    // col 8-9: reads_aux_1.lt_decomp[0..1]
    // adapter width = 10

    // Core record byte offsets
    let core = core_byte_offset("BranchEqual", arena);
    let a_0: usize = core;       // a[4] at core+0
    let b_0: usize = core + 4;   // b[4] at core+4
    let core_imm: usize = core + 8; // imm (u32) at core+8
    let core_opcode: usize = core + 12; // local_opcode (u8) at core+12

    // BranchEqualCoreCols layout (starting at col 10):
    // a[4], b[4], cmp_result, imm, opcode_beq_flag, opcode_bne_flag, diff_inv_marker[4]
    // core width = 4+4+1+1+1+1+4 = 16. Total = 10+16 = 26

    let mut columns = Vec::with_capacity(26);

    // Adapter columns (0-9)
    columns.push(ColumnMapping { col_index: 0, computation: DirectU32 { record_byte_offset: from_pc } });
    columns.push(ColumnMapping { col_index: 1, computation: DirectU32 { record_byte_offset: from_timestamp } });
    columns.push(ColumnMapping { col_index: 2, computation: DirectU32 { record_byte_offset: rs1_ptr } });
    columns.push(ColumnMapping { col_index: 3, computation: DirectU32 { record_byte_offset: rs2_ptr } });
    columns.push(ColumnMapping { col_index: 4, computation: DirectU32 { record_byte_offset: reads_aux_0_prev_ts } });
    columns.push(ColumnMapping { col_index: 5, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: reads_aux_0_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 6, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: reads_aux_0_prev_ts, limb_index: 1 } });
    columns.push(ColumnMapping { col_index: 7, computation: DirectU32 { record_byte_offset: reads_aux_1_prev_ts } });
    columns.push(ColumnMapping { col_index: 8, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: reads_aux_1_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 9, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: reads_aux_1_prev_ts, limb_index: 1 } });

    // Core columns (10-25)
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 10 + i, computation: DirectU8 { record_byte_offset: a_0 + i } });
    }
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 14 + i, computation: DirectU8 { record_byte_offset: b_0 + i } });
    }
    columns.push(ColumnMapping { col_index: 18, computation: BranchEqualCmpResult { a_byte_offset: a_0, b_byte_offset: b_0, opcode_byte_offset: core_opcode } });
    columns.push(ColumnMapping { col_index: 19, computation: DirectU32 { record_byte_offset: core_imm } });
    columns.push(ColumnMapping { col_index: 20, computation: BoolFromOpcode { opcode_byte_offset: core_opcode, expected_opcode: 0 } }); // beq
    columns.push(ColumnMapping { col_index: 21, computation: BoolFromOpcode { opcode_byte_offset: core_opcode, expected_opcode: 1 } }); // bne
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 22 + i, computation: BranchEqualDiffInvMarker { a_byte_offset: a_0, b_byte_offset: b_0, opcode_byte_offset: core_opcode, marker_index: i } });
    }

    AirColumnMapping {
        air_name: "BranchEqual",
        width: 26,
        record_byte_size: 24 + 16, // adapter(24) + core(16 incl padding)
        columns,
    }
}

// ============================================================================
// Mapping table for LessThan (Rv32BaseAluAdapter, width=37)
// ============================================================================

/// Build the mapping table for LessThan (slt/sltu/slti/sltiu).
/// Adapter shared with BaseAlu/Shift (width=19); LessThanCore width=18.
pub fn less_than_mapping() -> AirColumnMapping {
    less_than_mapping_for(ArenaType::Matrix)
}

pub fn less_than_mapping_for(arena: ArenaType) -> AirColumnMapping {
    use ColumnComputation::*;

    // Rv32BaseAluAdapter byte offsets (identical layout to BaseAlu/Shift).
    let from_pc: usize = 0;
    let from_timestamp: usize = 4;
    let rd_ptr: usize = 8;
    let rs1_ptr: usize = 12;
    let rs2: usize = 16;
    let rs2_as: usize = 20;
    let reads_aux_0_prev_ts: usize = 24;
    let reads_aux_1_prev_ts: usize = 28;
    let writes_aux_prev_ts: usize = 32;
    let writes_aux_prev_data_0: usize = 36;

    // LessThanCoreRecord layout: b[4] | c[4] | local_opcode (u8) — same prefix
    // as BaseAlu/Shift, but no result limbs (cmp_result is 0/1, derived).
    let core = core_byte_offset("LessThan", arena);
    let b_0: usize = core;
    let c_0: usize = core + 4;
    let local_opcode: usize = core + 8;

    // LessThanCoreCols layout (starting at adapter width = 19):
    //   b[4], c[4], cmp_result, opcode_slt_flag, opcode_sltu_flag,
    //   b_msb_f, c_msb_f, diff_marker[4], diff_val
    // → core width = 4 + 4 + 1 + 2 + 2 + 4 + 1 = 18; total width = 37.
    let mut columns = Vec::with_capacity(37);

    // ── Adapter columns (0-18) — identical to BaseAlu adapter ────────────────
    columns.push(ColumnMapping { col_index: 0, computation: DirectU32 { record_byte_offset: from_pc } });
    columns.push(ColumnMapping { col_index: 1, computation: DirectU32 { record_byte_offset: from_timestamp } });
    columns.push(ColumnMapping { col_index: 2, computation: DirectU32 { record_byte_offset: rd_ptr } });
    columns.push(ColumnMapping { col_index: 3, computation: DirectU32 { record_byte_offset: rs1_ptr } });
    columns.push(ColumnMapping { col_index: 4, computation: DirectU32 { record_byte_offset: rs2 } });
    columns.push(ColumnMapping { col_index: 5, computation: DirectU8 { record_byte_offset: rs2_as } });
    columns.push(ColumnMapping { col_index: 6, computation: DirectU32 { record_byte_offset: reads_aux_0_prev_ts } });
    columns.push(ColumnMapping { col_index: 7, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: reads_aux_0_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 8, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: reads_aux_0_prev_ts, limb_index: 1 } });
    columns.push(ColumnMapping { col_index: 9, computation: Conditional { condition_byte_offset: rs2_as, then_comp: Box::new(DirectU32 { record_byte_offset: reads_aux_1_prev_ts }) } });
    columns.push(ColumnMapping { col_index: 10, computation: Conditional { condition_byte_offset: rs2_as, then_comp: Box::new(TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: reads_aux_1_prev_ts, limb_index: 0 }) } });
    columns.push(ColumnMapping { col_index: 11, computation: Conditional { condition_byte_offset: rs2_as, then_comp: Box::new(TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: reads_aux_1_prev_ts, limb_index: 1 }) } });
    columns.push(ColumnMapping { col_index: 12, computation: DirectU32 { record_byte_offset: writes_aux_prev_ts } });
    columns.push(ColumnMapping { col_index: 13, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 2, prev_ts_byte_offset: writes_aux_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 14, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 2, prev_ts_byte_offset: writes_aux_prev_ts, limb_index: 1 } });
    columns.push(ColumnMapping { col_index: 15, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 } });
    columns.push(ColumnMapping { col_index: 16, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 1 } });
    columns.push(ColumnMapping { col_index: 17, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 2 } });
    columns.push(ColumnMapping { col_index: 18, computation: DirectU8 { record_byte_offset: writes_aux_prev_data_0 + 3 } });

    // ── Core columns (19-36) ────────────────────────────────────────────────
    // b[0..3]
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 19 + i, computation: DirectU8 { record_byte_offset: b_0 + i } });
    }
    // c[0..3]
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 23 + i, computation: DirectU8 { record_byte_offset: c_0 + i } });
    }
    // cmp_result (col 27): runs run_less_than(opcode, b, c) → cmp.
    columns.push(ColumnMapping { col_index: 27, computation: LessThanCmpResult { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0 } });
    // opcode flags: slt=0, sltu=1.
    columns.push(ColumnMapping { col_index: 28, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 0 } });
    columns.push(ColumnMapping { col_index: 29, computation: BoolFromOpcode { opcode_byte_offset: local_opcode, expected_opcode: 1 } });
    // b_msb_f, c_msb_f.
    columns.push(ColumnMapping { col_index: 30, computation: LessThanBMsbF { opcode_byte_offset: local_opcode, b_byte_offset: b_0 } });
    columns.push(ColumnMapping { col_index: 31, computation: LessThanCMsbF { opcode_byte_offset: local_opcode, c_byte_offset: c_0 } });
    // diff_marker[0..3] — share signature with cmp_result/diff_val for future fusion.
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 32 + i, computation: LessThanDiffMarker { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0, marker_index: i } });
    }
    // diff_val.
    columns.push(ColumnMapping { col_index: 36, computation: LessThanDiffVal { opcode_byte_offset: local_opcode, b_byte_offset: b_0, c_byte_offset: c_0 } });

    AirColumnMapping {
        air_name: "LessThan",
        width: 37,
        record_byte_size: 40 + 12, // adapter(40) + core(12 with padding)
        columns,
    }
}

// ============================================================================
// Mapping table for BranchLessThan (Rv32BranchAdapter, width=32)
// ============================================================================

/// Build the mapping table for BranchLessThan (blt/bltu/bge/bgeu).
/// Adapter shared with BranchEqual (width=10); BranchLtCore width=22.
pub fn branch_lt_mapping() -> AirColumnMapping {
    branch_lt_mapping_for(ArenaType::Matrix)
}

pub fn branch_lt_mapping_for(arena: ArenaType) -> AirColumnMapping {
    use ColumnComputation::*;

    // Rv32BranchAdapter byte offsets (same as BranchEqual).
    let from_pc: usize = 0;
    let from_timestamp: usize = 4;
    let rs1_ptr: usize = 8;
    let rs2_ptr: usize = 12;
    let reads_aux_0_prev_ts: usize = 16;
    let reads_aux_1_prev_ts: usize = 20;

    // BranchLtCoreRecord layout: a[4] | b[4] | imm (u32) | local_opcode (u8).
    let core = core_byte_offset("BranchLt", arena);
    let a_0: usize = core;
    let b_0: usize = core + 4;
    let core_imm: usize = core + 8;
    let core_opcode: usize = core + 12;

    // BranchLtCoreCols layout (starting at adapter width = 10):
    //   a[4], b[4], cmp_result, imm, opcode_blt, opcode_bltu, opcode_bge,
    //   opcode_bgeu, a_msb_f, b_msb_f, cmp_lt, diff_marker[4], diff_val.
    // → core width = 22; total width = 32.
    let mut columns = Vec::with_capacity(32);

    // ── Adapter columns (0-9) — identical to BranchEqual adapter ────────────
    columns.push(ColumnMapping { col_index: 0, computation: DirectU32 { record_byte_offset: from_pc } });
    columns.push(ColumnMapping { col_index: 1, computation: DirectU32 { record_byte_offset: from_timestamp } });
    columns.push(ColumnMapping { col_index: 2, computation: DirectU32 { record_byte_offset: rs1_ptr } });
    columns.push(ColumnMapping { col_index: 3, computation: DirectU32 { record_byte_offset: rs2_ptr } });
    columns.push(ColumnMapping { col_index: 4, computation: DirectU32 { record_byte_offset: reads_aux_0_prev_ts } });
    columns.push(ColumnMapping { col_index: 5, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: reads_aux_0_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 6, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: reads_aux_0_prev_ts, limb_index: 1 } });
    columns.push(ColumnMapping { col_index: 7, computation: DirectU32 { record_byte_offset: reads_aux_1_prev_ts } });
    columns.push(ColumnMapping { col_index: 8, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: reads_aux_1_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 9, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: reads_aux_1_prev_ts, limb_index: 1 } });

    // ── Core columns (10-31) ────────────────────────────────────────────────
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 10 + i, computation: DirectU8 { record_byte_offset: a_0 + i } });
    }
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 14 + i, computation: DirectU8 { record_byte_offset: b_0 + i } });
    }
    // cmp_result (branch taken when (cmp_result_internal ^ ge_op) selects taken-side).
    columns.push(ColumnMapping { col_index: 18, computation: BranchLtCmpResult { opcode_byte_offset: core_opcode, a_byte_offset: a_0, b_byte_offset: b_0 } });
    // imm (already a u32 in record).
    columns.push(ColumnMapping { col_index: 19, computation: DirectU32 { record_byte_offset: core_imm } });
    // opcode flags: BLT=0, BLTU=1, BGE=2, BGEU=3.
    columns.push(ColumnMapping { col_index: 20, computation: BoolFromOpcode { opcode_byte_offset: core_opcode, expected_opcode: 0 } });
    columns.push(ColumnMapping { col_index: 21, computation: BoolFromOpcode { opcode_byte_offset: core_opcode, expected_opcode: 1 } });
    columns.push(ColumnMapping { col_index: 22, computation: BoolFromOpcode { opcode_byte_offset: core_opcode, expected_opcode: 2 } });
    columns.push(ColumnMapping { col_index: 23, computation: BoolFromOpcode { opcode_byte_offset: core_opcode, expected_opcode: 3 } });
    // a_msb_f, b_msb_f.
    columns.push(ColumnMapping { col_index: 24, computation: BranchLtAMsbF { opcode_byte_offset: core_opcode, a_byte_offset: a_0 } });
    columns.push(ColumnMapping { col_index: 25, computation: BranchLtBMsbF { opcode_byte_offset: core_opcode, b_byte_offset: b_0 } });
    // cmp_lt (independent of opcode direction).
    columns.push(ColumnMapping { col_index: 26, computation: BranchLtCmpLt { opcode_byte_offset: core_opcode, a_byte_offset: a_0, b_byte_offset: b_0 } });
    // diff_marker[0..3].
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 27 + i, computation: BranchLtDiffMarker { opcode_byte_offset: core_opcode, a_byte_offset: a_0, b_byte_offset: b_0, marker_index: i } });
    }
    // diff_val.
    columns.push(ColumnMapping { col_index: 31, computation: BranchLtDiffVal { opcode_byte_offset: core_opcode, a_byte_offset: a_0, b_byte_offset: b_0 } });

    AirColumnMapping {
        air_name: "BranchLt",
        width: 32,
        record_byte_size: 24 + 16, // adapter(24) + core(16 incl padding)
        columns,
    }
}

// ============================================================================
// Mapping table for AUIPC (Rv32RdWriteAdapter, width=20)
// ============================================================================

/// Build the mapping table for AUIPC.
/// Adapter (Rv32RdWriteAdapter) width=10; Rv32AuipcCore width=10.
pub fn auipc_mapping() -> AirColumnMapping {
    auipc_mapping_for(ArenaType::Matrix)
}

pub fn auipc_mapping_for(arena: ArenaType) -> AirColumnMapping {
    use ColumnComputation::*;

    // Rv32RdWriteAdapter byte offsets (no reads — only one write).
    let from_pc: usize = 0;
    let from_timestamp: usize = 4;
    let rd_ptr: usize = 8;
    let rd_aux_prev_ts: usize = 12;
    let rd_aux_prev_data_0: usize = 16; // 4 bytes (prev_data[0..4])

    // Rv32AuipcCoreRecord layout: from_pc (u32) | imm (u32).
    let core = core_byte_offset("Auipc", arena);
    let core_pc: usize = core;
    let core_imm: usize = core + 4;

    // Adapter cols (0-9), then core cols (10-19):
    //   is_valid, imm_limbs[0..3], pc_limbs[0..2], rd_data[0..4]
    let mut columns = Vec::with_capacity(20);

    // ── Adapter (0-9) — Rv32RdWriteAdapter ─────────────────────────────────
    columns.push(ColumnMapping { col_index: 0, computation: DirectU32 { record_byte_offset: from_pc } });
    columns.push(ColumnMapping { col_index: 1, computation: DirectU32 { record_byte_offset: from_timestamp } });
    columns.push(ColumnMapping { col_index: 2, computation: DirectU32 { record_byte_offset: rd_ptr } });
    columns.push(ColumnMapping { col_index: 3, computation: DirectU32 { record_byte_offset: rd_aux_prev_ts } });
    columns.push(ColumnMapping { col_index: 4, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: rd_aux_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 5, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: rd_aux_prev_ts, limb_index: 1 } });
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 6 + i, computation: DirectU8 { record_byte_offset: rd_aux_prev_data_0 + i } });
    }

    // ── Core (10-19) ───────────────────────────────────────────────────────
    columns.push(ColumnMapping { col_index: 10, computation: Constant(1) }); // is_valid
    // imm_limbs[0..3] — bytes 0..2 of imm (limb 3 is constrained to 0).
    for i in 0..3 {
        columns.push(ColumnMapping { col_index: 11 + i, computation: DirectU8 { record_byte_offset: core_imm + i } });
    }
    // pc_limbs[0..2] — bytes 1..2 of pc (the "middle" two limbs).
    for i in 0..2 {
        columns.push(ColumnMapping { col_index: 14 + i, computation: DirectU8 { record_byte_offset: core_pc + i + 1 } });
    }
    // rd_data[0..4] — (pc + (imm << 8)).to_le_bytes()[i].
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 16 + i, computation: AuipcRdLimb { pc_byte_offset: core_pc, imm_byte_offset: core_imm, limb_index: i } });
    }

    AirColumnMapping {
        air_name: "Auipc",
        width: 20,
        record_byte_size: 20 + 8, // adapter(20) + core(8)
        columns,
    }
}

// ============================================================================
// Mapping table for JALR (Rv32JalrAdapter, width=28)
// ============================================================================

/// Build the mapping table for JALR.
/// Adapter (Rv32JalrAdapter) width=15; Rv32JalrCore width=13.
pub fn jalr_mapping() -> AirColumnMapping {
    jalr_mapping_for(ArenaType::Matrix)
}

pub fn jalr_mapping_for(arena: ArenaType) -> AirColumnMapping {
    use ColumnComputation::*;

    // Rv32JalrAdapter byte offsets.
    let from_pc: usize = 0;
    let from_timestamp: usize = 4;
    let rs1_ptr: usize = 8;
    let rd_ptr: usize = 12;            // u32::MAX iff no write
    let rs1_aux_prev_ts: usize = 16;
    let rd_aux_prev_ts: usize = 20;
    let rd_aux_prev_data_0: usize = 24; // 4 bytes

    // Rv32JalrCoreRecord layout: imm (u16) | from_pc (u32) | rs1_val (u32) | imm_sign (bool).
    let core = core_byte_offset("Jalr", arena);
    let core_imm: usize = core;
    let core_from_pc: usize = core + 4;
    let core_rs1_val: usize = core + 8;
    let core_imm_sign: usize = core + 12;

    // Trace col layout (15 adapter + 13 core = 28):
    //   adapter:
    //     0: from_state.pc, 1: from_state.timestamp,
    //     2: rs1_ptr,
    //     3: rs1_aux.prev_ts, 4-5: rs1_aux ts_decomp,
    //     6: rd_ptr (0 if u32::MAX),
    //     7: rd_aux.prev_ts (cond),
    //     8-9: rd_aux ts_decomp (cond),
    //     10-13: rd_aux.prev_data[0..3] (cond),
    //     14: needs_write
    //   core:
    //     15: imm (u16), 16-19: rs1_data[0..3], 20-22: rd_data[0..2],
    //     23: is_valid, 24: to_pc_least_sig_bit, 25-26: to_pc_limbs[0..1],
    //     27: imm_sign
    let mut columns = Vec::with_capacity(28);

    // ── Adapter (0-14) — Rv32JalrAdapter ───────────────────────────────────
    columns.push(ColumnMapping { col_index: 0, computation: DirectU32 { record_byte_offset: from_pc } });
    columns.push(ColumnMapping { col_index: 1, computation: DirectU32 { record_byte_offset: from_timestamp } });
    columns.push(ColumnMapping { col_index: 2, computation: DirectU32 { record_byte_offset: rs1_ptr } });
    // rs1 reads_aux is unconditional.
    columns.push(ColumnMapping { col_index: 3, computation: DirectU32 { record_byte_offset: rs1_aux_prev_ts } });
    columns.push(ColumnMapping { col_index: 4, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: rs1_aux_prev_ts, limb_index: 0 } });
    columns.push(ColumnMapping { col_index: 5, computation: TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 0, prev_ts_byte_offset: rs1_aux_prev_ts, limb_index: 1 } });
    // rd_ptr — 0 when u32::MAX (matches LoadStoreRdRs2Ptr semantics).
    columns.push(ColumnMapping { col_index: 6, computation: LoadStoreRdRs2Ptr { rd_rs2_ptr_byte_offset: rd_ptr } });
    // rd_aux fields gated on needs_write (rd_ptr != u32::MAX). The chip uses
    // `from_timestamp + 1` as the write curr_ts (one read happened first), so
    // delta=1. We can't reuse LoadStoreWriteAuxDecomp because that arm
    // hardcodes delta=2 (LoadStore's two memory accesses).
    columns.push(ColumnMapping {
        col_index: 7,
        computation: ConditionalNotMaxU32 {
            ptr_byte_offset: rd_ptr,
            then_comp: Box::new(DirectU32 { record_byte_offset: rd_aux_prev_ts }),
        },
    });
    columns.push(ColumnMapping {
        col_index: 8,
        computation: ConditionalNotMaxU32 {
            ptr_byte_offset: rd_ptr,
            then_comp: Box::new(TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: rd_aux_prev_ts, limb_index: 0 }),
        },
    });
    columns.push(ColumnMapping {
        col_index: 9,
        computation: ConditionalNotMaxU32 {
            ptr_byte_offset: rd_ptr,
            then_comp: Box::new(TimestampDecomp { curr_ts_byte_offset: from_timestamp, curr_ts_delta: 1, prev_ts_byte_offset: rd_aux_prev_ts, limb_index: 1 }),
        },
    });
    for i in 0..4 {
        columns.push(ColumnMapping {
            col_index: 10 + i,
            computation: ConditionalNotMaxU32 {
                ptr_byte_offset: rd_ptr,
                then_comp: Box::new(DirectU8 { record_byte_offset: rd_aux_prev_data_0 + i }),
            },
        });
    }
    // needs_write — 1 if rd_ptr != u32::MAX (matches LoadStoreNeedsWrite).
    columns.push(ColumnMapping { col_index: 14, computation: LoadStoreNeedsWrite { rd_rs2_ptr_byte_offset: rd_ptr } });

    // ── Core (15-27) ───────────────────────────────────────────────────────
    columns.push(ColumnMapping { col_index: 15, computation: DirectU16 { record_byte_offset: core_imm } });
    for i in 0..4 {
        columns.push(ColumnMapping { col_index: 16 + i, computation: DirectU8 { record_byte_offset: core_rs1_val + i } });
    }
    // rd_data[0..2] = top 3 bytes of (pc + 4) → bytes 1..3.
    for i in 0..3 {
        columns.push(ColumnMapping { col_index: 20 + i, computation: JalrRdLimb { pc_byte_offset: core_from_pc, limb_index: i } });
    }
    columns.push(ColumnMapping { col_index: 23, computation: Constant(1) }); // is_valid
    columns.push(ColumnMapping { col_index: 24, computation: JalrToPcLsb { rs1_byte_offset: core_rs1_val, imm_byte_offset: core_imm, imm_sign_byte_offset: core_imm_sign } });
    for i in 0..2 {
        columns.push(ColumnMapping { col_index: 25 + i, computation: JalrToPcLimb { rs1_byte_offset: core_rs1_val, imm_byte_offset: core_imm, imm_sign_byte_offset: core_imm_sign, limb_index: i } });
    }
    columns.push(ColumnMapping { col_index: 27, computation: DirectU8 { record_byte_offset: core_imm_sign } });

    AirColumnMapping {
        air_name: "Jalr",
        width: 28,
        record_byte_size: 28 + 16, // adapter(28) + core(16: u16 imm + 2 pad + 2 u32 + bool + 3 pad, aligned to u32)
        columns,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_base_alu_mapping_coverage() {
        let mapping = base_alu_mapping();
        assert_eq!(mapping.width, 36);
        assert_eq!(mapping.columns.len(), 36);
        // Verify all column indices are covered
        for (i, col) in mapping.columns.iter().enumerate() {
            assert_eq!(col.col_index, i, "Column {i} has wrong index");
        }
    }

    #[test]
    fn test_alu_add() {
        let result = run_alu_byte(0, &[10, 20, 30, 40], &[5, 10, 15, 20]);
        assert_eq!(result, [15, 30, 45, 60]);
    }

    #[test]
    fn test_alu_add_carry() {
        let result = run_alu_byte(0, &[200, 0, 0, 0], &[100, 0, 0, 0]);
        assert_eq!(result, [44, 1, 0, 0]); // 200+100=300, 300&0xFF=44, carry=1
    }

    #[test]
    fn test_alu_xor() {
        let result = run_alu_byte(2, &[0xFF, 0x0F, 0xAA, 0x55], &[0x0F, 0xF0, 0x55, 0xAA]);
        assert_eq!(result, [0xF0, 0xFF, 0xFF, 0xFF]);
    }

    #[test]
    fn test_alu_sub() {
        let result = run_alu_byte(1, &[20, 30, 40, 50], &[5, 10, 15, 20]);
        assert_eq!(result, [15, 20, 25, 30]);
    }

    use openvm_stark_backend::p3_field::PrimeCharacteristicRing;

    /// Validate the JIT mapping against known trace data from the keccak mock prove debug output.
    /// Row 0 of BaseAlu from the actual run:
    ///   Raw record (u32 LE): [2100252, 2805, 48, 92, 28, 1, 2801, 2785, 2462, 1, 0...0, 2, 0...0]
    ///   After fill_trace:    [2100252, 2805, 48, 92, 28, 1, 2801, 3, 0, 2785, 20, 0, 2462, 344, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0]
    #[test]
    fn test_base_alu_mapping_against_real_data() {
        use openvm_stark_sdk::p3_baby_bear::BabyBear;

        let mapping = base_alu_mapping();

        // Construct the raw arena row bytes as they appear in memory.
        // The arena stores the record struct bytes at the positions corresponding to the row buffer.
        // Adapter record: 40 bytes at positions 0..39
        // Core record: 12 bytes at positions 76..87 (core offset = 19 * 4 = 76)
        let mut record_bytes = vec![0u8; 36 * 4]; // 36 columns * 4 bytes each

        // Adapter record fields:
        record_bytes[0..4].copy_from_slice(&2100252u32.to_le_bytes()); // from_pc
        record_bytes[4..8].copy_from_slice(&2805u32.to_le_bytes()); // from_timestamp
        record_bytes[8..12].copy_from_slice(&48u32.to_le_bytes()); // rd_ptr
        record_bytes[12..16].copy_from_slice(&92u32.to_le_bytes()); // rs1_ptr
        record_bytes[16..20].copy_from_slice(&28u32.to_le_bytes()); // rs2
        record_bytes[20] = 1; // rs2_as (u8)
        record_bytes[24..28].copy_from_slice(&2801u32.to_le_bytes()); // reads_aux[0].prev_ts
        record_bytes[28..32].copy_from_slice(&2785u32.to_le_bytes()); // reads_aux[1].prev_ts
        record_bytes[32..36].copy_from_slice(&2462u32.to_le_bytes()); // writes_aux.prev_ts
        record_bytes[36] = 1; // writes_aux.prev_data[0]
        record_bytes[37] = 0; // writes_aux.prev_data[1]
        record_bytes[38] = 0; // writes_aux.prev_data[2]
        record_bytes[39] = 0; // writes_aux.prev_data[3]

        // Core record at offset 76:
        let core_off = 76;
        record_bytes[core_off] = 0; // b[0]
        record_bytes[core_off + 1] = 0; // b[1]
        record_bytes[core_off + 2] = 0; // b[2]
        record_bytes[core_off + 3] = 0; // b[3]
        record_bytes[core_off + 4] = 0; // c[0]
        record_bytes[core_off + 5] = 0; // c[1]
        record_bytes[core_off + 6] = 0; // c[2]
        record_bytes[core_off + 7] = 0; // c[3]
        record_bytes[core_off + 8] = 2; // local_opcode = XOR

        // Expected output from fill_trace_row (as u32):
        let expected: Vec<u32> = vec![
            2100252, 2805, 48, 92, 28, 1, 2801, 3, 0, 2785, 20, 0, 2462, 344, 0, 1, 0, 0, 0,
            // core: a=[0,0,0,0], b=[0,0,0,0], c=[0,0,0,0], flags=[0,0,1,0,0]
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
        ];

        // Evaluate each column using the mapping
        // Use range_max_bits from the actual run. Looking at the debug output,
        // timestamp decomp: diff = 2805 - 2801 - 1 = 3, lower_decomp[0] = 3, [1] = 0
        // This means range_max_bits is large enough that 3 fits in one limb.
        // BabyBear range checker typically uses max_bits = 29 or similar.
        // Let's figure it out: decompose(3, max_bits) with limb[0]=3, limb[1]=0
        // means 3 < 2^max_bits, so max_bits >= 2. The actual value doesn't matter
        // as long as 3 fits in the lower limb. Let's use 17 (common value).
        let range_max_bits = 17u32;

        for col_map in &mapping.columns {
            let jit_val: BabyBear =
                eval_column(&col_map.computation, &record_bytes, range_max_bits);
            let expected_val = BabyBear::from_u32(expected[col_map.col_index]);
            assert_eq!(
                jit_val, expected_val,
                "Mismatch at column {} ({}): JIT={}, expected={}",
                col_map.col_index,
                match &col_map.computation {
                    ColumnComputation::DirectU32 { .. } => "DirectU32",
                    ColumnComputation::DirectU8 { .. } => "DirectU8",
                    ColumnComputation::TimestampDecomp { .. } => "TimestampDecomp",
                    ColumnComputation::AluResult { .. } => "AluResult",
                    ColumnComputation::BoolFromOpcode { .. } => "BoolFromOpcode",
                    ColumnComputation::Conditional { .. } => "Conditional",
                    ColumnComputation::Constant(_) => "Constant",
                    _ => "Unsupported",
                },
                jit_val.as_canonical_u32(),
                expected_val.as_canonical_u32()
            );
        }
    }
}

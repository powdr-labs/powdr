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
        ColumnComputation::Constant(val) => F::from_u32(*val),
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

// ============================================================================
// Mapping table for BaseAlu (width=36)
// ============================================================================

/// Byte offset of the core record start within the arena row.
/// The adapter cols have width 19, so the core bytes start at byte offset 19 * 4 = 76.
const BASE_ALU_CORE_BYTE_OFFSET: usize = 19 * 4;

/// Build the mapping table for Rv32BaseAlu (adapter width=19, core width=17, total=36).
pub fn base_alu_mapping() -> AirColumnMapping {
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
    let core = BASE_ALU_CORE_BYTE_OFFSET;
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

/// Build the mapping table for Rv32LoadStore (adapter width=23, core width=18, total=41).
pub fn loadstore_mapping() -> AirColumnMapping {
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
    let core = LOADSTORE_CORE_BYTE_OFFSET;
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
                },
                jit_val.as_canonical_u32(),
                expected_val.as_canonical_u32()
            );
        }
    }
}

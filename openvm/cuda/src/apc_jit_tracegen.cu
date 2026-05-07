#include "primitives/buffer_view.cuh"
#include "primitives/constants.h"
#include "primitives/trace_access.h"
#include "expr_eval.cuh"

// ============================================================================================
// JIT column computation descriptors
// ============================================================================================

// Computation type IDs — must match Rust-side ColumnComputation enum
enum JitCompType : uint16_t {
    JIT_DIRECT_U32        = 0,
    JIT_DIRECT_U8         = 1,
    JIT_DIRECT_U16        = 2,
    JIT_TIMESTAMP_DECOMP  = 3,
    JIT_ALU_RESULT        = 4,
    JIT_BOOL_FROM_OPCODE  = 5,
    JIT_POINTER_LIMB      = 6,
    JIT_SHIFT_RESULT      = 7,
    JIT_SHIFT_BIT_MUL_L   = 8,
    JIT_SHIFT_BIT_MUL_R   = 9,
    JIT_SHIFT_B_SIGN      = 10,
    JIT_SHIFT_BIT_MARKER  = 11,
    JIT_SHIFT_LIMB_MARKER = 12,
    JIT_SHIFT_BIT_CARRY   = 13,
    JIT_BRANCH_EQ_CMP     = 14,
    JIT_BRANCH_EQ_DIFF_INV= 15,
    JIT_CONSTANT          = 16,
    JIT_LS_RD_RS2_PTR     = 17,
    JIT_LS_NEEDS_WRITE    = 18,
    JIT_LS_WRITE_AUX_TS   = 19,
    JIT_LS_WRITE_AUX_DEC  = 20,
    JIT_LS_IS_LOAD        = 21,
    JIT_LS_FLAG           = 22,
    JIT_LS_WRITE_DATA     = 23,
    // LessThan (slt/sltu) — share (opcode, b, c) signature for future fusion.
    JIT_LT_CMP            = 24,
    JIT_LT_DIFF_VAL       = 25,
    JIT_LT_DIFF_MARKER    = 26,
    JIT_LT_B_MSB_F        = 27,
    JIT_LT_C_MSB_F        = 28,
    // BranchLessThan (blt/bltu/bge/bgeu) — share (opcode, a, b) signature.
    JIT_BLT_CMP           = 29,
    JIT_BLT_CMP_LT        = 30,
    JIT_BLT_DIFF_VAL      = 31,
    JIT_BLT_DIFF_MARKER   = 32,
    JIT_BLT_A_MSB_F       = 33,
    JIT_BLT_B_MSB_F       = 34,
    // Auipc / Jalr arms.
    JIT_AUIPC_RD_LIMB     = 35,
    JIT_JALR_TO_PC_LSB    = 36,
    JIT_JALR_TO_PC_LIMB   = 37,
    JIT_JALR_RD_LIMB      = 38,
    // Conditional flag bits — set on `comp_type` to gate the column write.
    // p[5] holds the byte offset of the gate value within the record.
    //   JIT_COND_FLAG          (0x80): record[p[5]] != 0          (1-byte gate)
    //   JIT_COND_NOT_MAX_U32   (0x40): u32 at p[5] != 0xFFFFFFFF  (4-byte gate)
    JIT_COND_FLAG         = 0x80,
    JIT_COND_NOT_MAX_U32  = 0x40,
    JIT_COND_DIRECT_U32   = JIT_COND_FLAG | JIT_DIRECT_U32,
    JIT_COND_TS_DECOMP    = JIT_COND_FLAG | JIT_TIMESTAMP_DECOMP,
};

// Describes how to compute one APC column from record bytes.
// 16 bytes total, well-aligned.
extern "C" {
struct JitColumnDesc {
    uint16_t comp_type;     // JitCompType
    uint16_t apc_col;       // destination column in APC trace
    uint16_t p[6];          // parameters (byte offsets, indices, etc.)
};

// Describes one instruction within the APC.
struct JitInstructionDesc {
    uint32_t arena_offset;    // byte offset into concatenated arena buffer
    uint32_t record_stride;   // bytes between records for consecutive APC calls
    uint32_t record_offset;   // this instruction's byte offset within one APC call's records
    uint32_t col_desc_start;  // index into column descriptor array
    uint32_t col_desc_count;  // number of column descriptors for this instruction
};
}

// ============================================================================================
// Inline helpers for shift operations
// ============================================================================================

__device__ __forceinline__ void get_shift_amounts(uint8_t c0, int &limb_shift, int &bit_shift) {
    int shift = c0 % 32; // NUM_LIMBS * CELL_BITS = 4 * 8 = 32
    limb_shift = shift / 8;
    bit_shift = shift % 8;
}

__device__ __forceinline__ void run_alu(uint8_t opcode, const uint8_t *b, const uint8_t *c, uint8_t *a) {
    switch (opcode) {
        case 0: { // ADD
            uint32_t carry = 0;
            for (int i = 0; i < 4; i++) {
                uint32_t s = (uint32_t)b[i] + (uint32_t)c[i] + carry;
                a[i] = s & 0xFF; carry = s >> 8;
            }
            break;
        }
        case 1: { // SUB
            uint32_t borrow = 0;
            for (int i = 0; i < 4; i++) {
                uint32_t rhs = (uint32_t)c[i] + borrow;
                if ((uint32_t)b[i] >= rhs) { a[i] = b[i] - rhs; borrow = 0; }
                else { a[i] = (256 + b[i] - rhs); borrow = 1; }
            }
            break;
        }
        case 2: for (int i=0;i<4;i++) a[i] = b[i] ^ c[i]; break; // XOR
        case 3: for (int i=0;i<4;i++) a[i] = b[i] | c[i]; break; // OR
        case 4: for (int i=0;i<4;i++) a[i] = b[i] & c[i]; break; // AND
        default: for (int i=0;i<4;i++) a[i] = 0; break;
    }
}

__device__ __forceinline__ void run_shift(uint8_t opcode, const uint8_t *b, const uint8_t *c, uint8_t *a) {
    int ls, bs;
    get_shift_amounts(c[0], ls, bs);
    if (opcode == 0) { // SLL
        for (int i=0;i<4;i++) a[i] = 0;
        for (int i=ls;i<4;i++) {
            uint16_t high = (uint16_t)b[i-ls] << bs;
            uint16_t low = (i>ls) ? ((uint16_t)b[i-ls-1] >> (8-bs)) : 0;
            a[i] = (high | low) & 0xFF;
        }
    } else { // SRL/SRA
        uint8_t msb = b[3] >> 7;
        uint8_t fill = (opcode==1) ? 0 : (0xFF * msb);
        for (int i=0;i<4;i++) a[i] = fill;
        for (int i=0;i<4-ls;i++) {
            uint16_t p1 = (uint16_t)(b[i+ls] >> bs);
            uint16_t p2v = (i+ls+1<4) ? b[i+ls+1] : fill;
            uint16_t p2 = (uint16_t)p2v << (8-bs);
            a[i] = (p1 | p2) & 0xFF;
        }
    }
}

// Mirror of LessThanCoreAir::run_less_than (NUM_LIMBS=4, LIMB_BITS=8).
// Returns cmp_result via the function value, sets out_diff_idx (0..4 with 4
// meaning equal), out_b_sign, out_c_sign as side outputs.
__device__ __forceinline__ bool run_less_than(
    bool is_slt, const uint8_t *b, const uint8_t *c,
    int *out_diff_idx, bool *out_b_sign, bool *out_c_sign
) {
    bool b_sign = is_slt && ((b[3] >> 7) == 1);
    bool c_sign = is_slt && ((c[3] >> 7) == 1);
    *out_b_sign = b_sign;
    *out_c_sign = c_sign;
    for (int i = 3; i >= 0; --i) {
        if (b[i] != c[i]) {
            *out_diff_idx = i;
            return ((b[i] < c[i]) ^ b_sign ^ c_sign);
        }
    }
    *out_diff_idx = 4;
    return false;
}

// Mirror of BranchLessThanCoreAir::run_cmp (NUM_LIMBS=4, LIMB_BITS=8).
// Local opcodes: BLT=0, BLTU=1, BGE=2, BGEU=3.
__device__ __forceinline__ bool run_branch_lt(
    uint8_t local_opcode, const uint8_t *a, const uint8_t *b,
    int *out_diff_idx, bool *out_a_sign, bool *out_b_sign
) {
    bool signed_op = (local_opcode == 0) || (local_opcode == 2);
    bool ge_op     = (local_opcode == 2) || (local_opcode == 3);
    bool a_sign    = signed_op && ((a[3] >> 7) == 1);
    bool b_sign    = signed_op && ((b[3] >> 7) == 1);
    *out_a_sign = a_sign;
    *out_b_sign = b_sign;
    for (int i = 3; i >= 0; --i) {
        if (a[i] != b[i]) {
            *out_diff_idx = i;
            return ((a[i] < b[i]) ^ a_sign ^ b_sign ^ ge_op);
        }
    }
    *out_diff_idx = 4;
    return ge_op;
}

// ============================================================================================
// eval_jit_column: evaluates one column descriptor
// ============================================================================================

__device__ __forceinline__ Fp eval_jit_column(
    const JitColumnDesc &desc,
    const uint8_t *record,
    uint32_t range_max_bits
) {
    uint16_t ctype = desc.comp_type;
    if ((ctype & JIT_COND_FLAG) != 0) {
        uint16_t cond_offset = desc.p[5]; // condition byte offset stored in p[5]
        if (record[cond_offset] == 0) return Fp::zero();
        ctype &= ~JIT_COND_FLAG;
    }
    if ((ctype & JIT_COND_NOT_MAX_U32) != 0) {
        uint16_t ptr_offset = desc.p[5];
        uint32_t v; memcpy(&v, record + ptr_offset, 4);
        if (v == 0xFFFFFFFFu) return Fp::zero();
        ctype &= ~JIT_COND_NOT_MAX_U32;
    }

    switch (ctype) {
        case JIT_DIRECT_U32: {
            uint32_t v; memcpy(&v, record + desc.p[0], 4);
            return Fp(v);
        }
        case JIT_DIRECT_U8:
            return Fp((uint32_t)record[desc.p[0]]);
        case JIT_DIRECT_U16: {
            uint16_t v; memcpy(&v, record + desc.p[0], 2);
            return Fp((uint32_t)v);
        }
        case JIT_TIMESTAMP_DECOMP: {
            uint32_t curr, prev;
            memcpy(&curr, record + desc.p[0], 4);
            memcpy(&prev, record + desc.p[2], 4);
            uint32_t delta = desc.p[1];
            uint32_t diff = (curr + delta) - prev - 1;
            uint32_t mask = (1u << range_max_bits) - 1;
            uint32_t limb_idx = desc.p[3];
            return Fp((diff >> (range_max_bits * limb_idx)) & mask);
        }
        case JIT_ALU_RESULT: {
            uint8_t opcode = record[desc.p[0]];
            const uint8_t *b = record + desc.p[1];
            const uint8_t *c = record + desc.p[2];
            uint8_t a[4]; run_alu(opcode, b, c, a);
            return Fp((uint32_t)a[desc.p[3]]);
        }
        case JIT_BOOL_FROM_OPCODE:
            return Fp(record[desc.p[0]] == (uint8_t)desc.p[1]);
        case JIT_POINTER_LIMB: {
            uint32_t val; memcpy(&val, record + desc.p[0], 4);
            uint16_t imm; memcpy(&imm, record + desc.p[1], 2);
            uint8_t sign = record[desc.p[2]];
            uint32_t imm_ext = (uint32_t)imm + (sign ? 0xFFFF0000u : 0u);
            uint32_t ptr = val + imm_ext;
            return Fp((ptr >> (16 * desc.p[3])) & 0xFFFF);
        }
        case JIT_SHIFT_RESULT: {
            uint8_t opcode = record[desc.p[0]];
            const uint8_t *b = record + desc.p[1];
            const uint8_t *c = record + desc.p[2];
            uint8_t a[4]; run_shift(opcode, b, c, a);
            return Fp((uint32_t)a[desc.p[3]]);
        }
        case JIT_SHIFT_BIT_MUL_L: {
            bool is_sll = record[desc.p[0]] == 0;
            int ls, bs; get_shift_amounts(record[desc.p[1]], ls, bs);
            return is_sll ? Fp(1u << bs) : Fp::zero();
        }
        case JIT_SHIFT_BIT_MUL_R: {
            bool is_sll = record[desc.p[0]] == 0;
            int ls, bs; get_shift_amounts(record[desc.p[1]], ls, bs);
            return is_sll ? Fp::zero() : Fp(1u << bs);
        }
        case JIT_SHIFT_B_SIGN: {
            bool is_sra = record[desc.p[0]] == 2;
            return is_sra ? Fp((uint32_t)(record[desc.p[1] + 3] >> 7)) : Fp::zero();
        }
        case JIT_SHIFT_BIT_MARKER: {
            int ls, bs; get_shift_amounts(record[desc.p[0]], ls, bs);
            return Fp(bs == (int)desc.p[1]);
        }
        case JIT_SHIFT_LIMB_MARKER: {
            int ls, bs; get_shift_amounts(record[desc.p[0]], ls, bs);
            return Fp(ls == (int)desc.p[1]);
        }
        case JIT_SHIFT_BIT_CARRY: {
            bool is_sll = record[desc.p[0]] == 0;
            int ls, bs; get_shift_amounts(record[desc.p[2]], ls, bs);
            if (bs == 0) return Fp::zero();
            uint8_t bv = record[desc.p[1] + desc.p[3]];
            uint8_t carry = is_sll ? (bv >> (8 - bs)) : (bv & ((1u << bs) - 1));
            return Fp((uint32_t)carry);
        }
        case JIT_BRANCH_EQ_CMP: {
            const uint8_t *a = record + desc.p[0];
            const uint8_t *b = record + desc.p[1];
            bool is_beq = record[desc.p[2]] == 0;
            bool eq = (a[0]==b[0] && a[1]==b[1] && a[2]==b[2] && a[3]==b[3]);
            return Fp(is_beq ? eq : !eq);
        }
        case JIT_BRANCH_EQ_DIFF_INV: {
            const uint8_t *a = record + desc.p[0];
            const uint8_t *b = record + desc.p[1];
            int diff_idx = 4;
            for (int i=0;i<4;i++) { if(a[i]!=b[i]) { diff_idx=i; break; } }
            if (diff_idx == 4) diff_idx = 0;
            int marker = desc.p[3];
            bool all_eq = (a[0]==b[0] && a[1]==b[1] && a[2]==b[2] && a[3]==b[3]);
            if (marker == diff_idx && !all_eq) {
                Fp av = Fp((uint32_t)a[diff_idx]);
                Fp bv = Fp((uint32_t)b[diff_idx]);
                return inv(av - bv);
            }
            return Fp::zero();
        }
        case JIT_CONSTANT:
            return Fp(desc.p[0] | ((uint32_t)desc.p[1] << 16));
        case JIT_LS_RD_RS2_PTR: {
            uint32_t v; memcpy(&v, record + desc.p[0], 4);
            return (v == 0xFFFFFFFFu) ? Fp::zero() : Fp(v);
        }
        case JIT_LS_NEEDS_WRITE: {
            uint32_t v; memcpy(&v, record + desc.p[0], 4);
            return Fp(v != 0xFFFFFFFFu);
        }
        case JIT_LS_WRITE_AUX_TS: {
            uint32_t rd; memcpy(&rd, record + desc.p[1], 4);
            if (rd == 0xFFFFFFFFu) return Fp::zero();
            uint32_t v; memcpy(&v, record + desc.p[0], 4);
            return Fp(v);
        }
        case JIT_LS_WRITE_AUX_DEC: {
            uint32_t rd; memcpy(&rd, record + desc.p[2], 4);
            if (rd == 0xFFFFFFFFu) return Fp::zero();
            uint32_t curr, prev;
            memcpy(&curr, record + desc.p[0], 4);
            memcpy(&prev, record + desc.p[1], 4);
            uint32_t diff = (curr + 2) - prev - 1;
            uint32_t mask = (1u << range_max_bits) - 1;
            return Fp((diff >> (range_max_bits * desc.p[3])) & mask);
        }
        case JIT_LS_IS_LOAD:
            return Fp(record[desc.p[0]] <= 2);
        case JIT_LS_FLAG: {
            uint8_t op = record[desc.p[0]];
            uint8_t sh = record[desc.p[1]];
            uint8_t flags[4] = {0,0,0,0};
            switch(op) {
                case 0: flags[0]=2; break;
                case 2: if(sh==0) flags[1]=2; else if(sh==2) flags[2]=2; break;
                case 1: switch(sh){case 0:flags[3]=2;break;case 1:flags[0]=1;break;case 2:flags[1]=1;break;case 3:flags[2]=1;break;} break;
                case 3: flags[3]=1; break;
                case 4: if(sh==0){flags[0]=1;flags[1]=1;}else if(sh==2){flags[0]=1;flags[2]=1;} break;
                case 5: switch(sh){case 0:flags[0]=1;flags[3]=1;break;case 1:flags[1]=1;flags[2]=1;break;case 2:flags[1]=1;flags[3]=1;break;case 3:flags[2]=1;flags[3]=1;break;} break;
            }
            return Fp((uint32_t)flags[desc.p[2]]);
        }
        case JIT_LS_WRITE_DATA: {
            uint8_t op = record[desc.p[0]];
            int sh = (int)record[desc.p[1]];
            const uint8_t *rd = record + desc.p[2];
            int idx = desc.p[4];
            uint32_t prev; memcpy(&prev, record + desc.p[3] + idx*4, 4);
            uint32_t wd = 0;
            switch(op) {
                case 0: case 3: wd = rd[idx]; break; // LOADW, STOREW
                case 2: wd = (idx < 2) ? rd[idx+sh] : 0; break; // LOADHU
                case 1: wd = (idx == 0) ? rd[sh] : 0; break; // LOADBU
                case 4: wd = (idx>=sh && idx<2+sh) ? rd[idx-sh] : prev; break; // STOREH
                case 5: wd = (idx==sh) ? rd[0] : prev; break; // STOREB
            }
            return Fp(wd);
        }
        // ── LessThan arms (p[0]=opcode_off, p[1]=b_off, p[2]=c_off, p[3]=marker_index) ──
        case JIT_LT_CMP: {
            bool is_slt = (record[desc.p[0]] == 0);
            int diff_idx; bool b_sign, c_sign;
            bool cmp = run_less_than(is_slt, record + desc.p[1], record + desc.p[2],
                                     &diff_idx, &b_sign, &c_sign);
            return Fp((uint32_t)cmp);
        }
        case JIT_LT_DIFF_VAL: {
            bool is_slt = (record[desc.p[0]] == 0);
            const uint8_t *b = record + desc.p[1];
            const uint8_t *c = record + desc.p[2];
            int diff_idx; bool b_sign, c_sign;
            bool cmp = run_less_than(is_slt, b, c, &diff_idx, &b_sign, &c_sign);
            if (diff_idx == 4) return Fp::zero();
            if (diff_idx == 3) {
                // Use signed-aware MSb encodings — match the field arithmetic.
                Fp b_msb = b_sign ? -Fp((uint32_t)(256u - b[3])) : Fp((uint32_t)b[3]);
                Fp c_msb = c_sign ? -Fp((uint32_t)(256u - c[3])) : Fp((uint32_t)c[3]);
                return cmp ? (c_msb - b_msb) : (b_msb - c_msb);
            }
            uint8_t big = cmp ? c[diff_idx] : b[diff_idx];
            uint8_t small = cmp ? b[diff_idx] : c[diff_idx];
            return Fp((uint32_t)(big - small));
        }
        case JIT_LT_DIFF_MARKER: {
            bool is_slt = (record[desc.p[0]] == 0);
            int diff_idx; bool b_sign, c_sign;
            run_less_than(is_slt, record + desc.p[1], record + desc.p[2],
                          &diff_idx, &b_sign, &c_sign);
            int marker = (int)desc.p[3];
            return Fp((uint32_t)((diff_idx != 4) && (diff_idx == marker)));
        }
        case JIT_LT_B_MSB_F: {
            bool is_slt = (record[desc.p[0]] == 0);
            uint8_t b3 = record[desc.p[1] + 3];
            bool b_sign = is_slt && ((b3 >> 7) == 1);
            return b_sign ? -Fp((uint32_t)(256u - b3)) : Fp((uint32_t)b3);
        }
        case JIT_LT_C_MSB_F: {
            bool is_slt = (record[desc.p[0]] == 0);
            uint8_t c3 = record[desc.p[1] + 3];
            bool c_sign = is_slt && ((c3 >> 7) == 1);
            return c_sign ? -Fp((uint32_t)(256u - c3)) : Fp((uint32_t)c3);
        }
        // ── BranchLessThan arms (p[0]=opcode_off, p[1]=a_off, p[2]=b_off) ──
        case JIT_BLT_CMP: {
            uint8_t op = record[desc.p[0]];
            int diff_idx; bool a_sign, b_sign;
            bool cmp = run_branch_lt(op, record + desc.p[1], record + desc.p[2],
                                     &diff_idx, &a_sign, &b_sign);
            return Fp((uint32_t)cmp);
        }
        case JIT_BLT_CMP_LT: {
            uint8_t op = record[desc.p[0]];
            int diff_idx; bool a_sign, b_sign;
            bool cmp = run_branch_lt(op, record + desc.p[1], record + desc.p[2],
                                     &diff_idx, &a_sign, &b_sign);
            bool ge_op = (op == 2) || (op == 3);
            return Fp((uint32_t)(cmp ^ ge_op));
        }
        case JIT_BLT_DIFF_VAL: {
            uint8_t op = record[desc.p[0]];
            const uint8_t *a = record + desc.p[1];
            const uint8_t *b = record + desc.p[2];
            int diff_idx; bool a_sign, b_sign;
            bool cmp = run_branch_lt(op, a, b, &diff_idx, &a_sign, &b_sign);
            bool ge_op = (op == 2) || (op == 3);
            bool cmp_lt = cmp ^ ge_op;
            if (diff_idx == 4) return Fp::zero();
            if (diff_idx == 3) {
                Fp a_msb = a_sign ? -Fp((uint32_t)(256u - a[3])) : Fp((uint32_t)a[3]);
                Fp b_msb = b_sign ? -Fp((uint32_t)(256u - b[3])) : Fp((uint32_t)b[3]);
                return cmp_lt ? (b_msb - a_msb) : (a_msb - b_msb);
            }
            uint8_t big = cmp_lt ? b[diff_idx] : a[diff_idx];
            uint8_t small = cmp_lt ? a[diff_idx] : b[diff_idx];
            return Fp((uint32_t)(big - small));
        }
        case JIT_BLT_DIFF_MARKER: {
            uint8_t op = record[desc.p[0]];
            int diff_idx; bool a_sign, b_sign;
            run_branch_lt(op, record + desc.p[1], record + desc.p[2],
                          &diff_idx, &a_sign, &b_sign);
            int marker = (int)desc.p[3];
            return Fp((uint32_t)((diff_idx != 4) && (diff_idx == marker)));
        }
        case JIT_BLT_A_MSB_F: {
            uint8_t op = record[desc.p[0]];
            bool signed_op = (op == 0) || (op == 2);
            uint8_t a3 = record[desc.p[1] + 3];
            bool a_sign = signed_op && ((a3 >> 7) == 1);
            return a_sign ? -Fp((uint32_t)(256u - a3)) : Fp((uint32_t)a3);
        }
        case JIT_BLT_B_MSB_F: {
            uint8_t op = record[desc.p[0]];
            bool signed_op = (op == 0) || (op == 2);
            uint8_t b3 = record[desc.p[1] + 3];
            bool b_sign = signed_op && ((b3 >> 7) == 1);
            return b_sign ? -Fp((uint32_t)(256u - b3)) : Fp((uint32_t)b3);
        }
        // ── Auipc rd_data limb ──
        // p[0] = pc_byte_offset, p[1] = imm_byte_offset, p[2] = limb_index.
        case JIT_AUIPC_RD_LIMB: {
            uint32_t pc, imm;
            memcpy(&pc,  record + desc.p[0], 4);
            memcpy(&imm, record + desc.p[1], 4);
            uint32_t rd = pc + (imm << 8);
            return Fp((uint32_t)((rd >> (8 * desc.p[2])) & 0xFFu));
        }
        // ── Jalr to_pc least-significant bit ──
        // p[0] = rs1_byte_offset (u32), p[1] = imm_byte_offset (u16),
        // p[2] = imm_sign_byte_offset (1 byte: 0 or 1).
        case JIT_JALR_TO_PC_LSB: {
            uint32_t rs1; memcpy(&rs1, record + desc.p[0], 4);
            uint16_t imm; memcpy(&imm, record + desc.p[1], 2);
            uint8_t  sign = record[desc.p[2]];
            uint32_t to_pc = rs1 + (uint32_t)imm + (sign ? 0xFFFF0000u : 0u);
            return Fp((uint32_t)(to_pc & 1u));
        }
        // ── Jalr to_pc_limbs[limb_index] ──
        // limb 0: (to_pc & 0xFFFF) >> 1
        // limb 1: to_pc >> 16
        case JIT_JALR_TO_PC_LIMB: {
            uint32_t rs1; memcpy(&rs1, record + desc.p[0], 4);
            uint16_t imm; memcpy(&imm, record + desc.p[1], 2);
            uint8_t  sign = record[desc.p[2]];
            uint32_t to_pc = rs1 + (uint32_t)imm + (sign ? 0xFFFF0000u : 0u);
            uint32_t v = (desc.p[3] == 0u) ? ((to_pc & 0xFFFFu) >> 1) : (to_pc >> 16);
            return Fp(v);
        }
        // ── Jalr rd_data limb (top 3 bytes of pc + 4) ──
        // p[0] = pc_byte_offset, p[1] = limb_index ∈ 0..3
        case JIT_JALR_RD_LIMB: {
            uint32_t pc; memcpy(&pc, record + desc.p[0], 4);
            uint32_t rd = pc + 4u;
            return Fp((uint32_t)((rd >> (8u * (desc.p[1] + 1u))) & 0xFFu));
        }
        default:
            return Fp::zero();
    }
}

// ============================================================================================
// Main JIT tracegen kernel
// ============================================================================================

__global__ void apc_jit_tracegen_kernel(
    Fp* __restrict__ d_output,           // APC trace, column-major (pre-zeroed)
    size_t H,                            // trace height (power of 2)
    int num_apc_calls,                   // actual number of valid rows
    const uint8_t* __restrict__ d_arena, // concatenated arena buffer
    const JitInstructionDesc* __restrict__ d_instructions,
    int n_instructions,
    const JitColumnDesc* __restrict__ d_col_descs,
    uint32_t range_max_bits
) {
    const size_t total_threads = (size_t)gridDim.x * (size_t)blockDim.x;
    const size_t tid = (size_t)blockIdx.x * (size_t)blockDim.x + (size_t)threadIdx.x;

    for (size_t r = tid; r < (size_t)num_apc_calls; r += total_threads) {
        for (int i = 0; i < n_instructions; i++) {
            const JitInstructionDesc instr = d_instructions[i];
            const uint8_t* record = d_arena + instr.arena_offset
                                  + instr.record_offset
                                  + r * instr.record_stride;

            for (uint32_t c = 0; c < instr.col_desc_count; c++) {
                const JitColumnDesc desc = d_col_descs[instr.col_desc_start + c];
                Fp value = eval_jit_column(desc, record, range_max_bits);
                d_output[(size_t)desc.apc_col * H + r] = value;
            }
        }
    }
    // Padding rows beyond num_apc_calls stay zero (buffer is pre-zeroed)
}

// ============================================================================================
// Host launcher
// ============================================================================================

extern "C" int _apc_jit_tracegen(
    Fp*                      d_output,
    size_t                   H,
    int                      num_apc_calls,
    const uint8_t*           d_arena,
    const JitInstructionDesc* d_instructions,
    int                      n_instructions,
    const JitColumnDesc*     d_col_descs,
    uint32_t                 range_max_bits
) {
    if (num_apc_calls == 0) return 0;
    const int block_x = 256;
    const dim3 block(block_x, 1, 1);
    unsigned g = (unsigned)((num_apc_calls + block_x - 1) / block_x);
    if (g == 0u) g = 1u;
    const dim3 grid(g, 1, 1);

    apc_jit_tracegen_kernel<<<grid, block>>>(
        d_output, H, num_apc_calls,
        d_arena, d_instructions, n_instructions,
        d_col_descs, range_max_bits
    );
    return (int)cudaGetLastError();
}

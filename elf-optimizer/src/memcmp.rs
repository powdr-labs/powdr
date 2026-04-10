//! Specialized memcmp routine generation.

use crate::rv_insn::*;

/// Generate the "word_diff" epilogue: given two differing words in X5(t0) and
/// X6(t1), find the first differing byte (little-endian) and return
/// a0 = (byte_from_ptr1 - byte_from_ptr2) as the memcmp result.
/// Uses X7(t2) and X28(t3) as temporaries.
fn rv_generate_word_diff_epilogue() -> Vec<u32> {
    let mut instrs = Vec::new();
    // byte 0 (bits [7:0])
    instrs.push(rv_andi(X7, X5, 0xFF));
    instrs.push(rv_andi(X28, X6, 0xFF));
    let bne0_idx = instrs.len();
    instrs.push(0); // placeholder BNE → diff_done
                    // byte 1 (bits [15:8])
    instrs.push(rv_srli(X7, X5, 8));
    instrs.push(rv_andi(X7, X7, 0xFF));
    instrs.push(rv_srli(X28, X6, 8));
    instrs.push(rv_andi(X28, X28, 0xFF));
    let bne1_idx = instrs.len();
    instrs.push(0); // placeholder BNE → diff_done
                    // byte 2 (bits [23:16])
    instrs.push(rv_srli(X7, X5, 16));
    instrs.push(rv_andi(X7, X7, 0xFF));
    instrs.push(rv_srli(X28, X6, 16));
    instrs.push(rv_andi(X28, X28, 0xFF));
    let bne2_idx = instrs.len();
    instrs.push(0); // placeholder BNE → diff_done
                    // byte 3 — must be the differing one
    instrs.push(rv_srli(X7, X5, 24));
    instrs.push(rv_srli(X28, X6, 24));
    // diff_done: a0 = t2 - t3
    let diff_done_idx = instrs.len();
    instrs.push(rv_sub(X10, X7, X28));
    instrs.push(rv_ret());
    // patch BNEs
    instrs[bne0_idx] = rv_bne(X7, X28, ((diff_done_idx as i32) - (bne0_idx as i32)) * 4);
    instrs[bne1_idx] = rv_bne(X7, X28, ((diff_done_idx as i32) - (bne1_idx as i32)) * 4);
    instrs[bne2_idx] = rv_bne(X7, X28, ((diff_done_idx as i32) - (bne2_idx as i32)) * 4);
    instrs
}

/// Generate a specialized memcmp for a known constant length.
/// a0 = ptr1, a1 = ptr2, a2 = length (already set by caller, not used).
/// Returns a0 = comparison result (<0, 0, >0).
pub(crate) fn rv_generate_specialized_memcmp(length: u32) -> Vec<u32> {
    if length == 0 {
        // return 0
        return vec![rv_addi(X10, X0, 0), rv_ret()];
    }

    if length < 4 {
        // Pure byte-by-byte: short enough that word tricks don't help.
        let mut instrs = Vec::new();
        for i in 0..length {
            instrs.push(rv_lbu(X5, X10, i as i32)); // t0 = ptr1[i]
            instrs.push(rv_lbu(X6, X11, i as i32)); // t1 = ptr2[i]
            let bne_idx = instrs.len();
            instrs.push(0); // placeholder BNE → diff
            let _ = bne_idx; // handled below
        }
        // equal: return 0
        let equal_idx = instrs.len();
        instrs.push(rv_addi(X10, X0, 0));
        instrs.push(rv_ret());
        // byte_diff: return t0 - t1
        let byte_diff_idx = instrs.len();
        instrs.push(rv_sub(X10, X5, X6));
        instrs.push(rv_ret());

        // Patch BNE placeholders (every third instruction starting at index 2)
        for i in 0..length {
            let bne_idx = (i * 3 + 2) as usize;
            instrs[bne_idx] = rv_bne(X5, X6, ((byte_diff_idx as i32) - (bne_idx as i32)) * 4);
        }
        let _ = equal_idx;
        return instrs;
    }

    // length >= 4: alignment-dispatched word compare
    let num_words = length / 4;
    let tail = length % 4;

    let mut instrs = Vec::new();

    // Check alignment: (a0 | a1) & 3
    instrs.push(rv_or(X5, X10, X11)); // t0 = ptr1 | ptr2
    instrs.push(rv_andi(X5, X5, 3)); // t0 = (ptr1 | ptr2) & 3
    let bne_to_byte_idx = instrs.len();
    instrs.push(0); // placeholder BNE t0, x0 → byte_loop

    // ---- Word-aligned path ----
    let word_diff_jump_indices: Vec<usize> = (0..num_words)
        .map(|w| {
            let off = (w * 4) as i32;
            instrs.push(rv_lw(X5, X10, off)); // t0 = *(ptr1 + off)
            instrs.push(rv_lw(X6, X11, off)); // t1 = *(ptr2 + off)
            let idx = instrs.len();
            instrs.push(0); // placeholder BNE → word_diff
            idx
        })
        .collect();

    // Tail bytes (after words)
    let tail_bne_indices: Vec<usize> = (0..tail)
        .map(|i| {
            let off = (num_words * 4 + i) as i32;
            instrs.push(rv_lbu(X5, X10, off)); // t0 = ptr1[off]
            instrs.push(rv_lbu(X6, X11, off)); // t1 = ptr2[off]
            let idx = instrs.len();
            instrs.push(0); // placeholder BNE → byte_diff
            idx
        })
        .collect();

    // Equal: return 0
    instrs.push(rv_addi(X10, X0, 0));
    instrs.push(rv_ret());

    // word_diff epilogue: t0 and t1 have differing words
    let word_diff_idx = instrs.len();
    instrs.extend(rv_generate_word_diff_epilogue());

    // byte_diff: return t0 - t1 (for tail bytes)
    let byte_diff_idx = instrs.len();
    instrs.push(rv_sub(X10, X5, X6));
    instrs.push(rv_ret());

    // ---- Byte-by-byte fallback (unaligned) ----
    let byte_loop_idx = instrs.len();
    let beq_done_idx = instrs.len();
    instrs.push(0); // placeholder BEQ a2, x0 → equal_unaligned
    instrs.push(rv_lbu(X5, X10, 0)); // t0 = *ptr1
    instrs.push(rv_lbu(X6, X11, 0)); // t1 = *ptr2
    let bne_loop_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE → byte_diff_unaligned
    instrs.push(rv_addi(X10, X10, 1)); // ptr1++
    instrs.push(rv_addi(X11, X11, 1)); // ptr2++
    instrs.push(rv_addi(X12, X12, -1)); // a2--
    let j_loop_idx = instrs.len();
    instrs.push(0); // placeholder J → byte_loop
                    // equal_unaligned: return 0
    let equal_unaligned_idx = instrs.len();
    instrs.push(rv_addi(X10, X0, 0));
    instrs.push(rv_ret());
    // byte_diff_unaligned: return t0 - t1
    let byte_diff_unaligned_idx = instrs.len();
    instrs.push(rv_sub(X10, X5, X6));
    instrs.push(rv_ret());

    // ---- Patch all branch placeholders ----
    instrs[bne_to_byte_idx] = rv_bne(
        X5,
        X0,
        ((byte_loop_idx as i32) - (bne_to_byte_idx as i32)) * 4,
    );
    for &idx in &word_diff_jump_indices {
        instrs[idx] = rv_bne(X5, X6, ((word_diff_idx as i32) - (idx as i32)) * 4);
    }
    for &idx in &tail_bne_indices {
        instrs[idx] = rv_bne(X5, X6, ((byte_diff_idx as i32) - (idx as i32)) * 4);
    }
    instrs[beq_done_idx] = rv_beq(
        X12,
        X0,
        ((equal_unaligned_idx as i32) - (beq_done_idx as i32)) * 4,
    );
    instrs[bne_loop_diff_idx] = rv_bne(
        X5,
        X6,
        ((byte_diff_unaligned_idx as i32) - (bne_loop_diff_idx as i32)) * 4,
    );
    instrs[j_loop_idx] = rv_jal(X0, ((byte_loop_idx as i32) - (j_loop_idx as i32)) * 4);

    instrs
}

/// Generate a generic optimized memcmp (loop-based, word-aligned when possible).
/// Used for call sites where the length is not a compile-time constant.
/// a0 = ptr1, a1 = ptr2, a2 = length.
/// Returns a0 = comparison result.
pub(crate) fn rv_generate_optimized_memcmp_loop() -> Vec<u32> {
    let mut instrs = Vec::new();

    // If length == 0, return 0
    let beq_zero_idx = instrs.len();
    instrs.push(0); // placeholder BEQ a2, x0 → return_zero

    // Check alignment: (a0 | a1) & 3
    instrs.push(rv_or(X5, X10, X11)); // t0 = ptr1 | ptr2
    instrs.push(rv_andi(X5, X5, 3)); // t0 &= 3
    let bne_unaligned_idx = instrs.len();
    instrs.push(0); // placeholder BNE t0, x0 → byte_loop

    // ---- Word-aligned path ----
    // X13 = number of full words (a2 >> 2)
    instrs.push(rv_srli(X13, X12, 2));
    // X14 = tail bytes (a2 & 3)
    instrs.push(rv_andi(X14, X12, 3));

    // word_loop:
    let word_loop_idx = instrs.len();
    let beq_word_done_idx = instrs.len();
    instrs.push(0); // placeholder BEQ X13, x0 → tail_loop
    instrs.push(rv_lw(X5, X10, 0)); // t0 = *ptr1
    instrs.push(rv_lw(X6, X11, 0)); // t1 = *ptr2
    let bne_word_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE → word_diff
    instrs.push(rv_addi(X10, X10, 4)); // ptr1 += 4
    instrs.push(rv_addi(X11, X11, 4)); // ptr2 += 4
    instrs.push(rv_addi(X13, X13, -1)); // word_count--
    let j_word_loop_idx = instrs.len();
    instrs.push(0); // placeholder J → word_loop

    // tail_loop: compare remaining X14 bytes
    let tail_loop_idx = instrs.len();
    let beq_tail_done_idx = instrs.len();
    instrs.push(0); // placeholder BEQ X14, x0 → return_zero
    instrs.push(rv_lbu(X5, X10, 0));
    instrs.push(rv_lbu(X6, X11, 0));
    let bne_tail_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE → byte_diff
    instrs.push(rv_addi(X10, X10, 1));
    instrs.push(rv_addi(X11, X11, 1));
    instrs.push(rv_addi(X14, X14, -1));
    let j_tail_loop_idx = instrs.len();
    instrs.push(0); // placeholder J → tail_loop

    // return_zero:
    let return_zero_idx = instrs.len();
    instrs.push(rv_addi(X10, X0, 0));
    instrs.push(rv_ret());

    // word_diff: extract first differing byte from t0, t1
    let word_diff_idx = instrs.len();
    instrs.extend(rv_generate_word_diff_epilogue());

    // byte_diff: return t0 - t1
    let byte_diff_idx = instrs.len();
    instrs.push(rv_sub(X10, X5, X6));
    instrs.push(rv_ret());

    // ---- Unaligned byte-by-byte fallback loop ----
    let byte_loop_idx = instrs.len();
    let beq_byte_done_idx = instrs.len();
    instrs.push(0); // placeholder BEQ a2, x0 → return_zero
    instrs.push(rv_lbu(X5, X10, 0));
    instrs.push(rv_lbu(X6, X11, 0));
    let bne_byte_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE → byte_diff
    instrs.push(rv_addi(X10, X10, 1));
    instrs.push(rv_addi(X11, X11, 1));
    instrs.push(rv_addi(X12, X12, -1));
    let j_byte_loop_idx = instrs.len();
    instrs.push(0); // placeholder J → byte_loop

    // ---- Patch all branches ----
    instrs[beq_zero_idx] = rv_beq(
        X12,
        X0,
        ((return_zero_idx as i32) - (beq_zero_idx as i32)) * 4,
    );
    instrs[bne_unaligned_idx] = rv_bne(
        X5,
        X0,
        ((byte_loop_idx as i32) - (bne_unaligned_idx as i32)) * 4,
    );
    instrs[beq_word_done_idx] = rv_beq(
        X13,
        X0,
        ((tail_loop_idx as i32) - (beq_word_done_idx as i32)) * 4,
    );
    instrs[bne_word_diff_idx] = rv_bne(
        X5,
        X6,
        ((word_diff_idx as i32) - (bne_word_diff_idx as i32)) * 4,
    );
    instrs[j_word_loop_idx] = rv_jal(X0, ((word_loop_idx as i32) - (j_word_loop_idx as i32)) * 4);
    instrs[beq_tail_done_idx] = rv_beq(
        X14,
        X0,
        ((return_zero_idx as i32) - (beq_tail_done_idx as i32)) * 4,
    );
    instrs[bne_tail_diff_idx] = rv_bne(
        X5,
        X6,
        ((byte_diff_idx as i32) - (bne_tail_diff_idx as i32)) * 4,
    );
    instrs[j_tail_loop_idx] = rv_jal(X0, ((tail_loop_idx as i32) - (j_tail_loop_idx as i32)) * 4);
    instrs[beq_byte_done_idx] = rv_beq(
        X12,
        X0,
        ((return_zero_idx as i32) - (beq_byte_done_idx as i32)) * 4,
    );
    instrs[bne_byte_diff_idx] = rv_bne(
        X5,
        X6,
        ((byte_diff_idx as i32) - (bne_byte_diff_idx as i32)) * 4,
    );
    instrs[j_byte_loop_idx] = rv_jal(X0, ((byte_loop_idx as i32) - (j_byte_loop_idx as i32)) * 4);

    instrs
}

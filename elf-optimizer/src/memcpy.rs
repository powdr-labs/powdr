//! Specialized memcpy and memmove routine generation.

use crate::rv_insn::*;

/// Generate head bytes + word copies + tail bytes as raw RISC-V instructions.
fn rv_generate_head_word_tail(head_bytes: u32, total_length: u32) -> Vec<u32> {
    let remaining = total_length - head_bytes;
    let num_words = remaining / 4;
    let tail = remaining % 4;

    let mut instrs = Vec::new();
    for i in 0..head_bytes {
        instrs.push(rv_lbu(X13, X11, i as i32));
        instrs.push(rv_sb(X13, X10, i as i32));
    }
    for w in 0..num_words {
        let offset = (head_bytes + w * 4) as i32;
        instrs.push(rv_lw(X13, X11, offset));
        instrs.push(rv_sw(X13, X10, offset));
    }
    for i in 0..tail {
        let offset = (head_bytes + num_words * 4 + i) as i32;
        instrs.push(rv_lbu(X13, X11, offset));
        instrs.push(rv_sb(X13, X10, offset));
    }
    instrs
}

/// Generate a specialized memcpy as raw RISC-V u32 instructions for a known constant length.
pub(crate) fn rv_generate_specialized_memcpy(length: u32) -> Vec<u32> {
    if length == 0 {
        return vec![rv_ret()];
    }

    if length < 4 {
        let mut instrs = Vec::new();
        for i in 0..length {
            instrs.push(rv_lbu(X13, X11, i as i32));
            instrs.push(rv_sb(X13, X10, i as i32));
        }
        instrs.push(rv_ret());
        return instrs;
    }

    rv_generate_alignment_dispatched_copy(length, false)
}

/// Generate backward copy: same layout as forward (head bytes, words, tail bytes)
/// but copied in reverse order for memmove overlap safety.
fn rv_generate_backward_head_word_tail(head_bytes: u32, total_length: u32) -> Vec<u32> {
    let remaining = total_length - head_bytes;
    let num_words = remaining / 4;
    let tail = remaining % 4;

    let mut instrs = Vec::new();
    // Tail bytes (at the end), backward
    for i in (0..tail).rev() {
        let offset = (head_bytes + num_words * 4 + i) as i32;
        instrs.push(rv_lbu(X13, X11, offset));
        instrs.push(rv_sb(X13, X10, offset));
    }
    // Words, backward
    for w in (0..num_words).rev() {
        let offset = (head_bytes + w * 4) as i32;
        instrs.push(rv_lw(X13, X11, offset));
        instrs.push(rv_sw(X13, X10, offset));
    }
    // Head bytes, backward
    for i in (0..head_bytes).rev() {
        instrs.push(rv_lbu(X13, X11, i as i32));
        instrs.push(rv_sb(X13, X10, i as i32));
    }
    instrs
}

/// Generate an alignment-dispatched copy block (forward or backward).
fn rv_generate_alignment_dispatched_copy(length: u32, backward: bool) -> Vec<u32> {
    let gen_copy = if backward {
        rv_generate_backward_head_word_tail
    } else {
        rv_generate_head_word_tail
    };

    let mut instrs = Vec::new();

    // Alignment check
    instrs.push(rv_andi(X12, X11, 3)); // src & 3
    instrs.push(rv_andi(X14, X10, 3)); // dst & 3
    instrs.push(rv_or(X13, X12, X14)); // either misaligned?
    let bne_not_both_idx = instrs.len();
    instrs.push(0); // placeholder BNE

    // Path 1: Both aligned
    instrs.extend(gen_copy(0, length));
    instrs.push(rv_ret());

    // Not both aligned
    let not_both_aligned_idx = instrs.len();
    let bne_diff_idx = instrs.len();
    instrs.push(0); // placeholder BNE

    // Same misalignment dispatch
    instrs.push(rv_addi(X13, X0, 1));
    let beq_off1_idx = instrs.len();
    instrs.push(0); // placeholder BEQ
    instrs.push(rv_addi(X13, X0, 2));
    let beq_off2_idx = instrs.len();
    instrs.push(0); // placeholder BEQ

    // off by 3 → 1 head byte
    instrs.extend(gen_copy(1, length));
    instrs.push(rv_ret());

    // off by 2 → 2 head bytes
    let off2_idx = instrs.len();
    instrs.extend(gen_copy(2, length));
    instrs.push(rv_ret());

    // off by 1 → 3 head bytes
    let off1_idx = instrs.len();
    instrs.extend(gen_copy(3, length));
    instrs.push(rv_ret());

    // Byte-by-byte fallback
    let byte_path_idx = instrs.len();
    let range: Vec<u32> = if backward {
        (0..length).rev().collect()
    } else {
        (0..length).collect()
    };
    for i in range {
        instrs.push(rv_lbu(X13, X11, i as i32));
        instrs.push(rv_sb(X13, X10, i as i32));
    }
    instrs.push(rv_ret());

    // Patch branch offsets
    instrs[bne_not_both_idx] = rv_bne(
        X13,
        X0,
        ((not_both_aligned_idx as i32) - (bne_not_both_idx as i32)) * 4,
    );
    instrs[bne_diff_idx] = rv_bne(
        X12,
        X14,
        ((byte_path_idx as i32) - (bne_diff_idx as i32)) * 4,
    );
    instrs[beq_off1_idx] = rv_beq(X12, X13, ((off1_idx as i32) - (beq_off1_idx as i32)) * 4);
    instrs[beq_off2_idx] = rv_beq(X12, X13, ((off2_idx as i32) - (beq_off2_idx as i32)) * 4);

    instrs
}

/// Generate a specialized memcpy for known-aligned, known-length call sites.
/// Both a0 and a1 are 4-byte-aligned, so skip alignment dispatch entirely.
pub(crate) fn rv_generate_aligned_memcpy(length: u32) -> Vec<u32> {
    if length == 0 {
        return vec![rv_ret()];
    }
    let num_words = length / 4;
    let tail = length % 4;
    let mut instrs = Vec::new();
    for w in 0..num_words {
        let offset = (w * 4) as i32;
        instrs.push(rv_lw(X13, X11, offset));
        instrs.push(rv_sw(X13, X10, offset));
    }
    for i in 0..tail {
        let offset = (num_words * 4 + i) as i32;
        instrs.push(rv_lbu(X13, X11, offset));
        instrs.push(rv_sb(X13, X10, offset));
    }
    instrs.push(rv_ret());
    instrs
}

/// Generate a specialized memmove for known-aligned, known-length call sites.
/// Both pointers 4-byte-aligned; still needs direction check for overlap safety.
pub(crate) fn rv_generate_aligned_memmove(length: u32) -> Vec<u32> {
    if length == 0 {
        return vec![rv_ret()];
    }

    let num_words = length / 4;
    let tail = length % 4;

    let mut instrs = Vec::new();

    // Direction check: if dst < src, forward copy is safe
    let bltu_fwd_idx = instrs.len();
    instrs.push(0); // placeholder BLTU

    // Backward path (dst >= src): copy from end to start
    for i in (0..tail).rev() {
        let offset = (num_words * 4 + i) as i32;
        instrs.push(rv_lbu(X13, X11, offset));
        instrs.push(rv_sb(X13, X10, offset));
    }
    for w in (0..num_words).rev() {
        let offset = (w * 4) as i32;
        instrs.push(rv_lw(X13, X11, offset));
        instrs.push(rv_sw(X13, X10, offset));
    }
    instrs.push(rv_ret());

    // Forward path (dst < src)
    let forward_start = instrs.len();
    for w in 0..num_words {
        let offset = (w * 4) as i32;
        instrs.push(rv_lw(X13, X11, offset));
        instrs.push(rv_sw(X13, X10, offset));
    }
    for i in 0..tail {
        let offset = (num_words * 4 + i) as i32;
        instrs.push(rv_lbu(X13, X11, offset));
        instrs.push(rv_sb(X13, X10, offset));
    }
    instrs.push(rv_ret());

    instrs[bltu_fwd_idx] = rv_bltu(X10, X11, ((forward_start - bltu_fwd_idx) as i32) * 4);

    instrs
}
/// Unlike memcpy, memmove handles overlapping src/dst by choosing copy direction.
pub(crate) fn rv_generate_specialized_memmove(length: u32) -> Vec<u32> {
    if length == 0 {
        return vec![rv_ret()];
    }

    if length < 4 {
        let mut instrs = Vec::new();
        // Direction check: if dst < src, forward copy is safe
        let bltu_fwd_idx = instrs.len();
        instrs.push(0); // placeholder BLTU

        // Backward byte copy (dst >= src)
        for i in (0..length).rev() {
            instrs.push(rv_lbu(X13, X11, i as i32));
            instrs.push(rv_sb(X13, X10, i as i32));
        }
        instrs.push(rv_ret());

        // Forward byte copy (dst < src)
        let forward_start = instrs.len();
        for i in 0..length {
            instrs.push(rv_lbu(X13, X11, i as i32));
            instrs.push(rv_sb(X13, X10, i as i32));
        }
        instrs.push(rv_ret());

        instrs[bltu_fwd_idx] = rv_bltu(X10, X11, ((forward_start - bltu_fwd_idx) as i32) * 4);
        return instrs;
    }

    let mut instrs = Vec::new();

    // Direction check: if dst < src, jump to forward copy
    let bltu_fwd_idx = instrs.len();
    instrs.push(0); // placeholder BLTU

    // Backward path (dst >= src)
    instrs.extend(rv_generate_alignment_dispatched_copy(length, true));

    // Forward path (dst < src)
    let forward_start = instrs.len();
    instrs.extend(rv_generate_alignment_dispatched_copy(length, false));

    instrs[bltu_fwd_idx] = rv_bltu(X10, X11, ((forward_start - bltu_fwd_idx) as i32) * 4);

    instrs
}

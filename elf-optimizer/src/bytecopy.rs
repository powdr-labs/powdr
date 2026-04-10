//! Inline bytecopy optimizer (DFG-based).
//!
//! Detects runs of LBU/SB instructions, builds a data flow graph of byte-level
//! copy pairs, groups them into word-level transfers (including swap detection),
//! and generates optimized routines using LW/SW when addresses are word-aligned.

use std::collections::{BTreeSet, HashMap, HashSet};

use powdr_riscv_elf::debug_info::SymbolTable;

use crate::rv_insn::*;

const MIN_BYTECOPY_WORDS: usize = 3;

/// A word-level transfer: copy 4 consecutive bytes as a single LW/SW.
#[derive(Debug, Clone)]
pub(crate) struct WordTransfer {
    pub(crate) src_offset: i32,
    pub(crate) dst_offset: i32,
}

/// DFG analysis of a bytecopy sequence.
pub(crate) struct BytecopyDfg {
    pub(crate) start_idx: usize,
    pub(crate) end_idx: usize,
    pub(crate) base_reg: u32,
    pub(crate) word_transfers: Vec<WordTransfer>,
    /// Registers safe to clobber at entry (first use in sequence is LBU write).
    pub(crate) clobbered_regs: Vec<u32>,
    pub(crate) original_insns: Vec<u32>,
}

/// Scan for LBU/SB runs, build copy-pair DFG, group into word transfers.
pub(crate) fn scan_bytecopy_dfg(instructions: &[u32]) -> Vec<BytecopyDfg> {
    let mut results = Vec::new();
    let mut i = 0;

    while i < instructions.len() {
        let insn = instructions[i];
        if !is_lbu(insn) && !is_sb(insn) {
            i += 1;
            continue;
        }

        let base_reg = rv_rs1(insn);
        if base_reg == X0 {
            i += 1;
            continue;
        }

        let start = i;
        let mut j = i;
        while j < instructions.len() {
            let insn_j = instructions[j];
            if (is_lbu(insn_j) || is_sb(insn_j)) && rv_rs1(insn_j) == base_reg {
                j += 1;
            } else {
                break;
            }
        }

        if j - start < 8 {
            i = j;
            continue;
        }

        // Track which registers are safe to clobber at entry
        let mut first_use_is_write = [false; 32];
        let mut reg_seen = [false; 32];

        // Build copy pairs via register tracking
        let mut reg_src: [Option<i32>; 32] = [None; 32];
        let mut copies: Vec<(i32, i32)> = Vec::new();
        let mut clobbered_set = BTreeSet::new();

        for &insn_k in &instructions[start..j] {
            if is_lbu(insn_k) {
                let rd = rv_rd(insn_k);
                if rd != 0 && rd != base_reg {
                    if !reg_seen[rd as usize] {
                        first_use_is_write[rd as usize] = true;
                        reg_seen[rd as usize] = true;
                    }
                    reg_src[rd as usize] = Some(rv_imm_i(insn_k));
                    clobbered_set.insert(rd);
                }
            } else {
                let rs2 = rv_rs2(insn_k);
                if !reg_seen[rs2 as usize] {
                    reg_seen[rs2 as usize] = true;
                }
                if let Some(src) = reg_src[rs2 as usize].take() {
                    copies.push((src, rv_imm_s(insn_k)));
                }
            }
        }

        let word_transfers = group_word_transfers(&copies);

        if word_transfers.len() >= MIN_BYTECOPY_WORDS {
            let clobbered_regs: Vec<u32> = clobbered_set
                .into_iter()
                .filter(|&r| r != base_reg && first_use_is_write[r as usize])
                .collect();

            if clobbered_regs.len() >= 2 {
                results.push(BytecopyDfg {
                    start_idx: start,
                    end_idx: j,
                    base_reg,
                    word_transfers,
                    clobbered_regs,
                    original_insns: instructions[start..j].to_vec(),
                });
            }
        }

        i = j;
    }

    results
}

/// Group byte-level copy pairs into word transfers.
/// A word transfer is 4 consecutive copies (s,d),(s+1,d+1),(s+2,d+2),(s+3,d+3)
/// with matching alignment class (s%4 == d%4).
pub(crate) fn group_word_transfers(copies: &[(i32, i32)]) -> Vec<WordTransfer> {
    let mut src_to_dst: HashMap<i32, i32> = HashMap::new();
    for &(s, d) in copies {
        src_to_dst.insert(s, d);
    }

    let mut used = HashSet::new();
    let mut transfers = Vec::new();

    let mut sorted: Vec<(i32, i32)> = copies.to_vec();
    sorted.sort_by_key(|(s, _)| *s);

    for &(s, d) in &sorted {
        if used.contains(&s) {
            continue;
        }
        let d1 = src_to_dst.get(&(s + 1)).copied();
        let d2 = src_to_dst.get(&(s + 2)).copied();
        let d3 = src_to_dst.get(&(s + 3)).copied();
        if let (Some(d1), Some(d2), Some(d3)) = (d1, d2, d3) {
            if d1 == d + 1
                && d2 == d + 2
                && d3 == d + 3
                && s.rem_euclid(4) == d.rem_euclid(4)
                && !used.contains(&(s + 1))
                && !used.contains(&(s + 2))
                && !used.contains(&(s + 3))
            {
                transfers.push(WordTransfer {
                    src_offset: s,
                    dst_offset: d,
                });
                used.extend([s, s + 1, s + 2, s + 3]);
            }
        }
    }

    // Filter to the dominant alignment class so a single runtime check suffices
    if !transfers.is_empty() {
        let mut class_counts: HashMap<i32, usize> = HashMap::new();
        for t in &transfers {
            *class_counts.entry(t.src_offset.rem_euclid(4)).or_default() += 1;
        }
        let best_class = class_counts.into_iter().max_by_key(|(_, c)| *c).unwrap().0;
        transfers.retain(|t| t.src_offset.rem_euclid(4) == best_class);
    }

    transfers.sort_by_key(|t| t.src_offset);
    transfers
}

/// Pair up word transfers that form swaps (A→B and B→A both exist).
pub(crate) fn pair_swaps(
    transfers: &[WordTransfer],
) -> (Vec<(WordTransfer, WordTransfer)>, Vec<WordTransfer>) {
    let mut swap_pairs = Vec::new();
    let mut singles = Vec::new();
    let mut used: HashSet<usize> = HashSet::new();

    for (i, t) in transfers.iter().enumerate() {
        if used.contains(&i) {
            continue;
        }
        let rev = transfers.iter().enumerate().find(|(j, r)| {
            !used.contains(j)
                && *j != i
                && r.src_offset == t.dst_offset
                && r.dst_offset == t.src_offset
        });
        if let Some((j, _)) = rev {
            swap_pairs.push((t.clone(), transfers[j].clone()));
            used.insert(i);
            used.insert(j);
        } else {
            singles.push(t.clone());
            used.insert(i);
        }
    }

    (swap_pairs, singles)
}

/// Encode AUIPC+JALR for an unconditional jump (rd=x0, no link) using `tmp` as
/// the scratch register for the upper immediate.
pub(crate) fn rv_encode_jump(from_addr: u32, to_addr: u32, tmp: u32) -> (u32, u32) {
    let diff = to_addr.wrapping_sub(from_addr) as i32;
    let mut upper = diff >> 12;
    let lower = diff & 0xFFF;
    if lower >= 0x800 {
        upper += 1;
    }
    let auipc = rv_auipc(tmp, (upper as u32) << 12);
    let jalr = rv_jalr(X0, tmp, diff - (upper << 12));
    (auipc, jalr)
}

/// Generate an optimized routine from DFG analysis.
/// Emits: alignment check → aligned LW/SW path → JAL skip → byte fallback (original insns).
pub(crate) fn generate_dfg_routine(dfg: &BytecopyDfg) -> Vec<u32> {
    let mut code = Vec::new();

    let tmp1 = dfg.clobbered_regs[0];
    let tmp2 = dfg.clobbered_regs[1];

    let (swaps, singles) = pair_swaps(&dfg.word_transfers);

    // Alignment check: ADDI tmp1, base, first_transfer_src; ANDI tmp1, tmp1, 3; BNE → fallback
    let first_src = dfg
        .word_transfers
        .first()
        .map(|t| t.src_offset)
        .unwrap_or(0);
    code.push(rv_addi(tmp1, dfg.base_reg, first_src));
    code.push(rv_andi(tmp1, tmp1, 3));
    let branch_idx = code.len();
    code.push(0); // BNE placeholder

    // Aligned path: swaps use two temps, singles use one
    for (a, _b) in &swaps {
        code.push(rv_lw(tmp1, dfg.base_reg, a.src_offset));
        code.push(rv_lw(tmp2, dfg.base_reg, a.dst_offset));
        code.push(rv_sw(tmp1, dfg.base_reg, a.dst_offset));
        code.push(rv_sw(tmp2, dfg.base_reg, a.src_offset));
    }
    for s in &singles {
        code.push(rv_lw(tmp1, dfg.base_reg, s.src_offset));
        code.push(rv_sw(tmp1, dfg.base_reg, s.dst_offset));
    }

    // Skip over fallback
    let skip_idx = code.len();
    code.push(0); // JAL x0 placeholder

    // Byte fallback: replay original LBU/SB verbatim
    let fallback_offset = ((code.len() - branch_idx) as i32) * 4;
    code[branch_idx] = rv_bne(tmp1, X0, fallback_offset);

    for &insn in &dfg.original_insns {
        code.push(insn);
    }

    let skip_offset = ((code.len() - skip_idx) as i32) * 4;
    code[skip_idx] = rv_jal(X0, skip_offset);

    code
}

/// Scan the ELF for inline LBU/SB sequences and replace them with
/// DFG-optimized routines using word loads/stores when aligned.
pub(crate) fn optimize_bytecopy(
    instructions: &mut Vec<u32>,
    symbols: &mut SymbolTable,
    pc_base: u32,
) {
    let dfgs = scan_bytecopy_dfg(instructions);
    if dfgs.is_empty() {
        return;
    }

    println!(
        "bytecopy_opt: found {} qualifying DFG sequence(s)",
        dfgs.len()
    );

    let nop = rv_addi(X0, X0, 0);
    let mut patched = 0;

    for dfg in &dfgs {
        let seq_addr = pc_base + (dfg.start_idx as u32) * 4;
        let return_addr = pc_base + (dfg.end_idx as u32) * 4;

        let tmp = dfg.clobbered_regs[0];

        let mut routine = generate_dfg_routine(dfg);

        // Append return jump
        let routine_start_addr = pc_base + (instructions.len() as u32) * 4;
        let return_jump_addr = routine_start_addr + (routine.len() as u32) * 4;
        let (ret_auipc, ret_jalr) = rv_encode_jump(return_jump_addr, return_addr, tmp);
        routine.push(ret_auipc);
        routine.push(ret_jalr);

        let (swaps, singles) = pair_swaps(&dfg.word_transfers);
        let routine_insns = routine.len();
        symbols.insert(
            routine_start_addr,
            format!(
                "bytecopy_dfg_{}w{}s",
                swaps.len() * 2 + singles.len(),
                swaps.len()
            ),
        );
        instructions.extend(routine);

        // Patch original site
        let (fwd_auipc, fwd_jalr) = rv_encode_jump(seq_addr, routine_start_addr, tmp);
        let verify = rv_auipc_jalr_target(fwd_auipc, fwd_jalr, seq_addr);
        assert_eq!(
            verify, routine_start_addr,
            "bytecopy fwd jump mismatch at 0x{seq_addr:x}: got 0x{verify:x}, expected 0x{routine_start_addr:x}"
        );

        instructions[dfg.start_idx] = fwd_auipc;
        instructions[dfg.start_idx + 1] = fwd_jalr;
        for item in instructions
            .iter_mut()
            .take(dfg.end_idx)
            .skip(dfg.start_idx + 2)
        {
            *item = nop;
        }

        println!(
            "bytecopy_opt: DFG at 0x{seq_addr:x} (base=x{}, {} word transfers, {} swaps, {} singles), \
             routine at 0x{routine_start_addr:x} ({routine_insns} insns), {} insns NOP'd",
            dfg.base_reg,
            dfg.word_transfers.len(),
            swaps.len(),
            singles.len(),
            dfg.end_idx - dfg.start_idx - 2,
        );
        patched += 1;
    }

    println!("bytecopy_opt: patched {patched} DFG sequence(s)");
}

//! Call-site scanning, constant resolution, and call encoding/patching.

use std::collections::BTreeMap;

use powdr_riscv_elf::debug_info::SymbolTable;
use rustc_demangle::demangle;

use crate::rv_insn::*;

pub(crate) const MAX_MEMCPY_LENGTH: u32 = 128;

/// Try to resolve the constant value loaded into a register by scanning
/// backwards from `end_idx` (exclusive). Handles:
///   - ADDI rd, x0, imm  (small constants)
///   - LUI rd, upper  followed later by  ADDI rd, rd, lower
///
/// Returns None if the value cannot be statically determined.
/// `max_value`: if Some, reject constants above this threshold.
pub(crate) fn rv_resolve_reg_constant(
    instructions: &[u32],
    end_idx: usize,
    reg: u32,
    max_value: Option<u32>,
) -> Option<u32> {
    let start = end_idx.saturating_sub(16);
    for i in (start..end_idx).rev() {
        let insn = instructions[i];
        let rd = rv_rd(insn);
        if rd != reg {
            continue;
        }
        if is_addi(insn) && rv_rs1(insn) == X0 {
            // li reg, small_imm  →  addi reg, x0, imm
            let imm = rv_imm_i(insn);
            if imm < 0 {
                return None;
            }
            let val = imm as u32;
            if let Some(max) = max_value {
                if val > max {
                    return None;
                }
            }
            return Some(val);
        }
        if is_addi(insn) && rv_rs1(insn) == reg {
            // addi reg, reg, lower — look further back for LUI reg, upper
            let lower = rv_imm_i(insn);
            for j in (start..i).rev() {
                let insn2 = instructions[j];
                if rv_rd(insn2) != reg {
                    continue;
                }
                if is_lui(insn2) {
                    let upper = rv_imm_u(insn2);
                    let val = upper.wrapping_add(lower as u32);
                    if let Some(max) = max_value {
                        if val > max {
                            return None;
                        }
                    }
                    return Some(val);
                }
                // Some other write to reg before we found LUI — give up
                return None;
            }
            return None;
        }
        // Any other write to the register — give up
        return None;
    }
    None
}

/// Try to resolve the constant value loaded into register a2 (x12) by scanning
/// backwards from `end_idx` (exclusive) in the instruction stream.
pub(crate) fn rv_resolve_a2_constant(instructions: &[u32], end_idx: usize) -> Option<u32> {
    rv_resolve_reg_constant(instructions, end_idx, X12, Some(MAX_MEMCPY_LENGTH))
}

/// Scan all call sites to any of `target_addrs` and report how many have a
/// compile-time constant in a2 (x12) vs. dynamic values.
pub(crate) fn analyze_a2_constants(
    instructions: &[u32],
    pc_base: u32,
    target_addrs: &[u32],
    label: &str,
) {
    let mut total_calls = 0u32;
    let mut constant_calls = 0u32;
    let mut constant_histogram: BTreeMap<u32, u32> = BTreeMap::new();

    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];
        if !is_auipc(insn0) || !is_jalr(insn1) {
            continue;
        }
        let auipc_rd = rv_rd(insn0);
        if rv_rs1(insn1) != auipc_rd || rv_rd(insn1) != X1 {
            continue;
        }
        let auipc_addr = pc_base + (i as u32) * 4;
        let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
        if !target_addrs.contains(&target) {
            continue;
        }

        total_calls += 1;
        // No max_value cap — we want to see all constants.
        if let Some(val) = rv_resolve_reg_constant(instructions, i, X12, None) {
            constant_calls += 1;
            *constant_histogram.entry(val).or_default() += 1;
        }
    }

    if total_calls == 0 {
        println!("{label}: no call sites found");
        return;
    }

    let dynamic_calls = total_calls - constant_calls;
    let pct = (constant_calls as f64 / total_calls as f64) * 100.0;
    println!(
        "{label}: {constant_calls}/{total_calls} call sites ({pct:.1}%) have compile-time constant size, \
         {dynamic_calls} dynamic"
    );
    if !constant_histogram.is_empty() {
        let mut by_count: Vec<(u32, u32)> = constant_histogram.into_iter().collect();
        by_count.sort_by(|a, b| b.1.cmp(&a.1));
        println!("{label}: constant sizes (size × count):");
        for (val, count) in &by_count {
            println!("{label}:   {val} × {count}");
        }
    }
}

/// Debug helper: scan for all AUIPC+JALR pairs and JAL instructions that target `target_addr`,
/// printing diagnostics about what's found and what's filtered out.
pub(crate) fn debug_scan_calls(instructions: &[u32], pc_base: u32, target_addr: u32, label: &str) {
    let mut auipc_jalr_found = 0u32;
    let mut jal_found = 0u32;
    let mut auipc_jalr_rd_mismatch = 0u32;
    let mut auipc_jalr_not_x1 = 0u32;

    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];

        // Check AUIPC+JALR pattern
        if is_auipc(insn0) && is_jalr(insn1) {
            let auipc_rd = rv_rd(insn0);
            let jalr_rs1 = rv_rs1(insn1);
            let jalr_rd = rv_rd(insn1);
            let auipc_addr = pc_base + (i as u32) * 4;
            let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
            if target == target_addr {
                auipc_jalr_found += 1;
                if jalr_rs1 != auipc_rd {
                    auipc_jalr_rd_mismatch += 1;
                }
                if jalr_rd != X1 {
                    auipc_jalr_not_x1 += 1;
                    println!(
                        "  {label}: AUIPC+JALR at 0x{auipc_addr:x} targets 0x{target:x}, \
                         but JALR rd={jalr_rd} (not x1/ra)"
                    );
                }
            }
        }

        // Check JAL pattern
        let opcode = insn0 & 0x7F;
        if opcode == 0x6F {
            let imm = rv_imm_j(insn0);
            let addr = pc_base + (i as u32) * 4;
            let target = (addr as i64 + imm as i64) as u32;
            if target == target_addr {
                jal_found += 1;
            }
        }
    }

    println!(
        "  {label} call scan: AUIPC+JALR targeting 0x{target_addr:x}: {auipc_jalr_found} \
         (rd mismatch: {auipc_jalr_rd_mismatch}, not-x1: {auipc_jalr_not_x1}), \
         JAL targeting: {jal_found}"
    );
}

/// Scan ALL call sites to `target_addrs`, partitioned into constant-length
/// (within MAX_MEMCPY_LENGTH) and dynamic-length groups.
pub(crate) fn scan_all_call_sites_multi(
    instructions: &[u32],
    pc_base: u32,
    target_addrs: &[u32],
) -> (BTreeMap<u32, Vec<usize>>, Vec<usize>) {
    let mut by_length: BTreeMap<u32, Vec<usize>> = BTreeMap::new();
    let mut dynamic: Vec<usize> = Vec::new();

    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];
        if !is_auipc(insn0) || !is_jalr(insn1) {
            continue;
        }
        let auipc_rd = rv_rd(insn0);
        if rv_rs1(insn1) != auipc_rd || rv_rd(insn1) != X1 {
            continue;
        }
        let auipc_addr = pc_base + (i as u32) * 4;
        let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
        if !target_addrs.contains(&target) {
            continue;
        }
        match rv_resolve_reg_constant(instructions, i, X12, Some(MAX_MEMCPY_LENGTH)) {
            Some(length) => by_length.entry(length).or_default().push(i),
            None => dynamic.push(i),
        }
    }
    (by_length, dynamic)
}

/// Encode AUIPC+JALR pair for a call from `from_addr` to `to_addr`.
/// Returns (auipc_insn, jalr_insn) using register `rd` as the intermediate.
pub(crate) fn rv_encode_call(from_addr: u32, to_addr: u32, rd: u32) -> (u32, u32) {
    let diff = to_addr.wrapping_sub(from_addr) as i32;
    let mut upper = diff >> 12;
    let lower = diff & 0xFFF;
    // If lower would be sign-extended as negative, adjust upper
    if lower >= 0x800 {
        upper += 1;
    }
    let auipc = rv_auipc(rd, (upper as u32) << 12);
    let jalr = rv_jalr(X1, rd, diff - (upper << 12));
    (auipc, jalr)
}

/// Scan for call sites to any of `target_addrs` with constant a2.
pub(crate) fn scan_call_sites_multi(
    instructions: &[u32],
    pc_base: u32,
    target_addrs: &[u32],
) -> BTreeMap<u32, Vec<usize>> {
    let mut calls_by_length: BTreeMap<u32, Vec<usize>> = BTreeMap::new();
    for i in 0..instructions.len().saturating_sub(1) {
        let insn0 = instructions[i];
        let insn1 = instructions[i + 1];
        if !is_auipc(insn0) || !is_jalr(insn1) {
            continue;
        }
        let auipc_rd = rv_rd(insn0);
        if rv_rs1(insn1) != auipc_rd || rv_rd(insn1) != X1 {
            continue;
        }
        let auipc_addr = pc_base + (i as u32) * 4;
        let target = rv_auipc_jalr_target(insn0, insn1, auipc_addr);
        if !target_addrs.contains(&target) {
            continue;
        }
        if let Some(length) = rv_resolve_a2_constant(instructions, i) {
            if length <= MAX_MEMCPY_LENGTH {
                calls_by_length.entry(length).or_default().push(i);
            }
        }
    }
    calls_by_length
}

/// Generate specialized routines, append to instruction stream, patch call sites,
/// and register synthetic symbol names.
pub(crate) fn generate_and_patch(
    instructions: &mut Vec<u32>,
    symbols: &mut SymbolTable,
    pc_base: u32,
    calls_by_length: &BTreeMap<u32, Vec<usize>>,
    generate_routine: fn(u32) -> Vec<u32>,
    label: &str,
) {
    let mut routine_addrs: BTreeMap<u32, u32> = BTreeMap::new();
    let original_len = instructions.len();

    for &length in calls_by_length.keys() {
        let routine = generate_routine(length);
        let routine_start_idx = instructions.len();
        let routine_start_addr = pc_base + (routine_start_idx as u32) * 4;

        println!(
            "{label}: length={length} at addr 0x{routine_start_addr:x} ({} instructions)",
            routine.len()
        );

        symbols.insert(routine_start_addr, format!("{label}_len{length}"));
        instructions.extend(routine);
        routine_addrs.insert(length, routine_start_addr);
    }

    for (&length, auipc_indices) in calls_by_length {
        let target_addr = routine_addrs[&length];
        for &auipc_idx in auipc_indices {
            let auipc_addr = pc_base + (auipc_idx as u32) * 4;
            let rd = rv_rd(instructions[auipc_idx]);
            let (new_auipc, new_jalr) = rv_encode_call(auipc_addr, target_addr, rd);

            let verify_target = rv_auipc_jalr_target(new_auipc, new_jalr, auipc_addr);
            assert_eq!(
                verify_target, target_addr,
                "AUIPC+JALR encoding mismatch at 0x{auipc_addr:x}: got 0x{verify_target:x}, expected 0x{target_addr:x}"
            );

            instructions[auipc_idx] = new_auipc;
            instructions[auipc_idx + 1] = new_jalr;
        }
    }

    println!(
        "{label}: patched {} call sites across {} lengths, {original_len} -> {} instructions",
        calls_by_length.values().map(|v| v.len()).sum::<usize>(),
        calls_by_length.len(),
        instructions.len()
    );
}

/// Find ALL symbol addresses matching a substring on the raw or demangled name.
/// Returns vec of (address, joined name string).
pub(crate) fn find_symbols(symbols: &SymbolTable, needle: &str) -> Vec<(u32, String)> {
    symbols
        .table()
        .iter()
        .filter(|(_, names): &(&u32, &Vec<String>)| {
            names
                .iter()
                .any(|n| n.contains(needle) || format!("{:#}", demangle(n)).contains(needle))
        })
        .map(|(&addr, names)| (addr, names.join(", ")))
        .collect()
}

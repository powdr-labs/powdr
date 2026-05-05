use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::process;

use powdr_riscv_elf::debug_info::SymbolTable;

use crate::disasm::disasm;

pub(crate) fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Resolve the jump/branch target address for an instruction.
pub(crate) fn resolve_target(instructions: &[u32], idx: usize, pc_base: u32) -> Option<u32> {
    let insn = instructions[idx];
    let addr = pc_base + (idx as u32) * 4;
    let opcode = insn & 0x7f;

    match opcode {
        0x6f => {
            // JAL
            let imm20 = ((insn >> 31) & 1) as i32;
            let imm10_1 = ((insn >> 21) & 0x3FF) as i32;
            let imm11 = ((insn >> 20) & 1) as i32;
            let imm19_12 = ((insn >> 12) & 0xFF) as i32;
            let offset = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
            let offset = (offset << 11) >> 11;
            Some((addr as i32).wrapping_add(offset) as u32)
        }
        0x63 => {
            // Branch
            let imm12 = ((insn >> 31) & 1) as i32;
            let imm10_5 = ((insn >> 25) & 0x3F) as i32;
            let imm4_1 = ((insn >> 8) & 0xF) as i32;
            let imm11 = ((insn >> 7) & 1) as i32;
            let offset = (imm12 << 12) | (imm11 << 11) | (imm10_5 << 5) | (imm4_1 << 1);
            let offset = (offset << 19) >> 19;
            Some((addr as i32).wrapping_add(offset) as u32)
        }
        0x17 => {
            // AUIPC — check if followed by JALR
            if idx + 1 < instructions.len() {
                let next = instructions[idx + 1];
                if next & 0x7f == 0x67 {
                    let auipc_rd = (insn >> 7) & 0x1f;
                    let jalr_rs1 = (next >> 15) & 0x1f;
                    if auipc_rd == jalr_rs1 {
                        let upper = (insn & 0xfffff000) as i32;
                        let lower = (next as i32) >> 20;
                        return Some((addr as i64 + upper as i64 + lower as i64) as u32);
                    }
                }
            }
            None
        }
        0x67 => {
            // JALR — check if preceded by a matching AUIPC
            if idx > 0 {
                let prev = instructions[idx - 1];
                if prev & 0x7f == 0x17 {
                    let auipc_rd = (prev >> 7) & 0x1f;
                    let jalr_rs1 = (insn >> 15) & 0x1f;
                    if auipc_rd == jalr_rs1 {
                        let prev_addr = pc_base + ((idx - 1) as u32) * 4;
                        let upper = (prev & 0xfffff000) as i32;
                        let lower = (insn as i32) >> 20;
                        return Some((prev_addr as i64 + upper as i64 + lower as i64) as u32);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Find the end of a routine (next symbol or `ret`, whichever comes first).
#[allow(clippy::needless_range_loop)]
pub(crate) fn routine_end_idx(
    instructions: &[u32],
    start_idx: usize,
    symbols: &SymbolTable,
    pc_base: u32,
) -> usize {
    let start_addr = pc_base + (start_idx as u32) * 4;
    let max_by_sym = symbols
        .table()
        .range((start_addr + 4)..)
        .next()
        .map(|(&a, _)| ((a.wrapping_sub(pc_base)) / 4) as usize)
        .unwrap_or(instructions.len())
        .min(instructions.len());
    let max_idx = max_by_sym.min(start_idx + 2000);

    for i in start_idx..max_idx {
        // ret = jalr zero, ra, 0
        if instructions[i] == 0x00008067 {
            return i + 1;
        }
    }
    max_idx
}

/// Look up the best symbol name for a given address. Returns `None` for
/// mapping symbols ($x, $d, etc.) or if there is no symbol at that address.
fn symbol_name_at(symbols: &SymbolTable, addr: u32) -> Option<String> {
    symbols.table().get(&addr).and_then(|names| {
        names
            .iter()
            .find(|n| !(n.starts_with('$') && n.len() <= 2))
            .map(|n| format!("{:#}", rustc_demangle::demangle(n)))
    })
}

/// Format the link and optional symbol annotation for a resolved jump target.
fn format_target_link(
    escaped_asm: &str,
    target: u32,
    prefix: &str,
    symbols: &SymbolTable,
) -> String {
    let ts = format!("0x{target:08x}");
    let sym_annotation = symbol_name_at(symbols, target)
        .map(|name| {
            format!(
                "  <a class=\"jt\" href=\"#{prefix}{target:08x}\">&lt;{}&gt;</a>",
                html_escape(&name)
            )
        })
        .unwrap_or_default();

    if escaped_asm.contains(&ts) {
        let linked = escaped_asm.replace(
            &ts,
            &format!("<a class=\"jt\" href=\"#{prefix}{target:08x}\">{ts}</a>"),
        );
        format!("{linked}{sym_annotation}")
    } else if !sym_annotation.is_empty() {
        // JALR without an explicit hex target in the asm text — just append annotation
        format!("{escaped_asm}{sym_annotation}")
    } else {
        escaped_asm.to_string()
    }
}

/// Disassemble a range of instructions into an HTML code block.
pub(crate) fn format_code_block(
    instructions: &[u32],
    start_idx: usize,
    end_idx: usize,
    pc_base: u32,
    symbols: &SymbolTable,
    prefix: &str,
) -> String {
    let mut out = String::new();
    let end = end_idx.min(instructions.len());
    for i in start_idx..end {
        let addr = pc_base + (i as u32) * 4;
        if let Some(names) = symbols.table().get(&addr) {
            for name in names {
                // Skip RISC-V/ARM mapping symbols ($x, $d, $t, etc.)
                if name.starts_with('$') && name.len() <= 2 {
                    continue;
                }
                let d = format!("{:#}", rustc_demangle::demangle(name));
                let _ = write!(out, "<span class=\"sym\">{}</span>", html_escape(&d));
            }
        }
        let insn = instructions[i];
        let asm = disasm(insn, addr);
        let escaped = html_escape(&asm);

        let target = resolve_target(instructions, i, pc_base);
        let with_links = if let Some(t) = target {
            format_target_link(&escaped, t, prefix, symbols)
        } else {
            escaped
        };

        let nop_class = if insn == 0x00000013 { " nop" } else { "" };
        let _ = write!(
            out,
            "<span id=\"{prefix}{addr:08x}\" class=\"cl{nop_class}\">\
             <span class=\"ad\">{addr:08x}</span>  \
             <span class=\"by\">{insn:08x}</span>  \
             {with_links}</span>"
        );
    }
    out
}

/// Like `format_code_block` but highlights lines that differ between `this` and `other`.
/// `is_original` controls whether differing lines get `diff-del` (original) or `diff-add` (optimized).
#[allow(clippy::too_many_arguments)]
fn format_code_block_diff(
    this: &[u32],
    other: &[u32],
    start_idx: usize,
    end_idx: usize,
    pc_base: u32,
    symbols: &SymbolTable,
    prefix: &str,
    is_original: bool,
) -> String {
    let mut out = String::new();
    let end = end_idx.min(this.len());
    for i in start_idx..end {
        let addr = pc_base + (i as u32) * 4;
        if let Some(names) = symbols.table().get(&addr) {
            for name in names {
                if name.starts_with('$') && name.len() <= 2 {
                    continue;
                }
                let d = format!("{:#}", rustc_demangle::demangle(name));
                let _ = write!(out, "<span class=\"sym\">{}</span>", html_escape(&d));
            }
        }
        let insn = this[i];
        let asm = disasm(insn, addr);
        let escaped = html_escape(&asm);

        let target = resolve_target(this, i, pc_base);
        let with_links = if let Some(t) = target {
            format_target_link(&escaped, t, prefix, symbols)
        } else {
            escaped
        };

        let other_insn = if i < other.len() { other[i] } else { 0 };
        let differs = insn != other_insn || i >= other.len();
        let diff_class = if differs {
            if is_original {
                " diff-del"
            } else {
                " diff-add"
            }
        } else {
            ""
        };
        let nop_class = if insn == 0x00000013 { " nop" } else { "" };
        let _ = write!(
            out,
            "<span id=\"{prefix}{addr:08x}\" class=\"cl{nop_class}{diff_class}\">\
             <span class=\"ad\">{addr:08x}</span>  \
             <span class=\"by\">{insn:08x}</span>  \
             {with_links}</span>"
        );
    }
    out
}

/// Generate an interactive HTML diff report comparing original vs optimized code.
#[allow(clippy::too_many_lines)]
pub(crate) fn generate_html_diff(
    original: &[u32],
    patched: &[u32],
    symbols: &SymbolTable,
    pc_base: u32,
    output_path: &str,
) {
    let original_end_addr = pc_base + (original.len() as u32) * 4;
    let patched_end_addr = pc_base + (patched.len() as u32) * 4;

    // ── Phase 1: Analyse differences ──────────────────────────────────────

    // Find contiguous diff regions (ranges where instructions differ).
    let mut diff_regions: Vec<(usize, usize)> = Vec::new();
    {
        let mut i = 0;
        let n = original.len().min(patched.len());
        while i < n {
            if original[i] != patched[i] {
                let start = i;
                while i < n && original[i] != patched[i] {
                    i += 1;
                }
                diff_regions.push((start, i));
            } else {
                i += 1;
            }
        }
    }

    // Classify each diff region.
    struct CallSiteRedirect {
        addr: u32,
        orig_target: Option<u32>,
        new_target: Option<u32>,
    }
    struct BytecopyPatchInfo {
        addr: u32,
        orig_start: usize,
        orig_end: usize,
        new_target: Option<u32>,
    }

    let mut call_redirects: Vec<CallSiteRedirect> = Vec::new();
    let mut bc_patches: Vec<BytecopyPatchInfo> = Vec::new();

    for &(start, end) in &diff_regions {
        let addr = pc_base + (start as u32) * 4;
        let orig_target = resolve_target(original, start, pc_base);
        let new_target = resolve_target(patched, start, pc_base);
        if end - start <= 2 {
            call_redirects.push(CallSiteRedirect {
                addr,
                orig_target,
                new_target,
            });
        } else {
            bc_patches.push(BytecopyPatchInfo {
                addr,
                orig_start: start,
                orig_end: end,
                new_target,
            });
        }
    }

    // Group call-site redirects by new target.
    let mut redirect_groups: BTreeMap<u32, Vec<usize>> = BTreeMap::new();
    for (ci, cr) in call_redirects.iter().enumerate() {
        if let Some(target) = cr.new_target {
            redirect_groups.entry(target).or_default().push(ci);
        }
    }

    // Collect appended routines.
    struct RoutineInfo {
        addr: u32,
        name: String,
        start_idx: usize,
        end_idx: usize,
    }

    let mut appended: Vec<RoutineInfo> = Vec::new();
    for (&addr, names) in symbols.table().range(original_end_addr..patched_end_addr) {
        let idx = ((addr - pc_base) / 4) as usize;
        let end = routine_end_idx(patched, idx, symbols, pc_base);
        let name = names
            .first()
            .map(|n| format!("{:#}", rustc_demangle::demangle(n)))
            .unwrap_or_else(|| format!("routine_{addr:08x}"));
        appended.push(RoutineInfo {
            addr,
            name,
            start_idx: idx,
            end_idx: end,
        });
    }

    // Group appended routines by family.
    let mut families: BTreeMap<String, Vec<usize>> = BTreeMap::new();
    let mut bytecopy_routine_indices: Vec<usize> = Vec::new();

    for (i, r) in appended.iter().enumerate() {
        if r.name.starts_with("memcpy_aligned") {
            families
                .entry("memcpy (aligned)".into())
                .or_default()
                .push(i);
        } else if r.name.starts_with("memcpy_opt") {
            families.entry("memcpy".into()).or_default().push(i);
        } else if r.name.starts_with("memmove_aligned") {
            families
                .entry("memmove (aligned)".into())
                .or_default()
                .push(i);
        } else if r.name.starts_with("memmove_opt") {
            families.entry("memmove".into()).or_default().push(i);
        } else if r.name.starts_with("memcmp_aligned") {
            families
                .entry("memcmp (aligned)".into())
                .or_default()
                .push(i);
        } else if r.name.starts_with("memcmp_opt") {
            families.entry("memcmp".into()).or_default().push(i);
        } else if r.name.starts_with("bytecopy_dfg") || r.name.starts_with("bytecopy_opt") {
            bytecopy_routine_indices.push(i);
        }
    }

    // For each family, find the original function address (where call sites
    // used to point before patching).
    let mut family_orig: BTreeMap<String, u32> = BTreeMap::new();
    for (family, indices) in &families {
        'outer: for &ri in indices {
            if let Some(sites) = redirect_groups.get(&appended[ri].addr) {
                for &ci in sites {
                    if let Some(t) = call_redirects[ci].orig_target {
                        family_orig.insert(family.clone(), t);
                        break 'outer;
                    }
                }
            }
        }
    }

    // ── Phase 2: Generate HTML ────────────────────────────────────────────

    let mut h = String::with_capacity(4 * 1024 * 1024);

    // ── Header, CSS, JS ───────────────────────────────────────────────────
    h.push_str(
        r##"<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<title>ELF Optimizer Diff</title>
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{background:#0d1117;color:#c9d1d9;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;line-height:1.5}
.ctr{max-width:1800px;margin:0 auto;padding:20px}
header{background:#161b22;padding:20px 24px;border:1px solid #30363d;border-radius:6px;margin-bottom:16px}
h1{font-size:20px;margin-bottom:12px;color:#f0f6fc}
.stats{display:flex;gap:16px;flex-wrap:wrap}
.st{background:#21262d;padding:8px 16px;border-radius:6px;border:1px solid #30363d}
.st b{color:#58a6ff;font-size:18px}.st small{color:#8b949e;display:block;font-size:11px}
nav{position:sticky;top:0;background:#0d1117ee;backdrop-filter:blur(8px);border-bottom:1px solid #30363d;padding:8px 0;margin-bottom:16px;z-index:100;display:flex;gap:4px;flex-wrap:wrap}
nav a{color:#8b949e;text-decoration:none;padding:4px 12px;border-radius:20px;font-size:13px;border:1px solid #30363d}
nav a:hover{color:#c9d1d9;background:#21262d}
h2{font-size:16px;color:#f0f6fc;margin:24px 0 12px;padding-bottom:8px;border-bottom:1px solid #21262d}
.sb{background:#161b22;border:1px solid #30363d;border-radius:6px;margin-bottom:8px;overflow:hidden}
.sh{display:flex;justify-content:space-between;align-items:center;padding:8px 16px;background:#21262d;cursor:pointer;user-select:none}
.sh:hover{background:#292e36}
.sh h3{font-size:13px;color:#c9d1d9;font-weight:500}
.badge{background:#388bfd26;color:#58a6ff;padding:2px 8px;border-radius:10px;font-size:11px;white-space:nowrap}
.ct{display:none}.ct.open{display:block}
.ss{display:flex}
.pn{flex:1;min-width:0;overflow:auto;max-height:85vh}
.pn+.pn{border-left:1px solid #30363d}
.ph{padding:4px 16px;font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:.5px;position:sticky;top:0;z-index:1}
.po .ph{color:#f85149;background:rgba(248,81,73,.1)}
.px .ph{color:#3fb950;background:rgba(63,185,80,.1)}
.code{font-family:'SFMono-Regular',Consolas,'Liberation Mono',Menlo,monospace;font-size:12px;line-height:1.25;padding:4px 0;white-space:pre;overflow-x:auto}
.cl{padding:0 16px;display:block}.cl:hover{background:rgba(136,198,255,.06)}
.ad{color:#484f58;user-select:none}.by{color:#484f58;user-select:none}
.sym{color:#ffa657;font-weight:600;display:block;padding:0 16px;margin-top:4px}
a.jt{color:#58a6ff;text-decoration:none}a.jt:hover{text-decoration:underline}
.nop{opacity:.25}
.tb{display:flex;flex-wrap:wrap;gap:3px;padding:6px 16px;background:#161b22;border-bottom:1px solid #30363d}
.tab{background:0 0;color:#8b949e;border:1px solid transparent;border-radius:4px;padding:2px 8px;font-size:11px;cursor:pointer;font-family:inherit}
.tab:hover{color:#c9d1d9;background:#292e36}
.tab.on{color:#58a6ff;border-color:#58a6ff;background:#388bfd15}
.tc{display:none}.tc.on{display:block}
.fn-blk{border-bottom:1px solid #21262d}
.fn-hdr{padding:2px 16px;font-size:11px;cursor:pointer;user-select:none;display:flex;justify-content:space-between;align-items:center}
.fn-hdr:hover{background:#292e36}
.fn-hdr .fn-name{color:#ffa657;font-weight:600}
.fn-hdr .fn-badge{font-size:10px;color:#8b949e}
.fn-changed .fn-hdr{background:rgba(56,139,253,.08)}
.fn-changed .fn-hdr .fn-badge{color:#58a6ff}
.fn-body{display:none}
.fn-blk.open .fn-body{display:block}
.diff-add{background:rgba(63,185,80,.15)}
.diff-del{background:rgba(248,81,73,.15)}
.diff-toolbar{display:flex;gap:8px;padding:8px 16px;background:#21262d;border:1px solid #30363d;border-radius:6px;margin-bottom:8px;align-items:center;flex-wrap:wrap;position:sticky;top:32px;z-index:50}
.diff-toolbar button{background:#30363d;color:#c9d1d9;border:1px solid #484f58;border-radius:4px;padding:4px 12px;font-size:12px;cursor:pointer;font-family:inherit}
.diff-toolbar button:hover{background:#484f58}
.diff-toolbar .info{color:#8b949e;font-size:11px;margin-left:auto}
.diff-grid{display:grid;grid-template-columns:1fr 1fr;border:1px solid #30363d;border-radius:6px;overflow:hidden;background:#161b22}
.diff-col-hdr{padding:4px 16px;font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:.5px;position:sticky;top:64px;z-index:10}
.diff-col-hdr.do{color:#f85149;background:rgba(248,81,73,.15);border-bottom:1px solid #30363d}
.diff-col-hdr.dn{color:#3fb950;background:rgba(63,185,80,.15);border-bottom:1px solid #30363d;border-left:1px solid #30363d}
.diff-row{display:contents}
.diff-cell{min-width:0;overflow-x:auto;border-bottom:1px solid #21262d}
.diff-cell+.diff-cell{border-left:1px solid #30363d}
table{width:100%;border-collapse:collapse;font-size:12px;font-family:'SFMono-Regular',Consolas,monospace}
th{text-align:left;padding:6px 12px;background:#21262d;border-bottom:1px solid #30363d;color:#8b949e;font-size:11px;font-weight:600}
td{padding:4px 12px;border-bottom:1px solid #21262d}
tr:hover td{background:rgba(136,198,255,.04)}
td a{color:#58a6ff;text-decoration:none}td a:hover{text-decoration:underline}
</style>
<script>
function toggle(id){var e=document.getElementById(id);if(e)e.classList.toggle('open')}
function selectTab(g,t){
 document.querySelectorAll('.tc[data-g="'+g+'"]').forEach(function(e){e.classList.remove('on')});
 document.querySelectorAll('.tab[data-g="'+g+'"]').forEach(function(e){e.classList.remove('on')});
 var el=document.getElementById(t);if(el)el.classList.add('on');
 var b=document.querySelector('.tab[data-t="'+t+'"]');if(b)b.classList.add('on');
}
function toggleFn(id){var e=document.getElementById(id);if(e)e.classList.toggle('open')}
function setUnchanged(show){
 document.querySelectorAll('.fn-blk:not(.fn-changed)').forEach(function(e){
  if(show)e.classList.add('open');else e.classList.remove('open');
 });
}
function expandAll(){
 document.querySelectorAll('.fn-blk').forEach(function(e){e.classList.add('open')});
}
function collapseAll(){
 document.querySelectorAll('.fn-blk').forEach(function(e){e.classList.remove('open')});
}
</script>
</head><body>
"##,
    );

    // ── Summary ───────────────────────────────────────────────────────────
    let _ = write!(h, "<div class=\"ctr\">");
    let _ = write!(
        h,
        "<header><h1>ELF Optimizer — Diff Report</h1><div class=\"stats\">"
    );
    let _ = write!(
        h,
        "<div class=\"st\"><b>{}</b><small>original insns</small></div>",
        original.len()
    );
    let _ = write!(
        h,
        "<div class=\"st\"><b>{}</b><small>optimized insns</small></div>",
        patched.len()
    );
    let _ = write!(
        h,
        "<div class=\"st\"><b>+{}</b><small>appended insns</small></div>",
        patched.len() - original.len()
    );
    let _ = write!(
        h,
        "<div class=\"st\"><b>{}</b><small>call sites patched</small></div>",
        call_redirects.len()
    );
    let _ = write!(
        h,
        "<div class=\"st\"><b>{}</b><small>bytecopy patches</small></div>",
        bc_patches.len()
    );
    let _ = write!(
        h,
        "<div class=\"st\"><b>{}</b><small>new routines</small></div>",
        appended.len()
    );
    let _ = write!(h, "</div></header>");

    // ── Navigation ────────────────────────────────────────────────────────
    let _ = write!(h, "<nav>");
    for (family, indices) in &families {
        let _ = write!(
            h,
            "<a href=\"#fam-{family}\">{family} ({})</a>",
            indices.len()
        );
    }
    if !bc_patches.is_empty() {
        let _ = write!(
            h,
            "<a href=\"#bytecopy\">bytecopy ({})</a>",
            bc_patches.len()
        );
    }
    let _ = write!(h, "<a href=\"#callsites\">call sites</a>");
    let _ = write!(h, "<a href=\"#statistics\">statistics</a>");
    let _ = write!(h, "<a href=\"#full-diff\">full disassembly diff</a>");
    let _ = write!(h, "</nav>");

    // ── Routine family comparisons ────────────────────────────────────────
    for (family, indices) in &families {
        let total_sites: usize = indices
            .iter()
            .map(|&ri| {
                redirect_groups
                    .get(&appended[ri].addr)
                    .map(|s| s.len())
                    .unwrap_or(0)
            })
            .sum();
        let _ = write!(
            h,
            "<section id=\"fam-{family}\"><h2>{family} — {} specialized variant{}, \
             {total_sites} call site{} patched</h2>",
            indices.len(),
            if indices.len() == 1 { "" } else { "s" },
            if total_sites == 1 { "" } else { "s" },
        );

        // Side-by-side container
        let _ = write!(h, "<div class=\"sb\"><div class=\"ss\">");

        // Left panel: original function
        let _ = write!(h, "<div class=\"pn po\">");
        let _ = write!(h, "<div class=\"ph\">Original</div>");
        if let Some(&orig_addr) = family_orig.get(family.as_str()) {
            let orig_idx = ((orig_addr.wrapping_sub(pc_base)) / 4) as usize;
            if orig_idx < original.len() {
                let orig_end = routine_end_idx(original, orig_idx, symbols, pc_base);
                let _ = write!(h, "<div class=\"code\">");
                h.push_str(&format_code_block(
                    original,
                    orig_idx,
                    orig_end,
                    pc_base,
                    symbols,
                    &format!("o{family}_"),
                ));
                let _ = write!(h, "</div>");
            }
        } else {
            let _ = write!(
                h,
                "<div class=\"code\"><span class=\"cl\">\
                 (original function not found)</span></div>"
            );
        }
        let _ = write!(h, "</div>");

        // Right panel: tabs for each specialized variant
        let _ = write!(h, "<div class=\"pn px\">");
        let _ = write!(h, "<div class=\"ph\">Specialized</div>");

        // Tab bar
        let _ = write!(h, "<div class=\"tb\">");
        for (ti, &ri) in indices.iter().enumerate() {
            let r = &appended[ri];
            let n_sites = redirect_groups.get(&r.addr).map(|s| s.len()).unwrap_or(0);
            let label = r
                .name
                .strip_prefix(&format!("{family}_opt_"))
                .or_else(|| r.name.strip_prefix(&format!("{family}_opt")))
                .unwrap_or(&r.name);
            let active = if ti == 0 { " on" } else { "" };
            let _ = write!(
                h,
                "<button class=\"tab{active}\" data-g=\"{family}\" \
                 data-t=\"t_{family}_{ti}\" \
                 onclick=\"selectTab('{family}','t_{family}_{ti}')\">\
                 {label} <small>({n_sites})</small></button>"
            );
        }
        let _ = write!(h, "</div>");

        // Tab content panes
        for (ti, &ri) in indices.iter().enumerate() {
            let r = &appended[ri];
            let active = if ti == 0 { " on" } else { "" };
            let _ = write!(
                h,
                "<div id=\"t_{family}_{ti}\" class=\"tc{active}\" data-g=\"{family}\">\
                 <div class=\"code\">"
            );
            h.push_str(&format_code_block(
                patched,
                r.start_idx,
                r.end_idx,
                pc_base,
                symbols,
                &format!("s{family}{ti}_"),
            ));
            let _ = write!(h, "</div></div>");
        }

        let _ = write!(h, "</div>"); // pn px
        let _ = write!(h, "</div></div>"); // ss, sb
        let _ = write!(h, "</section>");
    }

    // ── Bytecopy patches ──────────────────────────────────────────────────
    if !bc_patches.is_empty() {
        let _ = write!(
            h,
            "<section id=\"bytecopy\"><h2>Bytecopy Optimizations ({} patches)</h2>",
            bc_patches.len()
        );

        for (i, patch) in bc_patches.iter().enumerate() {
            let n_orig = patch.orig_end - patch.orig_start;
            let _ = write!(h, "<div class=\"sb\">");
            let _ = write!(
                h,
                "<div class=\"sh\" onclick=\"toggle('bc{i}')\">\
                 <h3>▸ Patch at 0x{:08x} ({n_orig} insns → optimized)</h3></div>",
                patch.addr
            );
            let _ = write!(h, "<div id=\"bc{i}\" class=\"ct\"><div class=\"ss\">");

            // Left: original inline code
            let _ = write!(
                h,
                "<div class=\"pn po\"><div class=\"ph\">Original ({n_orig} insns)</div>\
                 <div class=\"code\">"
            );
            h.push_str(&format_code_block(
                original,
                patch.orig_start,
                patch.orig_end,
                pc_base,
                symbols,
                &format!("bo{i}_"),
            ));
            let _ = write!(h, "</div></div>");

            // Right: optimized routine
            if let Some(target) = patch.new_target {
                if let Some(&bri) = bytecopy_routine_indices
                    .iter()
                    .find(|&&ri| appended[ri].addr == target)
                {
                    let r = &appended[bri];
                    let n_opt = r.end_idx - r.start_idx;
                    let _ = write!(
                        h,
                        "<div class=\"pn px\">\
                         <div class=\"ph\">Optimized ({n_opt} insns)</div>\
                         <div class=\"code\">"
                    );
                    h.push_str(&format_code_block(
                        patched,
                        r.start_idx,
                        r.end_idx,
                        pc_base,
                        symbols,
                        &format!("bn{i}_"),
                    ));
                    let _ = write!(h, "</div></div>");
                }
            }

            let _ = write!(h, "</div></div></div>"); // ss, ct, sb
        }

        let _ = write!(h, "</section>");
    }

    // ── Call-site summary table ───────────────────────────────────────────
    {
        let total = call_redirects.len() + bc_patches.len();
        let _ = write!(
            h,
            "<section id=\"callsites\">\
             <h2>Patched Call Sites ({total} total)</h2>"
        );

        // Grouped summary
        let _ = write!(
            h,
            "<div class=\"sb\"><table><thead><tr>\
             <th>Routine</th><th>Call Sites</th><th>Insns</th>\
             </tr></thead><tbody>"
        );
        for (&target, sites) in &redirect_groups {
            let r = appended.iter().find(|r| r.addr == target);
            let rname = r.map(|r| r.name.as_str()).unwrap_or("unknown");
            let insns = r.map(|r| r.end_idx - r.start_idx).unwrap_or(0);
            let _ = write!(
                h,
                "<tr><td>{}</td><td>{}</td><td>{insns}</td></tr>",
                html_escape(rname),
                sites.len()
            );
        }
        for (i, patch) in bc_patches.iter().enumerate() {
            let rname = patch
                .new_target
                .and_then(|t| appended.iter().find(|r| r.addr == t))
                .map(|r| r.name.as_str())
                .unwrap_or("bytecopy");
            let _ = write!(
                h,
                "<tr><td><a class=\"jt\" href=\"#bc{i}\">{}</a></td>\
                 <td>1</td><td>{}</td></tr>",
                html_escape(rname),
                patch.orig_end - patch.orig_start
            );
        }
        let _ = write!(h, "</tbody></table></div>");

        // Expandable full list
        let _ = write!(h, "<div class=\"sb\">");
        let _ = write!(
            h,
            "<div class=\"sh\" onclick=\"toggle('cslist')\">\
             <h3>▸ Full call-site list ({total} entries)</h3></div>"
        );
        let _ = write!(
            h,
            "<div id=\"cslist\" class=\"ct\"><table><thead><tr>\
             <th>Address</th><th>Original Target</th>\
             <th>New Target</th><th>Routine</th>\
             </tr></thead><tbody>"
        );
        for (&target, sites) in &redirect_groups {
            let rname = appended
                .iter()
                .find(|r| r.addr == target)
                .map(|r| r.name.as_str())
                .unwrap_or("unknown");
            for &ci in sites {
                let site = &call_redirects[ci];
                let orig_str = site
                    .orig_target
                    .map(|t| format!("0x{t:08x}"))
                    .unwrap_or_else(|| "–".into());
                let _ = write!(
                    h,
                    "<tr><td>0x{:08x}</td><td>{orig_str}</td>\
                     <td>0x{target:08x}</td><td>{}</td></tr>",
                    site.addr,
                    html_escape(rname)
                );
            }
        }
        for (i, patch) in bc_patches.iter().enumerate() {
            let tstr = patch
                .new_target
                .map(|t| format!("0x{t:08x}"))
                .unwrap_or_else(|| "–".into());
            let name = patch
                .new_target
                .and_then(|t| appended.iter().find(|r| r.addr == t))
                .map(|r| r.name.as_str())
                .unwrap_or("bytecopy");
            let _ = write!(
                h,
                "<tr><td><a class=\"jt\" href=\"#bc{i}\">0x{:08x}</a></td>\
                 <td>(inline)</td><td>{tstr}</td><td>{}</td></tr>",
                patch.addr,
                html_escape(name)
            );
        }
        let _ = write!(h, "</tbody></table></div></div>");
        let _ = write!(h, "</section>");
    }

    // ── Statistics section ─────────────────────────────────────────────────
    {
        let _ = write!(
            h,
            "<section id=\"statistics\">\
             <h2>Optimization Statistics</h2>"
        );

        // Compute per-family stats
        struct FamilyStats {
            routines: usize,
            call_sites: usize,
            total_routine_insns: usize,
            lengths: Vec<(String, usize)>, // (routine_name, call_count)
        }

        let mut fam_stats: BTreeMap<String, FamilyStats> = BTreeMap::new();
        for (family, indices) in &families {
            let mut fs = FamilyStats {
                routines: indices.len(),
                call_sites: 0,
                total_routine_insns: 0,
                lengths: Vec::new(),
            };
            for &ri in indices {
                let r = &appended[ri];
                let insns = r.end_idx - r.start_idx;
                let sites = redirect_groups
                    .get(&r.addr)
                    .map(|s| s.len())
                    .unwrap_or(0);
                fs.call_sites += sites;
                fs.total_routine_insns += insns;
                fs.lengths.push((r.name.clone(), sites));
            }
            fam_stats.insert(family.clone(), fs);
        }

        // Summary cards
        let _ = write!(h, "<div class=\"stats\" style=\"margin-bottom:16px\">");
        let total_redirected: usize = fam_stats.values().map(|f| f.call_sites).sum();
        let total_routines: usize = fam_stats.values().map(|f| f.routines).sum();
        let total_routine_insns: usize = fam_stats.values().map(|f| f.total_routine_insns).sum();
        let _ = write!(
            h,
            "<div class=\"st\"><b>{total_redirected}</b><small>call sites redirected</small></div>"
        );
        let _ = write!(
            h,
            "<div class=\"st\"><b>{}</b><small>bytecopy patches</small></div>",
            bc_patches.len()
        );
        let _ = write!(
            h,
            "<div class=\"st\"><b>{total_routines}</b><small>specialized routines</small></div>"
        );
        let _ = write!(
            h,
            "<div class=\"st\"><b>{total_routine_insns}</b><small>routine insns total</small></div>"
        );
        let saved_insns = patched.len() as i64 - original.len() as i64;
        let _ = write!(
            h,
            "<div class=\"st\"><b>{:+}</b><small>instruction delta</small></div>",
            saved_insns
        );
        let _ = write!(h, "</div>");

        // Per-family breakdown table
        let _ = write!(
            h,
            "<div class=\"sb\"><table><thead><tr>\
             <th>Family</th><th>Routines</th><th>Call Sites</th>\
             <th>Total Routine Insns</th><th>Avg Routine Size</th>\
             </tr></thead><tbody>"
        );
        for (family, fs) in &fam_stats {
            let avg = if fs.routines > 0 {
                fs.total_routine_insns / fs.routines
            } else {
                0
            };
            let _ = write!(
                h,
                "<tr><td>{family}</td><td>{}</td><td>{}</td><td>{}</td><td>{avg}</td></tr>",
                fs.routines, fs.call_sites, fs.total_routine_insns
            );
        }
        if !bc_patches.is_empty() {
            let bc_total_insns: usize = bytecopy_routine_indices
                .iter()
                .map(|&ri| appended[ri].end_idx - appended[ri].start_idx)
                .sum();
            let avg = if !bytecopy_routine_indices.is_empty() {
                bc_total_insns / bytecopy_routine_indices.len()
            } else {
                0
            };
            let _ = write!(
                h,
                "<tr><td>bytecopy (inline)</td><td>{}</td><td>{}</td><td>{bc_total_insns}</td><td>{avg}</td></tr>",
                bytecopy_routine_indices.len(),
                bc_patches.len()
            );
        }
        let _ = write!(h, "</tbody></table></div>");

        // Per-routine detail table (expandable)
        let _ = write!(h, "<div class=\"sb\">");
        let _ = write!(
            h,
            "<div class=\"sh\" onclick=\"toggle('stat-detail')\">\
             <h3>▸ Per-Routine Detail ({} routines)</h3></div>",
            appended.len()
        );
        let _ = write!(
            h,
            "<div id=\"stat-detail\" class=\"ct\"><table><thead><tr>\
             <th>Routine</th><th>Family</th><th>Address</th>\
             <th>Insns</th><th>Call Sites</th>\
             </tr></thead><tbody>"
        );

        for (i, r) in appended.iter().enumerate() {
            let insns = r.end_idx - r.start_idx;
            let sites = redirect_groups
                .get(&r.addr)
                .map(|s| s.len())
                .unwrap_or(0);
            // Determine family
            let family = if r.name.starts_with("memcpy_aligned") {
                "memcpy (aligned)"
            } else if r.name.starts_with("memcpy_opt") {
                "memcpy"
            } else if r.name.starts_with("memmove_aligned") {
                "memmove (aligned)"
            } else if r.name.starts_with("memmove_opt") {
                "memmove"
            } else if r.name.starts_with("memcmp_aligned") {
                "memcmp (aligned)"
            } else if r.name.starts_with("memcmp_opt") {
                "memcmp"
            } else if r.name.starts_with("bytecopy_dfg") || r.name.starts_with("bytecopy_opt") {
                "bytecopy"
            } else {
                "other"
            };
            let _ = write!(
                h,
                "<tr><td>{}</td><td>{family}</td><td>0x{:08x}</td>\
                 <td>{insns}</td><td>{sites}</td></tr>",
                html_escape(&r.name),
                r.addr
            );
            let _ = i; // suppress unused
        }
        let _ = write!(h, "</tbody></table></div></div>");

        // Aligned vs unaligned comparison (if applicable)
        let has_aligned = fam_stats.keys().any(|k| k.contains("aligned"));
        if has_aligned {
            let _ = write!(
                h,
                "<h3 style=\"margin:16px 0 8px;color:#f0f6fc\">Alignment Analysis</h3>"
            );
            let _ = write!(
                h,
                "<div class=\"sb\"><table><thead><tr>\
                 <th>Function</th><th>Aligned Call Sites</th><th>Aligned Avg Insns</th>\
                 <th>Unaligned Call Sites</th><th>Unaligned Avg Insns</th>\
                 <th>Insn Savings (aligned)</th>\
                 </tr></thead><tbody>"
            );

            // Match aligned families with their unaligned counterparts
            let base_funcs = ["memcpy", "memmove", "memcmp"];
            for base in &base_funcs {
                let aligned_key = format!("{base} (aligned)");
                let unaligned_key = base.to_string();
                let a = fam_stats.get(&aligned_key);
                let u = fam_stats.get(&unaligned_key);
                if a.is_none() && u.is_none() {
                    continue;
                }
                let (a_sites, a_avg) = a
                    .map(|f| {
                        let avg = if f.routines > 0 {
                            f.total_routine_insns / f.routines
                        } else {
                            0
                        };
                        (f.call_sites, avg)
                    })
                    .unwrap_or((0, 0));
                let (u_sites, u_avg) = u
                    .map(|f| {
                        let avg = if f.routines > 0 {
                            f.total_routine_insns / f.routines
                        } else {
                            0
                        };
                        (f.call_sites, avg)
                    })
                    .unwrap_or((0, 0));
                let savings = if u_avg > a_avg {
                    format!(
                        "-{}%",
                        ((u_avg - a_avg) as f64 / u_avg as f64 * 100.0) as u32
                    )
                } else if a_avg > 0 {
                    "0%".to_string()
                } else {
                    "–".to_string()
                };
                let _ = write!(
                    h,
                    "<tr><td>{base}</td><td>{a_sites}</td><td>{a_avg}</td>\
                     <td>{u_sites}</td><td>{u_avg}</td><td>{savings}</td></tr>",
                );
            }
            let _ = write!(h, "</tbody></table></div>");
        }

        let _ = write!(h, "</section>");
    }

    // ── Full disassembly — side-by-side diff ────────────────────────────
    {
        // Build list of function boundaries from symbols.
        struct FnBlock {
            name: String,
            start_idx: usize,
            end_idx: usize,
            has_changes: bool,
        }

        // Collect all symbol addresses in the patched range as function starts.
        let all_sym_addrs: Vec<u32> = symbols
            .table()
            .range(pc_base..patched_end_addr)
            .filter_map(|(&addr, names)| {
                // Skip mapping symbols
                let dominated_by_mapping = names.iter().all(|n| n.starts_with('$') && n.len() <= 2);
                if dominated_by_mapping {
                    None
                } else {
                    Some(addr)
                }
            })
            .collect();

        let mut fn_blocks: Vec<FnBlock> = Vec::new();
        for (si, &sym_addr) in all_sym_addrs.iter().enumerate() {
            let start_idx = ((sym_addr - pc_base) / 4) as usize;
            if start_idx >= patched.len() {
                continue;
            }
            // End is next symbol or end of patched
            let end_idx = if si + 1 < all_sym_addrs.len() {
                (((all_sym_addrs[si + 1] - pc_base) / 4) as usize).min(patched.len())
            } else {
                patched.len()
            };

            let names = symbols.table().get(&sym_addr).unwrap();
            let name = names
                .iter()
                .find(|n| !(n.starts_with('$') && n.len() <= 2))
                .map(|n| format!("{:#}", rustc_demangle::demangle(n)))
                .unwrap_or_else(|| format!("fn_{sym_addr:08x}"));

            // Check if any instruction differs
            let has_changes = (start_idx..end_idx).any(|i| {
                let o = if i < original.len() {
                    original[i]
                } else {
                    0xDEAD_BEEF
                };
                let p = patched[i];
                o != p
            }) || start_idx >= original.len(); // appended = changed

            fn_blocks.push(FnBlock {
                name,
                start_idx,
                end_idx,
                has_changes,
            });
        }

        // Also include instructions before first symbol (if any)
        if fn_blocks.is_empty() || fn_blocks[0].start_idx > 0 {
            let end = if fn_blocks.is_empty() {
                patched.len()
            } else {
                fn_blocks[0].start_idx
            };
            let has_changes = (0..end.min(original.len())).any(|i| original[i] != patched[i]);
            fn_blocks.insert(
                0,
                FnBlock {
                    name: "(preamble)".to_string(),
                    start_idx: 0,
                    end_idx: end,
                    has_changes,
                },
            );
        }

        let changed_count = fn_blocks.iter().filter(|b| b.has_changes).count();
        let total_count = fn_blocks.len();

        let _ = write!(
            h,
            "<section id=\"full-diff\">\
             <h2>Full Disassembly — Side-by-Side Diff</h2>"
        );

        // Toolbar
        let _ = write!(
            h,
            "<div class=\"diff-toolbar\">\
             <button onclick=\"expandAll()\">Expand All</button>\
             <button onclick=\"collapseAll()\">Collapse All</button>\
             <button onclick=\"setUnchanged(false)\">Hide Unchanged</button>\
             <button onclick=\"setUnchanged(true)\">Show Unchanged</button>\
             <span class=\"info\">{changed_count} changed / {total_count} total functions</span>\
             </div>"
        );

        let _ = write!(h, "<div class=\"diff-grid\">");

        // Column headers
        let _ = write!(
            h,
            "<div class=\"diff-col-hdr do\">Original</div>\
             <div class=\"diff-col-hdr dn\">Optimized</div>"
        );

        // Each function is a row with two cells (original | optimized)
        for (fi, blk) in fn_blocks.iter().enumerate() {
            let changed_class = if blk.has_changes { " fn-changed" } else { "" };
            let initially_open = if blk.has_changes { " open" } else { "" };
            let insn_count = blk.end_idx - blk.start_idx;
            let change_label = if blk.has_changes { " ●" } else { "" };
            let escaped_name = html_escape(&blk.name);

            // Function header spans both columns
            let _ = write!(
                h,
                "<div id=\"fblk{fi}\" class=\"fn-blk{changed_class}{initially_open}\" \
                 style=\"grid-column:1/-1\">\
                 <div class=\"fn-hdr\" onclick=\"toggleFn('fblk{fi}')\">\
                 <span class=\"fn-name\">{escaped_name}{change_label}</span>\
                 <span class=\"fn-badge\">{insn_count} insns</span></div>\
                 <div class=\"fn-body\"><div style=\"display:grid;grid-template-columns:1fr 1fr\">"
            );

            // Left cell (original)
            let _ = write!(h, "<div class=\"diff-cell\"><div class=\"code\">");
            let orig_end = blk.end_idx.min(original.len());
            if blk.start_idx < orig_end {
                h.push_str(&format_code_block_diff(
                    original,
                    patched,
                    blk.start_idx,
                    orig_end,
                    pc_base,
                    symbols,
                    "dlo_",
                    true,
                ));
            }
            let _ = write!(h, "</div></div>");

            // Right cell (optimized)
            let _ = write!(h, "<div class=\"diff-cell\"><div class=\"code\">");
            h.push_str(&format_code_block_diff(
                patched,
                original,
                blk.start_idx,
                blk.end_idx,
                pc_base,
                symbols,
                "dro_",
                false,
            ));
            let _ = write!(h, "</div></div>");

            let _ = write!(h, "</div></div></div>"); // inner grid, fn-body, fn-blk
        }

        let _ = write!(h, "</div>"); // diff-grid
        let _ = write!(h, "</section>");
    }

    let _ = write!(h, "</div></body></html>");

    fs::write(output_path, &h).unwrap_or_else(|e| {
        eprintln!("Error writing {output_path}: {e}");
        process::exit(1);
    });
}

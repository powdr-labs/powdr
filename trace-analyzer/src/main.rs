use std::collections::{BTreeMap, HashMap};
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::{env, fs, process};

use goblin::elf::sym::STT_FUNC;
use goblin::elf::Elf;
use raki::decode::Decode;
use raki::instruction::Instruction;
use raki::Isa;
use rustc_demangle::demangle;

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

struct Function {
    name: String,
    addr: u32,
    size: u32,
}

struct FunctionStats {
    name: String,
    addr: u32,
    size: u32,
    call_count: u64,
    insn_count: u64,
    pc_hits: BTreeMap<u32, u64>,
}

struct FlameFrame {
    name: String,
    depth: usize,
    start: usize,
    end: usize,
}

struct BasicBlock {
    func_idx: usize,
    start_pc: u32,
    end_pc: u32,
    hit_count: u64,
}

struct TraceAnalysis {
    pc_counts: HashMap<u32, u64>,
    call_counts: HashMap<usize, u64>,
    /// caller_func_idx -> callee_func_idx -> count
    caller_edges: HashMap<usize, HashMap<usize, u64>>,
    flame_frames: Vec<FlameFrame>,
    trace_len: usize,
}

// ---------------------------------------------------------------------------
// ELF parsing
// ---------------------------------------------------------------------------

fn demangle_name(name: &str) -> String {
    format!("{:#}", demangle(name))
}

fn load_functions(elf: &Elf) -> Vec<Function> {
    let mut funcs: Vec<Function> = elf
        .syms
        .iter()
        .filter(|sym| sym.st_type() == STT_FUNC && sym.st_name != 0 && sym.st_size > 0)
        .map(|sym| Function {
            name: demangle_name(&elf.strtab[sym.st_name]),
            addr: sym.st_value as u32,
            size: sym.st_size as u32,
        })
        .collect();
    funcs.sort_by_key(|f| f.addr);
    funcs
}

fn lookup_function(funcs: &[Function], pc: u32) -> Option<usize> {
    let idx = funcs.partition_point(|f| f.addr <= pc);
    if idx == 0 {
        return None;
    }
    let f = &funcs[idx - 1];
    if pc < f.addr + f.size {
        Some(idx - 1)
    } else {
        None
    }
}

fn load_code<'a>(elf_bytes: &'a [u8], elf: &Elf, addr: u32, size: u32) -> Option<&'a [u8]> {
    for ph in &elf.program_headers {
        if ph.p_type == goblin::elf::program_header::PT_LOAD
            && (ph.p_flags & 1) != 0
            && addr >= ph.p_vaddr as u32
            && (addr as u64) < ph.p_vaddr + ph.p_memsz
        {
            let off = (ph.p_offset + (addr as u64 - ph.p_vaddr)) as usize;
            let end = (off + size as usize).min(elf_bytes.len());
            return Some(&elf_bytes[off..end]);
        }
    }
    None
}

// ---------------------------------------------------------------------------
// Streaming trace analysis
// ---------------------------------------------------------------------------

fn stream_analyze(path: &str, funcs: &[Function]) -> TraceAnalysis {
    let file = fs::File::open(path).unwrap_or_else(|e| {
        eprintln!("Failed to open trace file '{path}': {e}");
        process::exit(1);
    });
    let reader = BufReader::with_capacity(1 << 20, file);

    let mut pc_counts: HashMap<u32, u64> = HashMap::new();
    let mut call_counts: HashMap<usize, u64> = HashMap::new();
    let mut caller_edges: HashMap<usize, HashMap<usize, u64>> = HashMap::new();
    let mut flame_frames: Vec<FlameFrame> = Vec::new();
    let mut stack: Vec<(usize, usize)> = Vec::new();
    let mut prev_func_idx: Option<usize> = None;
    let mut row_idx: usize = 0;
    let mut first_line = true;

    for line_result in reader.lines() {
        let line = match line_result {
            Ok(l) => l,
            Err(_) => continue,
        };

        if first_line {
            first_line = false;
            if line.starts_with("segment") {
                continue;
            }
        }

        let bytes = line.as_bytes();
        let Some(c1) = memchr_byte(b',', bytes) else {
            continue;
        };
        let rest = &bytes[c1 + 1..];
        let Some(c2) = memchr_byte(b',', rest) else {
            continue;
        };
        let pc: u32 = parse_u32_fast(&rest[c2 + 1..]);

        *pc_counts.entry(pc).or_default() += 1;

        let func_idx = lookup_function(funcs, pc);

        if func_idx != prev_func_idx {
            if let Some(idx) = func_idx {
                *call_counts.entry(idx).or_default() += 1;
                if let Some(caller) = prev_func_idx {
                    *caller_edges
                        .entry(caller)
                        .or_default()
                        .entry(idx)
                        .or_default() += 1;
                }
            }

            if let Some(&(top_func, _)) = stack.last() {
                if func_idx != Some(top_func) {
                    let return_pos = stack.iter().rposition(|(f, _)| Some(*f) == func_idx);
                    if let Some(pos) = return_pos {
                        while stack.len() > pos + 1 {
                            let (f, start) = stack.pop().unwrap();
                            flame_frames.push(FlameFrame {
                                name: funcs[f].name.clone(),
                                depth: stack.len(),
                                start,
                                end: row_idx.saturating_sub(1),
                            });
                        }
                    } else if let Some(fi) = func_idx {
                        stack.push((fi, row_idx));
                    }
                }
            } else if let Some(fi) = func_idx {
                stack.push((fi, row_idx));
            }

            prev_func_idx = func_idx;
        }

        row_idx += 1;
        if row_idx % 5_000_000 == 0 {
            eprintln!("  ...processed {row_idx} rows");
        }
    }

    while let Some((f, start)) = stack.pop() {
        flame_frames.push(FlameFrame {
            name: funcs[f].name.clone(),
            depth: stack.len(),
            start,
            end: row_idx.saturating_sub(1),
        });
    }

    TraceAnalysis {
        pc_counts,
        call_counts,
        caller_edges,
        flame_frames,
        trace_len: row_idx,
    }
}

#[inline]
fn memchr_byte(needle: u8, haystack: &[u8]) -> Option<usize> {
    haystack.iter().position(|&b| b == needle)
}

#[inline]
fn parse_u32_fast(bytes: &[u8]) -> u32 {
    let mut n: u32 = 0;
    for &b in bytes {
        if b.is_ascii_digit() {
            n = n.wrapping_mul(10).wrapping_add((b - b'0') as u32);
        } else if b == b' ' || b == b'\r' || b == b'\n' {
            // skip
        } else {
            break;
        }
    }
    n
}

// ---------------------------------------------------------------------------
// Stats & basic blocks
// ---------------------------------------------------------------------------

fn build_stats(
    funcs: &[Function],
    pc_counts: &HashMap<u32, u64>,
    call_counts: &HashMap<usize, u64>,
) -> Vec<FunctionStats> {
    let mut stats: Vec<FunctionStats> = funcs
        .iter()
        .enumerate()
        .map(|(i, f)| {
            let mut insn_count = 0u64;
            let mut pc_hits = BTreeMap::new();
            for addr in (f.addr..f.addr + f.size).step_by(4) {
                if let Some(&count) = pc_counts.get(&addr) {
                    insn_count += count;
                    pc_hits.insert(addr, count);
                }
            }
            FunctionStats {
                name: f.name.clone(),
                addr: f.addr,
                size: f.size,
                call_count: *call_counts.get(&i).unwrap_or(&0),
                insn_count,
                pc_hits,
            }
        })
        .filter(|s| s.insn_count > 0)
        .collect();
    stats.sort_by(|a, b| b.insn_count.cmp(&a.insn_count));
    stats
}

fn is_branch_or_jump(word: u32) -> bool {
    let opcode = word & 0x7f;
    matches!(
        opcode,
        0b1100011 | // B-type
        0b1101111 | // JAL
        0b1100111 | // JALR
        0b1110011   // ECALL/EBREAK
    )
}

fn find_basic_blocks(
    funcs: &[Function],
    elf_bytes: &[u8],
    elf: &Elf,
    pc_counts: &HashMap<u32, u64>,
) -> Vec<BasicBlock> {
    let mut blocks = Vec::new();
    for (func_idx, func) in funcs.iter().enumerate() {
        let Some(code) = load_code(elf_bytes, elf, func.addr, func.size) else {
            continue;
        };
        let mut block_start = func.addr;
        let mut block_hits: u64 = 0;
        for i in (0..code.len()).step_by(4) {
            if i + 4 > code.len() {
                break;
            }
            let pc = func.addr + i as u32;
            let word = u32::from_le_bytes([code[i], code[i + 1], code[i + 2], code[i + 3]]);
            let hits = pc_counts.get(&pc).copied().unwrap_or(0);
            block_hits += hits;
            if is_branch_or_jump(word) || i + 4 >= code.len() {
                if block_hits > 0 {
                    blocks.push(BasicBlock {
                        func_idx,
                        start_pc: block_start,
                        end_pc: pc + 4,
                        hit_count: block_hits,
                    });
                }
                block_start = pc + 4;
                block_hits = 0;
            }
        }
    }
    blocks.sort_by(|a, b| b.hit_count.cmp(&a.hit_count));
    blocks
}

// ---------------------------------------------------------------------------
// Disassembly
// ---------------------------------------------------------------------------

fn reg_name(r: usize) -> &'static str {
    const NAMES: [&str; 32] = [
        "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3",
        "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
        "t3", "t4", "t5", "t6",
    ];
    if r < 32 { NAMES[r] } else { "??" }
}

fn format_instruction(insn: &Instruction) -> String {
    let op = format!("{:?}", insn.opc).to_lowercase();
    let rd = insn.rd.map(reg_name);
    let rs1 = insn.rs1.map(reg_name);
    let rs2 = insn.rs2.map(reg_name);
    let imm = insn.imm;
    match (rd, rs1, rs2, imm) {
        (Some(d), Some(s1), Some(s2), None) => format!("{op} {d}, {s1}, {s2}"),
        (Some(d), Some(s1), None, Some(i)) if op.starts_with('l') || op == "jalr" => {
            format!("{op} {d}, {i}({s1})")
        }
        (Some(d), Some(s1), None, Some(i)) => format!("{op} {d}, {s1}, {i}"),
        (None, Some(s1), Some(s2), Some(i)) if op.starts_with('s') => {
            format!("{op} {s2}, {i}({s1})")
        }
        (None, Some(s1), Some(s2), Some(i)) => format!("{op} {s1}, {s2}, {i}"),
        (Some(d), None, None, Some(i)) => format!("{op} {d}, 0x{:x}", i as u32),
        (Some(d), None, None, None) => format!("{op} {d}"),
        (None, None, None, None) => op,
        _ => {
            let mut s = op;
            if let Some(d) = rd { s += &format!(" {d}"); }
            if let Some(s1) = rs1 { s += &format!(", {s1}"); }
            if let Some(s2) = rs2 { s += &format!(", {s2}"); }
            if let Some(i) = imm { s += &format!(", {i}"); }
            s
        }
    }
}

/// Returns vec of (pc, raw_word, hits, disasm_text).
fn disassemble_range(
    elf_bytes: &[u8],
    elf: &Elf,
    addr: u32,
    size: u32,
    pc_hits: &BTreeMap<u32, u64>,
) -> Vec<(u32, u32, u64, String)> {
    let mut lines = Vec::new();
    let Some(code) = load_code(elf_bytes, elf, addr, size) else {
        return lines;
    };
    for i in (0..code.len()).step_by(4) {
        if i + 4 > code.len() {
            break;
        }
        let pc = addr + i as u32;
        let word = u32::from_le_bytes([code[i], code[i + 1], code[i + 2], code[i + 3]]);
        let hits = pc_hits.get(&pc).copied().unwrap_or(0);
        let disasm = match word.decode(Isa::Rv32) {
            Ok(insn) => format_instruction(&insn),
            Err(_) => format!(".word 0x{word:08x}"),
        };
        lines.push((pc, word, hits, disasm));
    }
    lines
}

// ---------------------------------------------------------------------------
// HTML helpers
// ---------------------------------------------------------------------------

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

fn js_escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "")
}

fn hit_class(hits: u64, max_hits: u64) -> &'static str {
    if hits > max_hits / 10 {
        "hot"
    } else if hits > max_hits / 100 {
        "warm"
    } else {
        "cold"
    }
}

// ---------------------------------------------------------------------------
// HTML generation
// ---------------------------------------------------------------------------

fn generate_html(
    stats: &[FunctionStats],
    flame_frames: &[FlameFrame],
    blocks: &[BasicBlock],
    funcs: &[Function],
    caller_edges: &HashMap<usize, HashMap<usize, u64>>,
    elf_bytes: &[u8],
    elf: &Elf,
    total_insns: u64,
    trace_len: usize,
) -> String {
    let mut html = String::new();

    let max_pc_hits = stats
        .iter()
        .flat_map(|s| s.pc_hits.values())
        .max()
        .copied()
        .unwrap_or(1);

    // --- Flame data JSON ---
    let flame_json: Vec<String> = flame_frames
        .iter()
        .map(|f| {
            format!(
                "{{\"name\":\"{}\",\"depth\":{},\"start\":{},\"end\":{}}}",
                js_escape(&f.name), f.depth, f.start, f.end,
            )
        })
        .collect();

    // --- Disassembly blocks (precomputed) ---
    let disasm_blocks: Vec<(String, Vec<(u32, u32, u64, String)>)> = stats
        .iter()
        .map(|s| {
            let lines = disassemble_range(elf_bytes, elf, s.addr, s.size, &s.pc_hits);
            (s.name.clone(), lines)
        })
        .collect();

    // --- Map function addr → row_id in stats table ---
    let addr_to_row: HashMap<u32, usize> = stats
        .iter()
        .enumerate()
        .map(|(i, s)| (s.addr, i))
        .collect();

    // --- Callers lookup for a function index ---
    // Returns (name, call_count, Option<row_id in stats>)
    let callers_for = |fi: usize| -> Vec<(&str, u64, Option<usize>)> {
        let mut v: Vec<(&str, u64, Option<usize>)> = caller_edges
            .iter()
            .filter_map(|(caller_idx, edges)| {
                edges.get(&fi).map(|&c| {
                    let name = funcs[*caller_idx].name.as_str();
                    let row = addr_to_row.get(&funcs[*caller_idx].addr).copied();
                    (name, c, row)
                })
            })
            .collect();
        v.sort_by(|a, b| b.1.cmp(&a.1));
        v.truncate(10);
        v
    };

    // ====================== Head ======================
    html.push_str(&format!(
        r##"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>RISC-V Execution Profile</title>
<style>
* {{ box-sizing: border-box; margin: 0; padding: 0; }}
body {{ font-family: 'Segoe UI', system-ui, -apple-system, sans-serif; background: #0d1117; color: #c9d1d9; }}
.container {{ max-width: 1400px; margin: 0 auto; padding: 24px; }}
h1 {{ font-size: 1.6em; margin-bottom: 8px; color: #58a6ff; }}
h2 {{ font-size: 1.2em; margin: 24px 0 12px; color: #79c0ff; }}
.summary {{ color: #8b949e; margin-bottom: 16px; font-size: 0.95em; }}

/* Tabs */
.tab-bar {{ display: flex; gap: 0; border-bottom: 2px solid #30363d; }}
.tab-btn {{ padding: 10px 24px; background: transparent; border: none; color: #8b949e; cursor: pointer; font-size: 0.95em; font-weight: 600; border-bottom: 2px solid transparent; margin-bottom: -2px; }}
.tab-btn:hover {{ color: #c9d1d9; }}
.tab-btn.active {{ color: #58a6ff; border-bottom-color: #58a6ff; }}
.tab-panel {{ display: none; padding-top: 16px; }}
.tab-panel.active {{ display: block; }}

/* Flame graph */
#flame-controls {{ margin-bottom: 8px; display: flex; align-items: center; gap: 8px; }}
#flame-controls button {{ background: #21262d; border: 1px solid #30363d; color: #c9d1d9; padding: 4px 12px; border-radius: 4px; cursor: pointer; font-size: 0.85em; }}
#flame-controls button:hover {{ background: #30363d; color: #58a6ff; }}
#flame-controls button:disabled {{ opacity: 0.4; cursor: default; }}
#flame-info {{ color: #8b949e; font-size: 0.85em; }}
#flame-container {{ position: relative; width: 100%; overflow-x: auto; background: #161b22; border: 1px solid #30363d; border-radius: 6px; margin-bottom: 24px; }}
#flame-canvas {{ display: block; cursor: pointer; }}
.flame-tooltip {{ position: absolute; display: none; background: #1c2128; border: 1px solid #444c56; padding: 6px 10px; border-radius: 4px; font-size: 13px; pointer-events: none; color: #c9d1d9; z-index: 10; white-space: nowrap; }}

/* Tables */
.tbl-wrap {{ width: 100%; overflow-x: auto; }}
table {{ width: 100%; border-collapse: collapse; background: #161b22; border: 1px solid #30363d; border-radius: 6px; overflow: hidden; table-layout: fixed; }}
th {{ background: #21262d; padding: 10px 14px; text-align: left; cursor: pointer; user-select: none; font-weight: 600; color: #8b949e; border-bottom: 1px solid #30363d; white-space: nowrap; }}
th:hover {{ color: #c9d1d9; }}
th.sorted-asc::after {{ content: ' ▲'; }}
th.sorted-desc::after {{ content: ' ▼'; }}
td {{ padding: 8px 14px; border-bottom: 1px solid #21262d; font-family: 'JetBrains Mono', 'Fira Code', monospace; font-size: 0.85em; }}
tr:hover {{ background: #1c2128; }}
tr.clickable {{ cursor: pointer; }}
.bar {{ height: 14px; background: #238636; border-radius: 2px; min-width: 1px; }}
.num {{ text-align: right; }}
.fn-col {{ overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }}

/* Disassembly */
.disasm-section {{ margin-bottom: 20px; }}
.disasm-header {{ font-size: 1em; color: #58a6ff; margin-bottom: 4px; cursor: pointer; }}
.disasm-header:hover {{ text-decoration: underline; }}
.disasm-body {{ display: none; background: #0d1117; border: 1px solid #30363d; border-radius: 4px; padding: 8px 12px; overflow-x: auto; }}
.disasm-body.open {{ display: block; }}
pre {{ font-family: 'JetBrains Mono', 'Fira Code', monospace; font-size: 0.82em; line-height: 1.5; white-space: pre; }}
.hot {{ color: #f85149; font-weight: bold; }}
.warm {{ color: #d29922; }}
.cold {{ color: #8b949e; }}

/* Search */
.search {{ width: 100%; padding: 8px 12px; margin-bottom: 12px; background: #0d1117; border: 1px solid #30363d; border-radius: 6px; color: #c9d1d9; font-size: 0.95em; }}
.search:focus {{ outline: none; border-color: #58a6ff; }}

/* Hot block cards */
.block-card {{ background: #161b22; border: 1px solid #30363d; border-radius: 6px; padding: 16px; margin-bottom: 12px; }}
.block-card h3 {{ font-size: 0.95em; color: #58a6ff; margin-bottom: 8px; word-break: break-all; }}
.block-meta {{ font-size: 0.85em; color: #8b949e; margin-bottom: 8px; }}
.block-meta span {{ margin-right: 16px; }}
.callers {{ margin-bottom: 8px; }}
.callers .tag {{ display: inline-block; background: #21262d; border: 1px solid #30363d; border-radius: 4px; padding: 2px 8px; margin: 2px 4px 2px 0; font-size: 0.8em; color: #79c0ff; max-width: 400px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; cursor: pointer; text-decoration: none; }}
.callers .tag:hover {{ background: #30363d; color: #58a6ff; }}
.block-disasm {{ max-height: 300px; overflow-y: auto; }}
</style>
</head>
<body>
<div class="container">
<h1>RISC-V Execution Profile</h1>
<p class="summary">
  {total_insns} total instruction executions across {num_funcs} functions &middot; {trace_len} trace rows
</p>

<div class="tab-bar">
  <button class="tab-btn active" data-tab="timeline">Timeline</button>
  <button class="tab-btn" data-tab="functions">Functions</button>
  <button class="tab-btn" data-tab="hotblocks">Hot Blocks</button>
</div>
"##,
        num_funcs = stats.len(),
    ));

    // ====================== Tab 1: Timeline ======================
    html.push_str(r##"
<div class="tab-panel active" id="tab-timeline">
<h2>Flame Graph</h2>
<div id="flame-controls">
  <button id="flame-reset" title="Reset zoom">⟲ Reset zoom</button>
  <button id="flame-back" title="Zoom out one level">← Back</button>
  <span id="flame-info"></span>
</div>
<div id="flame-container">
  <canvas id="flame-canvas" width="1350" height="300"></canvas>
  <div class="flame-tooltip" id="flame-tooltip"></div>
</div>
</div>
"##);

    // ====================== Tab 2: Functions ======================
    // Column order: Address | Insn Hits | Calls | % Total | Dist | Function
    // Function is last so its overflow doesn't push numeric columns off-screen.
    html.push_str(r#"
<div class="tab-panel" id="tab-functions">
<h2>Function Profile</h2>
<input type="text" class="search" id="fn-search" placeholder="Filter functions…" autocomplete="off">
<div class="tbl-wrap">
<table id="profile-table">
<thead>
<tr>
  <th data-col="0" data-type="num" class="num" style="width:110px">Address</th>
  <th data-col="1" data-type="num" class="num sorted-desc" style="width:100px">Insn Hits</th>
  <th data-col="2" data-type="num" class="num" style="width:80px">Calls</th>
  <th data-col="3" data-type="num" class="num" style="width:80px">% Total</th>
  <th data-col="4" data-type="bar" style="width:100px">Dist</th>
  <th data-col="5" data-type="str">Function</th>
</tr>
</thead>
<tbody>
"#);

    for (row_id, s) in stats.iter().enumerate() {
        let pct = if total_insns > 0 {
            (s.insn_count as f64 / total_insns as f64) * 100.0
        } else {
            0.0
        };
        html.push_str(&format!(
            "<tr class=\"clickable\" data-fn=\"fn-{row_id}\">\
             <td class=\"num\">0x{:08x}</td>\
             <td class=\"num\">{}</td>\
             <td class=\"num\">{}</td>\
             <td class=\"num\">{:.2}%</td>\
             <td><div class=\"bar\" style=\"width:{:.1}%\"></div></td>\
             <td class=\"fn-col\" title=\"{}\">{}</td>\
             </tr>\n",
            s.addr,
            s.insn_count,
            s.call_count,
            pct,
            pct.min(100.0),
            html_escape(&s.name),
            html_escape(&s.name),
        ));
    }

    html.push_str("</tbody></table>\n</div>\n\n<h2>Disassembly</h2>\n");

    for (row_id, (name, lines)) in disasm_blocks.iter().enumerate() {
        html.push_str(&format!(
            "<div class=\"disasm-section\" id=\"fn-{row_id}\">\n\
             <div class=\"disasm-header\" onclick=\"this.nextElementSibling.classList.toggle('open')\">\
             ▶ {}</div>\n\
             <div class=\"disasm-body\"><pre>",
            html_escape(name),
        ));
        html.push_str("  Address     Encoding      Hits  Instruction\n");
        html.push_str("  ─────────   ────────  ────────  ───────────\n");
        for (pc, word, hits, text) in lines {
            let class = hit_class(*hits, max_pc_hits);
            html.push_str(&format!(
                "<span class=\"{class}\">  0x{pc:08x}:  {word:08x}  {hits:>8}  {}</span>\n",
                html_escape(text),
            ));
        }
        html.push_str("</pre></div></div>\n");
    }
    html.push_str("</div>\n");

    // ====================== Tab 3: Hot Blocks ======================
    let top_blocks = &blocks[..blocks.len().min(200)];

    html.push_str(r#"
<div class="tab-panel" id="tab-hotblocks">
<h2>Hottest Basic Blocks</h2>
<p class="summary">Basic blocks sorted by total instruction executions. Shows which functions call into each block&#39;s parent function.</p>
<input type="text" class="search" id="bb-search" placeholder="Filter by function name…" autocomplete="off">
<div id="hotblocks-list">
"#);

    for (i, b) in top_blocks.iter().enumerate() {
        let fname = html_escape(&funcs[b.func_idx].name);
        let n_insn = (b.end_pc - b.start_pc) / 4;
        let callers = callers_for(b.func_idx);

        html.push_str(&format!(
            "<div class=\"block-card\" data-fn=\"{}\">\n\
             <h3>#{} &mdash; {}</h3>\n\
             <div class=\"block-meta\">\
             <span>0x{:08x}..0x{:08x}</span>\
             <span>{n_insn} insns</span>\
             <span><b>{}</b> total hits</span>\
             </div>\n",
            html_escape(&funcs[b.func_idx].name),
            i + 1,
            fname,
            b.start_pc,
            b.end_pc,
            b.hit_count,
        ));

        if !callers.is_empty() {
            html.push_str("<div class=\"callers\">Called from: ");
            for (cname, count, row_id) in &callers {
                if let Some(rid) = row_id {
                    html.push_str(&format!(
                        "<a class=\"tag\" href=\"#fn-{rid}\" data-row=\"{rid}\" title=\"{}\">{} (×{})</a>",
                        html_escape(cname),
                        html_escape(cname),
                        count,
                    ));
                } else {
                    html.push_str(&format!(
                        "<span class=\"tag\" title=\"{}\">{} (×{})</span>",
                        html_escape(cname),
                        html_escape(cname),
                        count,
                    ));
                }
            }
            html.push_str("</div>\n");
        }

        // Inline disassembly
        let mut block_pc_hits = BTreeMap::new();
        if let Some(fs) = stats.iter().find(|s| s.addr == funcs[b.func_idx].addr) {
            for addr in (b.start_pc..b.end_pc).step_by(4) {
                if let Some(&h) = fs.pc_hits.get(&addr) {
                    block_pc_hits.insert(addr, h);
                }
            }
        }
        let dlines = disassemble_range(elf_bytes, elf, b.start_pc, b.end_pc - b.start_pc, &block_pc_hits);
        if !dlines.is_empty() {
            html.push_str("<div class=\"block-disasm\"><pre>");
            for (pc, word, hits, text) in &dlines {
                let class = hit_class(*hits, max_pc_hits);
                html.push_str(&format!(
                    "<span class=\"{class}\">0x{pc:08x}  {word:08x}  {hits:>8}  {}</span>\n",
                    html_escape(text),
                ));
            }
            html.push_str("</pre></div>\n");
        }
        html.push_str("</div>\n");
    }

    html.push_str("</div></div>\n");

    // ====================== JavaScript ======================
    html.push_str(&format!(
        r##"
<script>
// --- Tabs ---
document.querySelectorAll('.tab-btn').forEach(btn => {{
  btn.addEventListener('click', () => {{
    document.querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
    document.querySelectorAll('.tab-panel').forEach(p => p.classList.remove('active'));
    btn.classList.add('active');
    document.getElementById('tab-' + btn.dataset.tab).classList.add('active');
  }});
}});

// --- Sortable table ---
(function() {{
  const table = document.getElementById('profile-table');
  if (!table) return;
  const headers = table.querySelectorAll('th');
  let sortCol = 1, sortAsc = false;

  headers.forEach(th => {{
    th.addEventListener('click', () => {{
      const col = parseInt(th.dataset.col);
      if (col === sortCol) {{ sortAsc = !sortAsc; }} else {{ sortCol = col; sortAsc = false; }}
      headers.forEach(h => h.classList.remove('sorted-asc', 'sorted-desc'));
      th.classList.add(sortAsc ? 'sorted-asc' : 'sorted-desc');
      const tbody = table.querySelector('tbody');
      const rows = Array.from(tbody.querySelectorAll('tr'));
      const isNum = headers[sortCol].dataset.type === 'num';
      rows.sort((a, b) => {{
        let va = a.children[sortCol].textContent.trim().replace('%','');
        let vb = b.children[sortCol].textContent.trim().replace('%','');
        if (isNum) {{ va = parseFloat(va) || 0; vb = parseFloat(vb) || 0; }}
        let cmp = va < vb ? -1 : va > vb ? 1 : 0;
        return sortAsc ? cmp : -cmp;
      }});
      rows.forEach(r => tbody.appendChild(r));
    }});
  }});

  table.querySelectorAll('tr.clickable').forEach(tr => {{
    tr.addEventListener('click', () => {{
      const el = document.getElementById(tr.dataset.fn);
      if (el) {{
        el.querySelector('.disasm-body').classList.add('open');
        el.scrollIntoView({{ behavior: 'smooth', block: 'start' }});
      }}
    }});
  }});
}})();

// --- Function search ---
document.getElementById('fn-search').addEventListener('input', e => {{
  const q = e.target.value.toLowerCase();
  document.querySelectorAll('#profile-table tbody tr').forEach(tr => {{
    // Function name is the last column (index 5)
    tr.style.display = tr.children[5].textContent.toLowerCase().includes(q) ? '' : 'none';
  }});
}});

// --- Hot block search ---
document.getElementById('bb-search').addEventListener('input', e => {{
  const q = e.target.value.toLowerCase();
  document.querySelectorAll('#hotblocks-list .block-card').forEach(card => {{
    card.style.display = (card.dataset.fn || '').toLowerCase().includes(q) ? '' : 'none';
  }});
}});

// --- Clickable caller tags: switch to Functions tab & scroll to function ---
document.querySelectorAll('.callers a.tag[data-row]').forEach(a => {{
  a.addEventListener('click', e => {{
    e.preventDefault();
    // Switch to Functions tab
    document.querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
    document.querySelectorAll('.tab-panel').forEach(p => p.classList.remove('active'));
    const fnBtn = document.querySelector('.tab-btn[data-tab="functions"]');
    if (fnBtn) fnBtn.classList.add('active');
    const fnPanel = document.getElementById('tab-functions');
    if (fnPanel) fnPanel.classList.add('active');
    // Find and scroll to the function disassembly
    const el = document.getElementById('fn-' + a.dataset.row);
    if (el) {{
      el.querySelector('.disasm-body').classList.add('open');
      el.scrollIntoView({{ behavior: 'smooth', block: 'start' }});
    }}
  }});
}});

// --- Flame graph with zoom ---
const flameData = [{flame_json}];
const canvas = document.getElementById('flame-canvas');
const ctx = canvas.getContext('2d');
const tooltip = document.getElementById('flame-tooltip');
const totalTS = {trace_len};
const resetBtn = document.getElementById('flame-reset');
const backBtn = document.getElementById('flame-back');
const flameInfo = document.getElementById('flame-info');

let viewStart = 0;
let viewEnd = totalTS;
let zoomStack = [];

function updateButtons() {{
  const zoomed = viewStart !== 0 || viewEnd !== totalTS;
  resetBtn.disabled = !zoomed;
  backBtn.disabled = zoomStack.length === 0;
  if (zoomed) {{
    const pct = ((viewEnd - viewStart) / totalTS * 100).toFixed(2);
    flameInfo.textContent = 'Viewing ' + pct + '% of total trace';
  }} else {{
    flameInfo.textContent = '';
  }}
}}

function drawFlame() {{
  if (flameData.length === 0) return;
  const W = canvas.width;
  const rowH = 22;

  // Collect visible frames with their original index for stable coloring
  const visible = [];
  for (let i = 0; i < flameData.length; i++) {{
    const f = flameData[i];
    if (f.end >= viewStart && f.start <= viewEnd) visible.push({{ f, i }});
  }}
  const maxDepth = visible.reduce((m, v) => Math.max(m, v.f.depth), 0);
  canvas.height = Math.max(60, (maxDepth + 2) * rowH);

  ctx.fillStyle = '#161b22';
  ctx.fillRect(0, 0, W, canvas.height);

  const colors = ['#238636','#1f6feb','#8957e5','#da3633','#d29922','#3fb950','#79c0ff','#f0883e'];
  const span = viewEnd - viewStart;

  for (const {{ f, i }} of visible) {{
    const x = ((f.start - viewStart) / span) * W;
    const w = Math.max(1, ((f.end - f.start + 1) / span) * W);
    const y = canvas.height - (f.depth + 1) * rowH;
    if (x + w < 0 || x > W) continue;
    ctx.fillStyle = colors[i % colors.length];
    ctx.fillRect(x, y, w, rowH - 1);
    if (w > 40) {{
      ctx.fillStyle = '#fff';
      ctx.font = '11px monospace';
      ctx.save();
      ctx.beginPath();
      ctx.rect(Math.max(0, x), y, w, rowH);
      ctx.clip();
      ctx.fillText(f.name, Math.max(0, x) + 3, y + rowH - 5);
      ctx.restore();
    }}
  }}
  updateButtons();
}}

function hitTest(mx, my) {{
  const W = canvas.width;
  const rowH = 22;
  const span = viewEnd - viewStart;
  let best = null;
  for (const f of flameData) {{
    if (f.end < viewStart || f.start > viewEnd) continue;
    const x = ((f.start - viewStart) / span) * W;
    const w = Math.max(1, ((f.end - f.start + 1) / span) * W);
    const y = canvas.height - (f.depth + 1) * rowH;
    if (mx >= x && mx <= x + w && my >= y && my <= y + rowH) best = f;
  }}
  return best;
}}

canvas.addEventListener('mousemove', e => {{
  const rect = canvas.getBoundingClientRect();
  const mx = (e.clientX - rect.left) * (canvas.width / rect.width);
  const my = (e.clientY - rect.top) * (canvas.height / rect.height);
  const hit = hitTest(mx, my);
  if (hit) {{
    tooltip.style.display = 'block';
    tooltip.style.left = (e.clientX - rect.left + 12) + 'px';
    tooltip.style.top = (e.clientY - rect.top - 28) + 'px';
    const dur = hit.end - hit.start + 1;
    const pctTotal = (dur / totalTS * 100).toFixed(2);
    const pctView = (dur / (viewEnd - viewStart) * 100).toFixed(2);
    tooltip.textContent = hit.name + ' (' + pctTotal + '% total, ' + pctView + '% view) — click to zoom';
  }} else {{
    tooltip.style.display = 'none';
  }}
}});

canvas.addEventListener('mouseleave', () => {{ tooltip.style.display = 'none'; }});

canvas.addEventListener('click', e => {{
  const rect = canvas.getBoundingClientRect();
  const mx = (e.clientX - rect.left) * (canvas.width / rect.width);
  const my = (e.clientY - rect.top) * (canvas.height / rect.height);
  const hit = hitTest(mx, my);
  if (hit) {{
    zoomStack.push({{ start: viewStart, end: viewEnd }});
    viewStart = hit.start;
    viewEnd = hit.end + 1;
    drawFlame();
  }}
}});

canvas.addEventListener('contextmenu', e => {{
  e.preventDefault();
  if (zoomStack.length > 0) {{
    const prev = zoomStack.pop();
    viewStart = prev.start;
    viewEnd = prev.end;
    drawFlame();
  }}
}});

// Mouse wheel zoom: scroll to zoom in/out at cursor position
canvas.addEventListener('wheel', e => {{
  e.preventDefault();
  const rect = canvas.getBoundingClientRect();
  const mx = (e.clientX - rect.left) / rect.width; // 0..1 position
  const span = viewEnd - viewStart;
  const factor = e.deltaY > 0 ? 1.3 : 1 / 1.3; // scroll down = zoom out
  const newSpan = Math.min(totalTS, Math.max(100, span * factor));
  const center = viewStart + mx * span;
  let newStart = Math.max(0, center - mx * newSpan);
  let newEnd = newStart + newSpan;
  if (newEnd > totalTS) {{ newEnd = totalTS; newStart = Math.max(0, newEnd - newSpan); }}
  if (newStart !== viewStart || newEnd !== viewEnd) {{
    if (zoomStack.length === 0 || zoomStack.length < 200) {{
      zoomStack.push({{ start: viewStart, end: viewEnd }});
    }}
    viewStart = Math.floor(newStart);
    viewEnd = Math.ceil(newEnd);
    drawFlame();
  }}
}}, {{ passive: false }});

resetBtn.addEventListener('click', () => {{
  zoomStack = [];
  viewStart = 0;
  viewEnd = totalTS;
  drawFlame();
}});

backBtn.addEventListener('click', () => {{
  if (zoomStack.length > 0) {{
    const prev = zoomStack.pop();
    viewStart = prev.start;
    viewEnd = prev.end;
    drawFlame();
  }}
}});

drawFlame();
</script>
</div>
</body>
</html>
"##,
        flame_json = flame_json.join(","),
    ));

    html
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!(
            "Usage: {} <elf-file> <trace-dump.csv> [output.html]",
            args[0]
        );
        process::exit(1);
    }

    let elf_path = &args[1];
    let trace_path = &args[2];
    let output_path = if args.len() > 3 {
        PathBuf::from(&args[3])
    } else {
        PathBuf::from("profile.html")
    };

    eprintln!("Loading ELF: {elf_path}");
    let elf_bytes = fs::read(elf_path).unwrap_or_else(|e| {
        eprintln!("Failed to read ELF: {e}");
        process::exit(1);
    });
    let elf = Elf::parse(&elf_bytes).unwrap_or_else(|e| {
        eprintln!("Failed to parse ELF: {e}");
        process::exit(1);
    });

    let funcs = load_functions(&elf);
    eprintln!("Found {} functions in symbol table", funcs.len());

    eprintln!("Streaming trace: {trace_path}");
    let analysis = stream_analyze(trace_path, &funcs);
    eprintln!("Processed {} trace rows", analysis.trace_len);

    eprintln!("Building stats...");
    let stats = build_stats(&funcs, &analysis.pc_counts, &analysis.call_counts);
    let total_insns: u64 = stats.iter().map(|s| s.insn_count).sum();

    eprintln!("Detecting basic blocks...");
    let blocks = find_basic_blocks(&funcs, &elf_bytes, &elf, &analysis.pc_counts);
    eprintln!("Found {} hot basic blocks", blocks.len());

    eprintln!("Generating HTML...");
    let html = generate_html(
        &stats,
        &analysis.flame_frames,
        &blocks,
        &funcs,
        &analysis.caller_edges,
        &elf_bytes,
        &elf,
        total_insns,
        analysis.trace_len,
    );

    let mut out = fs::File::create(&output_path).unwrap_or_else(|e| {
        eprintln!("Failed to create output: {e}");
        process::exit(1);
    });
    out.write_all(html.as_bytes()).unwrap();

    eprintln!(
        "Done! {} functions, {} total insn hits",
        stats.len(),
        total_insns
    );
    eprintln!("Output: {}", output_path.display());
}

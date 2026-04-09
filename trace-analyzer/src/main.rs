use std::collections::{BTreeMap, HashMap};
use std::io::Write;
use std::path::PathBuf;
use std::{env, fs, process};

use goblin::elf::sym::STT_FUNC;
use goblin::elf::Elf;
use raki::decode::Decode;
use raki::instruction::Instruction;
use raki::Isa;

// ---------------------------------------------------------------------------
// Data types
// ---------------------------------------------------------------------------

/// A function extracted from the ELF symbol table.
struct Function {
    name: String,
    addr: u32,
    size: u32,
}

/// A single row from the trace CSV.
struct TraceRow {
    segment_idx: usize,
    timestamp: u64,
    pc: u32,
}

/// Aggregated stats for a function.
struct FunctionStats {
    name: String,
    addr: u32,
    size: u32,
    /// Number of times the function was *entered* (called)
    call_count: u64,
    /// Total number of instruction executions inside this function
    insn_count: u64,
    /// Per-PC hit counts within this function, sorted by offset
    pc_hits: BTreeMap<u32, u64>,
}

/// A frame in the flame-graph stack.
struct FlameFrame {
    name: String,
    depth: usize,
    start: usize,
    /// inclusive end timestamp index
    end: usize,
}

// ---------------------------------------------------------------------------
// ELF parsing
// ---------------------------------------------------------------------------

fn load_functions(elf: &Elf) -> Vec<Function> {
    let mut funcs: Vec<Function> = elf
        .syms
        .iter()
        .filter(|sym| sym.st_type() == STT_FUNC && sym.st_name != 0 && sym.st_size > 0)
        .map(|sym| Function {
            name: elf.strtab[sym.st_name].to_owned(),
            addr: sym.st_value as u32,
            size: sym.st_size as u32,
        })
        .collect();
    funcs.sort_by_key(|f| f.addr);
    funcs
}

/// Given a sorted list of functions and a PC, find which function owns it.
fn lookup_function(funcs: &[Function], pc: u32) -> Option<usize> {
    // binary search for the last function whose addr <= pc
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

// ---------------------------------------------------------------------------
// Trace parsing
// ---------------------------------------------------------------------------

fn load_trace(path: &str) -> Vec<TraceRow> {
    let content = fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Failed to read trace file '{path}': {e}");
        process::exit(1);
    });
    let mut rows = Vec::new();
    for (lineno, line) in content.lines().enumerate() {
        if lineno == 0 && line.starts_with("segment") {
            continue; // header
        }
        let parts: Vec<&str> = line.split(',').collect();
        if parts.len() < 3 {
            continue;
        }
        let segment_idx: usize = parts[0].trim().parse().unwrap_or(0);
        let timestamp: u64 = parts[1].trim().parse().unwrap_or(0);
        let pc: u32 = parts[2].trim().parse().unwrap_or(0);
        rows.push(TraceRow {
            segment_idx,
            timestamp,
            pc,
        });
    }
    // Sort by (segment_idx, timestamp)
    rows.sort_by(|a, b| {
        a.segment_idx
            .cmp(&b.segment_idx)
            .then(a.timestamp.cmp(&b.timestamp))
    });
    rows
}

// ---------------------------------------------------------------------------
// Analysis
// ---------------------------------------------------------------------------

fn analyze(funcs: &[Function], trace: &[TraceRow]) -> Vec<FunctionStats> {
    // Count per-PC hits
    let mut pc_counts: HashMap<u32, u64> = HashMap::new();
    for row in trace {
        *pc_counts.entry(row.pc).or_default() += 1;
    }

    // Detect function calls: a "call" happens when the function changes
    let mut call_counts: HashMap<usize, u64> = HashMap::new();
    let mut prev_func_idx: Option<usize> = None;
    for row in trace {
        let func_idx = lookup_function(funcs, row.pc);
        if func_idx != prev_func_idx {
            if let Some(idx) = func_idx {
                *call_counts.entry(idx).or_default() += 1;
            }
        }
        prev_func_idx = func_idx;
    }

    // Build per-function stats
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

/// Build flame-graph frames from the trace.
/// Each contiguous run of PCs inside the same function becomes a frame.
fn build_flame_frames(funcs: &[Function], trace: &[TraceRow]) -> Vec<FlameFrame> {
    if trace.is_empty() {
        return Vec::new();
    }

    // Track the "call stack" by detecting transitions.
    // We keep a simple model: each contiguous run in a function is a frame at depth 0.
    // When a function calls another, we nest. We approximate this using a shadow stack.
    let mut frames = Vec::new();
    let mut stack: Vec<(usize, usize)> = Vec::new(); // (func_idx, start_ts_index)

    for (ts_idx, row) in trace.iter().enumerate() {
        let func_idx = lookup_function(funcs, row.pc);
        let current = func_idx;

        // Check if we're still in the same function as top of stack
        if let Some(&(top_func, _)) = stack.last() {
            if current == Some(top_func) {
                continue; // same function, keep going
            }

            // Check if we returned to something already on the stack
            let return_pos = stack.iter().rposition(|(f, _)| Some(*f) == current);
            if let Some(pos) = return_pos {
                // Pop everything above the return target
                while stack.len() > pos + 1 {
                    let (f, start) = stack.pop().unwrap();
                    frames.push(FlameFrame {
                        name: funcs[f].name.clone(),
                        depth: stack.len(),
                        start,
                        end: ts_idx - 1,
                    });
                }
                continue;
            }

            // Otherwise, this is a new call
        }

        if let Some(fi) = current {
            stack.push((fi, ts_idx));
        }
    }

    // Flush remaining stack
    let last_idx = trace.len() - 1;
    while let Some((f, start)) = stack.pop() {
        frames.push(FlameFrame {
            name: funcs[f].name.clone(),
            depth: stack.len(),
            start,
            end: last_idx,
        });
    }

    frames
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
    if r < 32 {
        NAMES[r]
    } else {
        "??"
    }
}

fn format_instruction(insn: &Instruction) -> String {
    let op = format!("{:?}", insn.opc).to_lowercase();
    let rd = insn.rd.map(reg_name);
    let rs1 = insn.rs1.map(reg_name);
    let rs2 = insn.rs2.map(reg_name);
    let imm = insn.imm;

    match (rd, rs1, rs2, imm) {
        // R-type: op rd, rs1, rs2
        (Some(d), Some(s1), Some(s2), None) => format!("{op} {d}, {s1}, {s2}"),
        // I-type load/jalr: op rd, imm(rs1)
        (Some(d), Some(s1), None, Some(i))
            if op.starts_with("l") || op == "jalr" =>
        {
            format!("{op} {d}, {i}({s1})")
        }
        // I-type: op rd, rs1, imm
        (Some(d), Some(s1), None, Some(i)) => format!("{op} {d}, {s1}, {i}"),
        // S-type store: op rs2, imm(rs1)
        (None, Some(s1), Some(s2), Some(i)) if op.starts_with('s') => {
            format!("{op} {s2}, {i}({s1})")
        }
        // B-type: op rs1, rs2, imm
        (None, Some(s1), Some(s2), Some(i)) => format!("{op} {s1}, {s2}, {i}"),
        // U-type: op rd, imm
        (Some(d), None, None, Some(i)) => format!("{op} {d}, 0x{:x}", i as u32),
        // J-type: op rd, imm
        (Some(d), None, None, None) => format!("{op} {d}"),
        // Bare: op
        (None, None, None, None) => op,
        // Fallback
        _ => {
            let mut s = op;
            if let Some(d) = rd {
                s += &format!(" {d}");
            }
            if let Some(s1) = rs1 {
                s += &format!(", {s1}");
            }
            if let Some(s2) = rs2 {
                s += &format!(", {s2}");
            }
            if let Some(i) = imm {
                s += &format!(", {i}");
            }
            s
        }
    }
}

fn disassemble_function(elf_bytes: &[u8], elf: &Elf, func: &FunctionStats) -> Vec<String> {
    let mut lines = Vec::new();

    // Find the program header that contains this address
    let addr = func.addr;
    let size = func.size;

    let mut file_offset = None;
    for ph in &elf.program_headers {
        if ph.p_type == goblin::elf::program_header::PT_LOAD
            && (ph.p_flags & 1) != 0
            && addr >= ph.p_vaddr as u32
            && addr < (ph.p_vaddr + ph.p_memsz) as u32
        {
            let off_in_seg = (addr - ph.p_vaddr as u32) as u64;
            file_offset = Some((ph.p_offset + off_in_seg) as usize);
            break;
        }
    }

    let Some(offset) = file_offset else {
        lines.push(format!("  ; could not find code for 0x{addr:08x}"));
        return lines;
    };

    let end = (offset + size as usize).min(elf_bytes.len());
    let code = &elf_bytes[offset..end];

    for i in (0..code.len()).step_by(4) {
        if i + 4 > code.len() {
            break;
        }
        let pc = addr + i as u32;
        let word = u32::from_le_bytes([code[i], code[i + 1], code[i + 2], code[i + 3]]);
        let hits = func.pc_hits.get(&pc).copied().unwrap_or(0);

        let disasm = match word.decode(Isa::Rv32) {
            Ok(insn) => format_instruction(&insn),
            Err(_) => format!(".word 0x{word:08x}"),
        };

        lines.push(format!(
            "  0x{pc:08x}:  {word:08x}  {hits:>8}  {disasm}"
        ));
    }

    lines
}

// ---------------------------------------------------------------------------
// HTML generation
// ---------------------------------------------------------------------------

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn generate_html(
    stats: &[FunctionStats],
    flame_frames: &[FlameFrame],
    elf_bytes: &[u8],
    elf: &Elf,
    total_insns: u64,
    trace_len: usize,
) -> String {
    let mut html = String::new();

    // Precompute flame data as JSON
    let flame_json: Vec<String> = flame_frames
        .iter()
        .map(|f| {
            format!(
                "{{\"name\":\"{}\",\"depth\":{},\"start\":{},\"end\":{}}}",
                html_escape(&f.name),
                f.depth,
                f.start,
                f.end
            )
        })
        .collect();

    // Build disassembly for each function
    let disasm_blocks: Vec<(String, Vec<String>)> = stats
        .iter()
        .map(|s| {
            let lines = disassemble_function(elf_bytes, elf, s);
            (s.name.clone(), lines)
        })
        .collect();

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

/* Flame graph */
#flame-container {{ position: relative; width: 100%; overflow-x: auto; background: #161b22; border: 1px solid #30363d; border-radius: 6px; margin-bottom: 24px; }}
#flame-canvas {{ display: block; }}
#flame-tooltip {{ position: absolute; display: none; background: #1c2128; border: 1px solid #444c56; padding: 6px 10px; border-radius: 4px; font-size: 13px; pointer-events: none; color: #c9d1d9; z-index: 10; white-space: nowrap; }}

/* Table */
table {{ width: 100%; border-collapse: collapse; background: #161b22; border: 1px solid #30363d; border-radius: 6px; overflow: hidden; }}
th {{ background: #21262d; padding: 10px 14px; text-align: left; cursor: pointer; user-select: none; font-weight: 600; color: #8b949e; border-bottom: 1px solid #30363d; }}
th:hover {{ color: #c9d1d9; }}
th.sorted-asc::after {{ content: ' ▲'; }}
th.sorted-desc::after {{ content: ' ▼'; }}
td {{ padding: 8px 14px; border-bottom: 1px solid #21262d; font-family: 'JetBrains Mono', 'Fira Code', monospace; font-size: 0.85em; }}
tr:hover {{ background: #1c2128; }}
tr.clickable {{ cursor: pointer; }}
.bar {{ height: 14px; background: #238636; border-radius: 2px; min-width: 1px; }}
.num {{ text-align: right; }}

/* Disassembly */
.disasm-section {{ margin-bottom: 20px; }}
.disasm-header {{ font-size: 1em; color: #58a6ff; margin-bottom: 4px; cursor: pointer; }}
.disasm-header:hover {{ text-decoration: underline; }}
.disasm-body {{ display: none; background: #0d1117; border: 1px solid #30363d; border-radius: 4px; padding: 8px 12px; overflow-x: auto; }}
.disasm-body.open {{ display: block; }}
pre {{ font-family: 'JetBrains Mono', 'Fira Code', monospace; font-size: 0.82em; line-height: 1.5; }}
.hot {{ color: #f85149; font-weight: bold; }}
.warm {{ color: #d29922; }}
.cold {{ color: #8b949e; }}

/* Search */
#search {{ width: 100%; padding: 8px 12px; margin-bottom: 12px; background: #0d1117; border: 1px solid #30363d; border-radius: 6px; color: #c9d1d9; font-size: 0.95em; }}
#search:focus {{ outline: none; border-color: #58a6ff; }}
</style>
</head>
<body>
<div class="container">
<h1>RISC-V Execution Profile</h1>
<p class="summary">
  {total_insns} total instruction executions across {num_funcs} functions &middot; {trace_len} trace rows
</p>

<h2>Flame Graph</h2>
<div id="flame-container">
  <canvas id="flame-canvas" width="1350" height="300"></canvas>
  <div id="flame-tooltip"></div>
</div>

<h2>Function Profile</h2>
<input type="text" id="search" placeholder="Filter functions…" autocomplete="off">
<table id="profile-table">
<thead>
<tr>
  <th data-col="0" data-type="str">Function</th>
  <th data-col="1" data-type="num" class="num">Address</th>
  <th data-col="2" data-type="num" class="num sorted-desc">Insn Hits</th>
  <th data-col="3" data-type="num" class="num">Calls</th>
  <th data-col="4" data-type="num" class="num">% Total</th>
  <th data-col="5" data-type="bar">Distribution</th>
</tr>
</thead>
<tbody>
"##,
        num_funcs = stats.len(),
    ));

    // Table rows
    for s in stats {
        let pct = if total_insns > 0 {
            (s.insn_count as f64 / total_insns as f64) * 100.0
        } else {
            0.0
        };
        let bar_width = pct.min(100.0);
        html.push_str(&format!(
            "<tr class=\"clickable\" data-fn=\"{}\">\
             <td>{}</td>\
             <td class=\"num\">0x{:08x}</td>\
             <td class=\"num\">{}</td>\
             <td class=\"num\">{}</td>\
             <td class=\"num\">{:.2}%</td>\
             <td><div class=\"bar\" style=\"width:{:.1}%\"></div></td>\
             </tr>\n",
            html_escape(&s.name),
            html_escape(&s.name),
            s.addr,
            s.insn_count,
            s.call_count,
            pct,
            bar_width
        ));
    }

    html.push_str("</tbody></table>\n\n<h2>Disassembly</h2>\n");

    // Disassembly sections
    let max_hits = stats.iter().map(|s| s.insn_count).max().unwrap_or(1);
    for (name, lines) in &disasm_blocks {
        let id = html_escape(name);
        html.push_str(&format!(
            "<div class=\"disasm-section\" id=\"disasm-{}\">\n\
             <div class=\"disasm-header\" onclick=\"this.nextElementSibling.classList.toggle('open')\">\
             ▶ {}</div>\n\
             <div class=\"disasm-body\"><pre>",
            id, id
        ));
        html.push_str("  Address     Encoding    Hits  Instruction\n");
        html.push_str("  ─────────   ────────  ──────  ───────────\n");
        for line in lines {
            // Parse the hits number to apply coloring
            let class = if let Some(hits_str) = line.split_whitespace().nth(2) {
                if let Ok(hits) = hits_str.parse::<u64>() {
                    if hits > max_hits / 10 {
                        "hot"
                    } else if hits > max_hits / 100 {
                        "warm"
                    } else {
                        "cold"
                    }
                } else {
                    "cold"
                }
            } else {
                "cold"
            };
            html.push_str(&format!(
                "<span class=\"{}\">{}</span>\n",
                class,
                html_escape(line)
            ));
        }
        html.push_str("</pre></div></div>\n");
    }

    // JavaScript
    html.push_str(&format!(
        r##"
<script>
// --- Sortable table ---
const table = document.getElementById('profile-table');
const headers = table.querySelectorAll('th');
let sortCol = 2, sortAsc = false;

headers.forEach(th => {{
  th.addEventListener('click', () => {{
    const col = parseInt(th.dataset.col);
    if (col === sortCol) {{ sortAsc = !sortAsc; }} else {{ sortCol = col; sortAsc = false; }}
    headers.forEach(h => h.classList.remove('sorted-asc', 'sorted-desc'));
    th.classList.add(sortAsc ? 'sorted-asc' : 'sorted-desc');
    sortTable();
  }});
}});

function sortTable() {{
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
}}

// --- Search / Filter ---
document.getElementById('search').addEventListener('input', e => {{
  const q = e.target.value.toLowerCase();
  table.querySelectorAll('tbody tr').forEach(tr => {{
    tr.style.display = tr.children[0].textContent.toLowerCase().includes(q) ? '' : 'none';
  }});
}});

// --- Click row to show disassembly ---
table.querySelectorAll('tr.clickable').forEach(tr => {{
  tr.addEventListener('click', () => {{
    const fn_name = tr.dataset.fn;
    const el = document.getElementById('disasm-' + fn_name);
    if (el) {{
      el.querySelector('.disasm-body').classList.add('open');
      el.scrollIntoView({{ behavior: 'smooth', block: 'start' }});
    }}
  }});
}});

// --- Flame graph ---
const flameData = [{flame_json}];
const canvas = document.getElementById('flame-canvas');
const ctx = canvas.getContext('2d');
const tooltip = document.getElementById('flame-tooltip');
const totalTS = {trace_len};

function drawFlame() {{
  const W = canvas.width;
  const H = canvas.height;
  const maxDepth = flameData.reduce((m, f) => Math.max(m, f.depth), 0);
  const rowH = Math.min(24, H / (maxDepth + 2));
  canvas.height = (maxDepth + 2) * rowH;

  ctx.fillStyle = '#161b22';
  ctx.fillRect(0, 0, W, canvas.height);

  const colors = ['#238636','#1f6feb','#8957e5','#da3633','#d29922','#3fb950','#79c0ff','#f0883e'];

  flameData.forEach((f, i) => {{
    const x = (f.start / totalTS) * W;
    const w = Math.max(1, ((f.end - f.start + 1) / totalTS) * W);
    const y = canvas.height - (f.depth + 1) * rowH;
    ctx.fillStyle = colors[i % colors.length];
    ctx.fillRect(x, y, w, rowH - 1);
    if (w > 40) {{
      ctx.fillStyle = '#fff';
      ctx.font = '11px monospace';
      ctx.save();
      ctx.beginPath();
      ctx.rect(x, y, w, rowH);
      ctx.clip();
      ctx.fillText(f.name, x + 3, y + rowH - 5);
      ctx.restore();
    }}
  }});
}}

canvas.addEventListener('mousemove', e => {{
  const rect = canvas.getBoundingClientRect();
  const mx = e.clientX - rect.left;
  const my = e.clientY - rect.top;
  const W = canvas.width;
  const maxDepth = flameData.reduce((m, f) => Math.max(m, f.depth), 0);
  const rowH = Math.min(24, canvas.height / (maxDepth + 2));

  let hit = null;
  for (const f of flameData) {{
    const x = (f.start / totalTS) * W;
    const w = Math.max(1, ((f.end - f.start + 1) / totalTS) * W);
    const y = canvas.height - (f.depth + 1) * rowH;
    if (mx >= x && mx <= x + w && my >= y && my <= y + rowH) {{
      hit = f;
    }}
  }}
  if (hit) {{
    tooltip.style.display = 'block';
    tooltip.style.left = (e.clientX - rect.left + 12) + 'px';
    tooltip.style.top = (e.clientY - rect.top - 28) + 'px';
    const pct = ((hit.end - hit.start + 1) / totalTS * 100).toFixed(2);
    tooltip.textContent = hit.name + ' (' + pct + '%)';
  }} else {{
    tooltip.style.display = 'none';
  }}
}});

canvas.addEventListener('mouseleave', () => {{ tooltip.style.display = 'none'; }});

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

    eprintln!("Loading trace: {trace_path}");
    let trace = load_trace(trace_path);
    eprintln!("Loaded {} trace rows", trace.len());

    eprintln!("Analyzing...");
    let stats = analyze(&funcs, &trace);
    let total_insns: u64 = stats.iter().map(|s| s.insn_count).sum();
    let flame_frames = build_flame_frames(&funcs, &trace);

    eprintln!("Generating HTML...");
    let html = generate_html(&stats, &flame_frames, &elf_bytes, &elf, total_insns, trace.len());

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

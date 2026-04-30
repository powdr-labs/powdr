//! Profile-aware HTML report.
//!
//! Consumes a `pc,count` CSV (as produced by the openvm interpreter's
//! `pc_profile` module) and renders an interactive, self-contained HTML
//! report that:
//!
//!   * shows aggregate statistics (samples, hot pcs, coverage),
//!   * lists functions sorted by total samples (with cumulative coverage),
//!   * lets the user click a function to see per-instruction sample counts
//!     with heat coloring.
//!
//! The instruction stream rendered is the *optimized* one — i.e. the same
//! stream the interpreter actually executed.

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;
use std::process;

use powdr_riscv_elf::debug_info::SymbolTable;

use crate::disasm::disasm;
use crate::html::{html_escape, resolve_target, routine_end_idx};

/// Parse a `pc,count` CSV into a map. Lines starting with `#` and the header
/// row (where `pc` is non-numeric) are skipped.
pub(crate) fn parse_profile_csv(path: &Path) -> BTreeMap<u32, u64> {
    let text = fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Error reading profile {}: {e}", path.display());
        process::exit(1);
    });

    let mut out = BTreeMap::new();
    for (lineno, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let mut it = line.splitn(2, ',');
        let pc_s = match it.next() {
            Some(s) => s.trim(),
            None => continue,
        };
        let count_s = match it.next() {
            Some(s) => s.trim(),
            None => continue,
        };

        let pc = if let Some(stripped) = pc_s.strip_prefix("0x").or_else(|| pc_s.strip_prefix("0X"))
        {
            u32::from_str_radix(stripped, 16).ok()
        } else {
            pc_s.parse::<u32>().ok().or_else(|| u32::from_str_radix(pc_s, 16).ok())
        };
        let count = count_s.parse::<u64>().ok();

        match (pc, count) {
            (Some(pc), Some(count)) => {
                out.insert(pc, count);
            }
            _ => {
                // Header row or malformed; skip silently except for the first.
                if lineno == 0 {
                    continue;
                }
            }
        }
    }
    out
}

/// One row in the per-function summary table.
struct FunctionRow {
    addr: u32,
    end_addr: u32,
    name: String,
    total: u64,
    hottest_pc: u32,
    hottest_count: u64,
    instr_count: usize,
    hit_count: usize,
}

/// Group consecutive instructions starting at every symbol address into
/// "functions". A function ends at the next symbol address or at a `ret`
/// instruction (mirroring `routine_end_idx`).
fn build_function_rows(
    instructions: &[u32],
    symbols: &SymbolTable,
    pc_base: u32,
    profile: &BTreeMap<u32, u64>,
) -> Vec<FunctionRow> {
    let mut rows: Vec<FunctionRow> = Vec::new();
    let end_addr = pc_base + (instructions.len() as u32) * 4;

    for (&addr, names) in symbols.table().range(pc_base..end_addr) {
        let name = names
            .iter()
            .find(|n| !(n.starts_with('$') && n.len() <= 2))
            .map(|n| format!("{:#}", rustc_demangle::demangle(n)));
        let name = match name {
            Some(n) => n,
            None => continue,
        };

        let start_idx = ((addr - pc_base) / 4) as usize;
        if start_idx >= instructions.len() {
            continue;
        }
        let end_idx = routine_end_idx(instructions, start_idx, symbols, pc_base);
        let f_end_addr = pc_base + (end_idx as u32) * 4;

        let mut total = 0u64;
        let mut hottest_pc = addr;
        let mut hottest_count = 0u64;
        let mut hit_count = 0usize;
        for i in start_idx..end_idx {
            let pc = pc_base + (i as u32) * 4;
            if let Some(&c) = profile.get(&pc) {
                if c > 0 {
                    total = total.saturating_add(c);
                    hit_count += 1;
                    if c > hottest_count {
                        hottest_count = c;
                        hottest_pc = pc;
                    }
                }
            }
        }

        rows.push(FunctionRow {
            addr,
            end_addr: f_end_addr,
            name,
            total,
            hottest_pc,
            hottest_count,
            instr_count: end_idx - start_idx,
            hit_count,
        });
    }

    rows
}

/// Map a normalized fraction `f` in `[0, 1]` to a CSS background color.
/// We use a perceptually OK red→yellow ramp on top of the dark theme,
/// modulating opacity by `f`.
fn heat_color(frac: f64) -> String {
    if frac <= 0.0 {
        return String::new();
    }
    // Use log scale so warm regions are visually meaningful.
    let f = (frac.ln_1p() / 1f64.ln_1p()).clamp(0.0, 1.0);
    let alpha = 0.10 + 0.55 * f;
    // red→orange→yellow gradient
    let r = 248;
    let g = (81.0 + (180.0 - 81.0) * f).round() as u32;
    let b = (73.0 - 73.0 * f).round() as u32;
    format!("background:rgba({r},{g},{b},{alpha:.3});")
}

fn fmt_count(n: u64) -> String {
    let s = n.to_string();
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len() + s.len() / 3);
    for (i, &b) in bytes.iter().enumerate() {
        if i > 0 && (bytes.len() - i) % 3 == 0 {
            out.push(',');
        }
        out.push(b as char);
    }
    out
}

fn fmt_pct(num: u64, denom: u64) -> String {
    if denom == 0 {
        "0.000%".into()
    } else {
        format!("{:.3}%", (num as f64 / denom as f64) * 100.0)
    }
}

fn render_function_body(
    instructions: &[u32],
    symbols: &SymbolTable,
    pc_base: u32,
    profile: &BTreeMap<u32, u64>,
    start_idx: usize,
    end_idx: usize,
    fn_total: u64,
    prefix: &str,
) -> String {
    let mut out = String::new();
    let max = profile
        .iter()
        .filter_map(|(&pc, &c)| {
            let i = pc.checked_sub(pc_base)? / 4;
            let i = i as usize;
            if i >= start_idx && i < end_idx {
                Some(c)
            } else {
                None
            }
        })
        .max()
        .unwrap_or(0);

    for i in start_idx..end_idx.min(instructions.len()) {
        let addr = pc_base + (i as u32) * 4;
        if i != start_idx {
            if let Some(names) = symbols.table().get(&addr) {
                for name in names {
                    if name.starts_with('$') && name.len() <= 2 {
                        continue;
                    }
                    let d = format!("{:#}", rustc_demangle::demangle(name));
                    let _ = write!(out, "<span class=\"sym\">{}</span>", html_escape(&d));
                }
            }
        }
        let insn = instructions[i];
        let asm = disasm(insn, addr);
        let escaped = html_escape(&asm);
        let target = resolve_target(instructions, i, pc_base);
        let with_links = if let Some(t) = target {
            // Reuse a minimal target-link formatter (don't pull in the diff one).
            let ts = format!("0x{t:08x}");
            if escaped.contains(&ts) {
                escaped.replace(
                    &ts,
                    &format!("<a class=\"jt\" href=\"#{prefix}{t:08x}\">{ts}</a>"),
                )
            } else {
                escaped
            }
        } else {
            escaped
        };

        let count = profile.get(&addr).copied().unwrap_or(0);
        let frac_global = if max == 0 { 0.0 } else { count as f64 / max as f64 };
        let style = heat_color(frac_global);
        let count_disp = if count == 0 {
            String::from("<span class=\"cz\">·</span>")
        } else {
            fmt_count(count)
        };
        let pct_fn = if fn_total == 0 {
            String::new()
        } else {
            format!(
                "<span class=\"pct\">{}</span>",
                fmt_pct(count, fn_total)
            )
        };

        let _ = write!(
            out,
            "<span id=\"{prefix}{addr:08x}\" class=\"cl\" style=\"{style}\">\
             <span class=\"cnt\">{count_disp}</span>\
             {pct_fn}\
             <span class=\"ad\">{addr:08x}</span>  \
             <span class=\"by\">{insn:08x}</span>  \
             {with_links}</span>"
        );
    }
    out
}

/// Render the profile HTML report.
pub fn generate_profile_html(
    instructions: &[u32],
    symbols: &SymbolTable,
    pc_base: u32,
    profile_path: &Path,
    output_path: &str,
) {
    let profile = parse_profile_csv(profile_path);

    let total_samples: u64 = profile.values().sum();
    let unique_pcs = profile.values().filter(|&&c| c > 0).count();
    let (top_pc, top_count) = profile
        .iter()
        .max_by_key(|(_, &c)| c)
        .map(|(&pc, &c)| (pc, c))
        .unwrap_or((0, 0));

    let mut rows = build_function_rows(instructions, symbols, pc_base, &profile);
    let attributed: u64 = rows.iter().map(|r| r.total).sum();
    rows.sort_by(|a, b| b.total.cmp(&a.total).then(a.addr.cmp(&b.addr)));

    let mut h = String::with_capacity(8 * 1024 * 1024);
    let prefix = "p_";

    h.push_str(r##"<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<title>ELF Profile Report</title>
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{background:#0d1117;color:#c9d1d9;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;line-height:1.5}
.ctr{max-width:1800px;margin:0 auto;padding:20px}
header{background:#161b22;padding:20px 24px;border:1px solid #30363d;border-radius:6px;margin-bottom:16px}
h1{font-size:20px;margin-bottom:12px;color:#f0f6fc}
.stats{display:flex;gap:16px;flex-wrap:wrap}
.st{background:#21262d;padding:8px 16px;border-radius:6px;border:1px solid #30363d}
.st b{color:#58a6ff;font-size:18px}.st small{color:#8b949e;display:block;font-size:11px}
h2{font-size:16px;color:#f0f6fc;margin:24px 0 12px;padding-bottom:8px;border-bottom:1px solid #21262d}
.toolbar{display:flex;gap:8px;padding:8px 12px;background:#21262d;border:1px solid #30363d;border-radius:6px;margin-bottom:8px;align-items:center;flex-wrap:wrap;position:sticky;top:0;z-index:50}
.toolbar input{background:#0d1117;color:#c9d1d9;border:1px solid #30363d;border-radius:4px;padding:4px 8px;font-size:12px;font-family:inherit;flex:1;min-width:200px}
.toolbar button{background:#30363d;color:#c9d1d9;border:1px solid #484f58;border-radius:4px;padding:4px 12px;font-size:12px;cursor:pointer;font-family:inherit}
.toolbar button:hover{background:#484f58}
.toolbar .info{color:#8b949e;font-size:11px;margin-left:auto}
table{width:100%;border-collapse:collapse;font-size:12px;font-family:'SFMono-Regular',Consolas,monospace;background:#161b22;border:1px solid #30363d;border-radius:6px;overflow:hidden}
th{text-align:left;padding:6px 12px;background:#21262d;border-bottom:1px solid #30363d;color:#8b949e;font-size:11px;font-weight:600;cursor:pointer;user-select:none;position:sticky;top:42px;z-index:5}
th:hover{color:#c9d1d9}
th.sort-asc::after{content:" ▲";color:#58a6ff}
th.sort-desc::after{content:" ▼";color:#58a6ff}
td{padding:4px 12px;border-bottom:1px solid #21262d;vertical-align:top}
td.num{text-align:right;font-variant-numeric:tabular-nums}
tr:hover td{background:rgba(136,198,255,.04)}
td a.fn{color:#ffa657;text-decoration:none;cursor:pointer}
td a.fn:hover{text-decoration:underline}
.bar{position:relative;height:14px;background:#0d1117;border-radius:2px;overflow:hidden;min-width:80px}
.bar>span{position:absolute;left:0;top:0;bottom:0;background:linear-gradient(90deg,#388bfd,#58a6ff)}
details{background:#161b22;border:1px solid #30363d;border-radius:6px;margin-bottom:6px}
details>summary{padding:8px 16px;cursor:pointer;user-select:none;list-style:none;display:flex;justify-content:space-between;align-items:center;gap:12px}
details>summary::-webkit-details-marker{display:none}
details[open]>summary{background:#21262d;border-bottom:1px solid #30363d}
details>summary .fn-name{color:#ffa657;font-weight:600;font-family:'SFMono-Regular',Consolas,monospace;font-size:12px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;flex:1;min-width:0}
details>summary .fn-meta{color:#8b949e;font-size:11px;font-variant-numeric:tabular-nums;white-space:nowrap}
.code{font-family:'SFMono-Regular',Consolas,'Liberation Mono',Menlo,monospace;font-size:12px;line-height:1.35;padding:6px 0;white-space:pre;overflow-x:auto;max-height:80vh}
.cl{padding:0 12px;display:block}
.ad{color:#484f58;user-select:none}.by{color:#484f58;user-select:none}
.cnt{display:inline-block;width:14ch;text-align:right;color:#c9d1d9;user-select:none;padding-right:8px}
.pct{display:inline-block;width:8ch;text-align:right;color:#8b949e;user-select:none;padding-right:12px}
.cz{color:#30363d}
.sym{color:#ffa657;font-weight:600;display:block;padding:0 12px;margin-top:4px}
a.jt{color:#58a6ff;text-decoration:none}a.jt:hover{text-decoration:underline}
.muted{color:#8b949e}
</style>
<script>
function filterRows(){
 var q=document.getElementById('flt').value.toLowerCase();
 var rows=document.querySelectorAll('#fnTable tbody tr');
 var n=0;
 rows.forEach(function(r){
  var name=r.getAttribute('data-name')||'';
  var show=q===''||name.indexOf(q)!==-1;
  r.style.display=show?'':'none';
  if(show)n++;
 });
 document.getElementById('cnt').textContent=n+' / '+rows.length+' functions';
}
function sortTable(col,numeric){
 var tbody=document.querySelector('#fnTable tbody');
 var rows=Array.from(tbody.querySelectorAll('tr'));
 var ths=document.querySelectorAll('#fnTable th');
 var th=ths[col];
 var asc=!th.classList.contains('sort-desc');
 ths.forEach(function(x){x.classList.remove('sort-asc','sort-desc')});
 th.classList.add(asc?'sort-desc':'sort-asc');
 rows.sort(function(a,b){
  var av=a.children[col].getAttribute('data-v');
  var bv=b.children[col].getAttribute('data-v');
  if(numeric){av=parseFloat(av)||0;bv=parseFloat(bv)||0;return asc?bv-av:av-bv}
  return asc?bv.localeCompare(av):av.localeCompare(bv);
 });
 rows.forEach(function(r){tbody.appendChild(r)});
}
function jumpTo(id){
 var d=document.getElementById('det_'+id);
 if(d){d.open=true;d.scrollIntoView({behavior:'smooth',block:'start'});
  var el=document.getElementById('p_'+id);if(el)el.style.outline='2px solid #58a6ff';
  setTimeout(function(){if(el)el.style.outline=''},2000);
 }
}
function expandAllFns(){document.querySelectorAll('#detList details').forEach(function(d){d.open=true})}
function collapseAllFns(){document.querySelectorAll('#detList details').forEach(function(d){d.open=false})}
</script>
</head><body>
<div class="ctr">
"##);

    // Header
    let _ = write!(
        h,
        r##"<header>
<h1>ELF Profile Report</h1>
<div class="stats">
 <div class="st"><b>{}</b><small>total samples</small></div>
 <div class="st"><b>{}</b><small>unique PCs hit</small></div>
 <div class="st"><b>{}</b><small>functions hit</small></div>
 <div class="st"><b>0x{:08x}</b><small>hottest PC ({} samples, {} of total)</small></div>
 <div class="st"><b>{}</b><small>attributed to functions ({})</small></div>
</div>
</header>
"##,
        fmt_count(total_samples),
        fmt_count(unique_pcs as u64),
        fmt_count(rows.iter().filter(|r| r.total > 0).count() as u64),
        top_pc,
        fmt_count(top_count),
        fmt_pct(top_count, total_samples),
        fmt_count(attributed),
        fmt_pct(attributed, total_samples),
    );

    // Function table
    h.push_str(r##"<h2>Functions (sorted by samples)</h2>
<div class="toolbar">
 <input id="flt" type="text" placeholder="Filter functions by name…" oninput="filterRows()">
 <button onclick="expandAllFns()">Expand all</button>
 <button onclick="collapseAllFns()">Collapse all</button>
 <span class="info" id="cnt"></span>
</div>
<table id="fnTable">
<thead><tr>
 <th onclick="sortTable(0,true)" class="sort-desc">Samples</th>
 <th onclick="sortTable(1,true)">% total</th>
 <th onclick="sortTable(2,true)">cum %</th>
 <th onclick="sortTable(3,false)">Address</th>
 <th onclick="sortTable(4,true)">Insns</th>
 <th onclick="sortTable(5,true)">Hits</th>
 <th onclick="sortTable(6,false)">Function</th>
</tr></thead><tbody>
"##);

    let mut cum: u64 = 0;
    for r in &rows {
        cum = cum.saturating_add(r.total);
        let pct = if total_samples == 0 {
            0.0
        } else {
            (r.total as f64 / total_samples as f64) * 100.0
        };
        let cum_pct = if total_samples == 0 {
            0.0
        } else {
            (cum as f64 / total_samples as f64) * 100.0
        };
        let id = format!("{:08x}", r.addr);
        let _ = write!(
            h,
            "<tr data-name=\"{name_lc}\">\
              <td class=\"num\" data-v=\"{tot}\">{tot_s}</td>\
              <td class=\"num\" data-v=\"{pct:.6}\"><div class=\"bar\"><span style=\"width:{barw:.2}%\"></span></div><span class=\"muted\"> {pct:.3}%</span></td>\
              <td class=\"num\" data-v=\"{cum_pct:.6}\">{cum_pct:.2}%</td>\
              <td data-v=\"{addr_v}\"><span class=\"muted\">0x</span>{id}</td>\
              <td class=\"num\" data-v=\"{ic}\">{ic}</td>\
              <td class=\"num\" data-v=\"{hc}\">{hc}</td>\
              <td data-v=\"{name_attr}\"><a class=\"fn\" onclick=\"jumpTo('{id}')\">{name_html}</a></td>\
             </tr>\n",
            name_lc = html_escape(&r.name.to_lowercase()),
            tot = r.total,
            tot_s = fmt_count(r.total),
            pct = pct,
            barw = pct.min(100.0).max(0.0),
            cum_pct = cum_pct,
            addr_v = r.addr,
            id = id,
            ic = r.instr_count,
            hc = r.hit_count,
            name_attr = html_escape(&r.name),
            name_html = html_escape(&r.name),
        );
        let _ = r.end_addr; // silence unused
        let _ = r.hottest_pc;
        let _ = r.hottest_count;
    }
    h.push_str("</tbody></table>\n");

    // Per-function details: only render functions with at least one sample
    // (otherwise the page balloons to many MBs).
    h.push_str("<h2>Function bodies (hot functions)</h2><div id=\"detList\">\n");
    let mut details_count = 0;
    for r in &rows {
        if r.total == 0 {
            continue;
        }
        details_count += 1;
        let id = format!("{:08x}", r.addr);
        let start_idx = ((r.addr - pc_base) / 4) as usize;
        let end_idx = ((r.end_addr - pc_base) / 4) as usize;

        let pct = if total_samples == 0 {
            0.0
        } else {
            (r.total as f64 / total_samples as f64) * 100.0
        };

        let _ = write!(
            h,
            "<details id=\"det_{id}\"><summary>\
              <span class=\"fn-name\">{name}</span>\
              <span class=\"fn-meta\">{samples} samples ({pct:.3}%) · {hits}/{insns} hit · 0x{id}</span>\
             </summary><div class=\"code\">",
            id = id,
            name = html_escape(&r.name),
            samples = fmt_count(r.total),
            pct = pct,
            hits = r.hit_count,
            insns = r.instr_count,
        );
        h.push_str(&render_function_body(
            instructions,
            symbols,
            pc_base,
            &profile,
            start_idx,
            end_idx,
            r.total,
            prefix,
        ));
        h.push_str("</div></details>\n");
    }
    if details_count == 0 {
        h.push_str("<p class=\"muted\">No function received any samples.</p>\n");
    }
    h.push_str("</div></div>\n<script>filterRows()</script>\n</body></html>\n");

    fs::write(output_path, h.as_bytes()).unwrap_or_else(|e| {
        eprintln!("Error writing {output_path}: {e}");
        process::exit(1);
    });
    println!(
        "Wrote profile HTML report to {output_path} ({} functions, {} with samples)",
        rows.len(),
        details_count
    );
}

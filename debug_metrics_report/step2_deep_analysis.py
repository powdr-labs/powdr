#!/usr/bin/env python3
"""
Step 2: Deep analysis of the discrepancies.
Investigate what 'cells' actually counts vs what the APC analyzer counts.
"""

import json
import re
from collections import defaultdict

with open("metrics_v2_pairing_combined.json") as f:
    metrics_data = json.load(f)
with open("apc_candidates_pairing_v2.json") as f:
    apc_data = json.load(f)

exp = metrics_data["metrics_v2_pairing_apc000"]

# ============================================================
# 1. Check the label structure of different metrics
# ============================================================

print("=" * 70)
print("LABEL STRUCTURE OF COUNTER METRICS")
print("=" * 70)

for metric_name in ["cells", "rows", "main_cols", "prep_cols", "perm_cols",
                     "total_cells", "constraints", "interactions"]:
    entries = [c for c in exp["counter"] if c["metric"] == metric_name]
    if entries:
        label_keys = [sorted([k for k, v in e["labels"]]) for e in entries[:3]]
        print(f"\n  {metric_name}: {len(entries)} entries")
        print(f"    Label keys: {label_keys[0]}")
        print(f"    Example: {dict(entries[0]['labels'])}, value={entries[0]['value']}")

# ============================================================
# 2. For a specific AIR, look at ALL metric entries
# ============================================================

print()
print("=" * 70)
print("ALL ENTRIES FOR BaseAluCoreAir (air_id=18)")
print("=" * 70)

for c in exp["counter"]:
    labels = dict(c["labels"])
    if labels.get("air_id") == "18":
        print(f"  metric={c['metric']:<20} value={c['value']:<15} labels={labels}")

# ============================================================
# 3. Check main_cols entries specifically - do they have air_id?
# ============================================================

print()
print("=" * 70)
print("MAIN_COLS ENTRIES (first 10)")
print("=" * 70)

main_cols_entries = [c for c in exp["counter"] if c["metric"] == "main_cols"]
for c in main_cols_entries[:10]:
    labels = dict(c["labels"])
    print(f"  air_id={labels.get('air_id','N/A'):<5} air_name={labels.get('air_name','N/A'):<60} segment={labels.get('segment','N/A'):<5} value={c['value']}")

# ============================================================
# 4. Compute per-AIR column widths from per-segment data
# ============================================================

print()
print("=" * 70)
print("PER-AIR WIDTH: main + prep + perm cols")
print("=" * 70)

app_entries = []
for c in exp["counter"] + exp["gauge"]:
    labels = dict(c["labels"])
    if labels.get("group", "").startswith("app_proof"):
        labels["metric"] = c["metric"]
        labels["value"] = c["value"]
        app_entries.append(labels)

# Get per-AIR column widths (they should be constant across segments for a given AIR)
air_widths = {}
for e in app_entries:
    aid = e.get("air_id", "?")
    if aid not in air_widths:
        air_widths[aid] = {"name": e.get("air_name", "?"), "main": set(), "prep": set(), "perm": set()}
    if e["metric"] == "main_cols":
        air_widths[aid]["main"].add(float(e["value"]))
    elif e["metric"] == "prep_cols":
        air_widths[aid]["prep"].add(float(e["value"]))
    elif e["metric"] == "perm_cols":
        air_widths[aid]["perm"].add(float(e["value"]))

# Compute cells, rows per AIR
air_cells = defaultdict(float)
air_rows = defaultdict(float)
air_rows_per_seg = defaultdict(list)
for e in app_entries:
    aid = e.get("air_id", "?")
    if e["metric"] == "cells":
        air_cells[aid] += float(e["value"])
    elif e["metric"] == "rows":
        air_rows[aid] += float(e["value"])
        air_rows_per_seg[aid].append((e.get("segment", "?"), float(e["value"])))

print(f"{'AIR':<55} {'MainC':>6} {'PrepC':>6} {'PermC':>6} {'Total':>6} {'cells/rows':>10}")
print("-" * 100)
for aid in sorted(air_widths.keys(), key=lambda x: air_cells.get(x, 0), reverse=True):
    info = air_widths[aid]
    mc = list(info["main"])[0] if info["main"] else 0
    pc = list(info["prep"])[0] if info["prep"] else 0
    pmc = list(info["perm"])[0] if info["perm"] else 0
    total = mc + pc + pmc
    eff = air_cells[aid] / air_rows[aid] if air_rows[aid] > 0 else 0
    name = info["name"][:54]
    print(f"{name:<55} {mc:>6.0f} {pc:>6.0f} {pmc:>6.0f} {total:>6.0f} {eff:>10.1f}")

# ============================================================
# 5. Key finding: cells/rows = main + prep + perm for each AIR
# ============================================================

print()
print("=" * 70)
print("VERIFICATION: cells/rows == main + prep + perm ?")
print("=" * 70)

for aid in sorted(air_widths.keys(), key=lambda x: air_cells.get(x, 0), reverse=True):
    info = air_widths[aid]
    mc = list(info["main"])[0] if info["main"] else 0
    pc = list(info["prep"])[0] if info["prep"] else 0
    pmc = list(info["perm"])[0] if info["perm"] else 0
    total = mc + pc + pmc
    eff = air_cells[aid] / air_rows[aid] if air_rows[aid] > 0 else 0
    if air_rows[aid] > 0:
        match = "OK" if abs(eff - total) < 0.01 else f"MISMATCH (diff={eff-total:.1f})"
        if abs(eff - total) >= 0.01:
            print(f"  air_id={aid} {info['name'][:50]}: cells/rows={eff:.1f}, total_cols={total:.0f}  {match}")

print("  (Only mismatches shown. If nothing printed, all match.)")

# ============================================================
# 6. Quantify each source of discrepancy for trace cells
# ============================================================

print()
print("=" * 70)
print("DISCREPANCY DECOMPOSITION: TRACE CELLS")
print("=" * 70)

# APC analyzer: sum(main_cols_before * exec_freq) = 1.54B
# Metrics viewer: sum over all AIRs of cells = 5.77B

# Factor 1: prep + perm columns
# The APC analyzer only counts main_cols. The viewer counts main + prep + perm.
# For instruction AIRs, what fraction of cells come from non-main columns?

def is_normal_instruction_air(name):
    m = re.match(r"^VmAirWrapper<[^,]+,\s*([^>]+?)(?:<(\d+)(?:,\s*\d+)*>)?\s*>$", name or "")
    if not m:
        return False
    if m.group(1) == "FieldExpressionCoreAir":
        return False
    if m.group(2) and int(m.group(2)) != 4:
        return False
    return True

instr_main_cells = 0
instr_total_cells = 0
instr_rows_total = 0
for aid in air_widths:
    info = air_widths[aid]
    if is_normal_instruction_air(info["name"]):
        mc = list(info["main"])[0] if info["main"] else 0
        total = mc + (list(info["prep"])[0] if info["prep"] else 0) + (list(info["perm"])[0] if info["perm"] else 0)
        instr_main_cells += mc * air_rows[aid]
        instr_total_cells += air_cells[aid]
        instr_rows_total += air_rows[aid]

apcs = apc_data["apcs"]
apc_software_cells = sum(a["cost_before"] * a["execution_frequency"] for a in apcs)
apc_total_exec_freq = sum(a["execution_frequency"] for a in apcs)

print(f"\n  APC analyzer software cells:                     {apc_software_cells:>14,.0f}  (1.00x)")
print(f"  Instr AIR rows * main_cols (viewer):             {instr_main_cells:>14,.0f}  ({instr_main_cells/apc_software_cells:.2f}x)")
print(f"  Instr AIR total cells (main+prep+perm, viewer):  {instr_total_cells:>14,.0f}  ({instr_total_cells/apc_software_cells:.2f}x)")
print(f"  All AIR total cells (viewer):                    {5766672260:>14,.0f}  ({5766672260/apc_software_cells:.2f}x)")

# Factor 2: Padding - rows in metrics vs execution frequency
# The rows in the metrics are padded to powers of 2
print(f"\n  Instruction AIR rows (viewer, padded):            {instr_rows_total:>14,.0f}")
print(f"  APC total execution frequency:                   {apc_total_exec_freq:>14,.0f}")
print(f"  Ratio:                                           {instr_rows_total/apc_total_exec_freq:.2f}x")

# ============================================================
# 7. Per-AIR: rows vs what we'd expect from execution frequency
# ============================================================

print()
print("=" * 70)
print("PER-AIR: ROWS BREAKDOWN BY SEGMENT (top instruction AIRs)")
print("=" * 70)

for aid in sorted(air_widths.keys(), key=lambda x: air_cells.get(x, 0), reverse=True)[:10]:
    info = air_widths[aid]
    if not is_normal_instruction_air(info["name"]):
        continue
    segs = sorted(air_rows_per_seg[aid], key=lambda x: x[0])
    mc = list(info["main"])[0] if info["main"] else 0
    total_width = mc + (list(info["prep"])[0] if info["prep"] else 0) + (list(info["perm"])[0] if info["perm"] else 0)
    print(f"\n  {info['name'][:70]}")
    print(f"    main_cols={mc:.0f}, total_cols={total_width:.0f}, total_rows={air_rows[aid]:,.0f}")
    for seg, rows in segs:
        # Is rows a power of 2?
        is_pow2 = rows > 0 and (int(rows) & (int(rows) - 1)) == 0
        print(f"    segment {seg}: rows={rows:>12,.0f}  {'(power of 2)' if is_pow2 else ''}")

# ============================================================
# 8. What are the APC analyzer costs really?
# ============================================================

print()
print("=" * 70)
print("APC ANALYZER: WHAT DOES cost_before / main_columns REPRESENT?")
print("=" * 70)

# Check relationship between cost_before and stats
for a in apcs[:5]:
    print(f"  cost_before={a['cost_before']}, width_before={a['width_before']}, "
          f"stats.before.main_columns={a['stats']['before']['main_columns']}")
    print(f"    -> cost_before == main_columns: {a['cost_before'] == a['stats']['before']['main_columns']}")

# ============================================================
# 9. Quantify: how many instruction executions does each AIR correspond to?
# ============================================================

print()
print("=" * 70)
print("INSTRUCTION MAPPING: Which AIRs handle which instructions?")
print("=" * 70)

# From the APC data, we can see instruction types (ADD, STOREW, LOADW, etc.)
# Count instruction types and their frequencies
instr_type_freq = defaultdict(int)
for a in apcs:
    for block in a["original_blocks"]:
        for instr in block["instructions"]:
            # Extract instruction type (first word)
            itype = instr.split()[0]
            instr_type_freq[itype] += a["execution_frequency"]

print(f"{'Instruction':>15} {'Exec Frequency':>15}")
print("-" * 35)
for itype, freq in sorted(instr_type_freq.items(), key=lambda x: -x[1])[:20]:
    print(f"  {itype:>13} {freq:>15,}")

total_instr_execs = sum(instr_type_freq.values())
print(f"\n  Total instruction executions from APCs: {total_instr_execs:,}")

# Compare with AIR rows
# BaseAluCoreAir handles ADD, SUB, etc. → has 18.87M rows
# LoadStoreCoreAir handles LOADW, STOREW → has 9.44M rows
# MulHCoreAir handles MULHU → has 4.46M rows
# etc.

# Instruction types → expected AIR mapping
print(f"\n  Expected row counts from instruction frequencies:")
add_freq = instr_type_freq.get("ADD", 0) + instr_type_freq.get("SUB", 0)
print(f"    ADD+SUB → BaseAluCoreAir:       {add_freq:>12,}  (viewer rows: 18,874,368)")
load_freq = instr_type_freq.get("LOADW", 0) + instr_type_freq.get("STOREW", 0)
print(f"    LOADW+STOREW → LoadStoreCoreAir: {load_freq:>12,}  (viewer rows: 9,437,184)")
sltu_freq = instr_type_freq.get("SLTU", 0) + instr_type_freq.get("SLT", 0)
print(f"    SLTU+SLT → LessThanCoreAir:     {sltu_freq:>12,}  (viewer rows: 9,437,184)")
mulh_freq = instr_type_freq.get("MULHU", 0) + instr_type_freq.get("MULHSU", 0)
print(f"    MULHU → MulHCoreAir:             {mulh_freq:>12,}  (viewer rows: 4,456,448)")
mul_freq = instr_type_freq.get("MUL", 0)
print(f"    MUL → MultiplicationCoreAir:     {mul_freq:>12,}  (viewer rows: 4,456,448)")
beq_freq = instr_type_freq.get("BEQ", 0) + instr_type_freq.get("BNE", 0)
print(f"    BEQ+BNE → BranchEqualCoreAir:   {beq_freq:>12,}  (viewer rows: 2,359,296)")

# ============================================================
# 10. Understand the padding factor
# ============================================================

print()
print("=" * 70)
print("PADDING FACTOR ANALYSIS")
print("=" * 70)

# For each AIR, if we know the actual instruction count and the padded rows,
# we can compute the padding factor.
# But we don't know the exact mapping. Let's at least check the padding.
# Rows are padded to next power of 2 per segment.
# With 5 segments, rows are spread across them.

# Let's check: total instruction executions vs total rows
print(f"\n  Total instruction executions (APC analyzer):     {total_instr_execs:>14,}")
print(f"  Total instruction AIR rows (metrics viewer):     {instr_rows_total:>14,}")
print(f"  Padding factor (rows / instructions):            {instr_rows_total / total_instr_execs:.2f}x")

# Let's check per-AIR padding
print(f"\n  Per-AIR padding (using ADD+SUB → BaseAluCoreAir as example):")
print(f"    Actual ADD+SUB executions:    {add_freq:>12,}")
print(f"    Rows in metrics (5 segments): {18874368:>12,}")
print(f"    Average rows per segment:     {18874368/5:>12,.0f}")
print(f"    Padding factor:               {18874368 / add_freq:.2f}x")

# ============================================================
# 11. Width comparison: APC main_columns vs total width in metrics
# ============================================================

print()
print("=" * 70)
print("WIDTH COMPARISON: APC main_cols vs metrics total width")
print("=" * 70)

# The APC analyzer's main_columns for ADD is the width of the ADD instruction
# in the BaseAlu chip. In the metrics, the BaseAlu chip has main_cols=40 but
# total (main+prep+perm) = 116.
# So the APC's main_columns for an ADD should be something that sums to 40.

# Let's check: what main_columns does the APC analyzer report for ADD instructions?
add_widths = set()
for a in apcs:
    for block in a["original_blocks"]:
        for instr in block["instructions"]:
            if instr.startswith("ADD "):
                # This is part of a block, the width covers the whole block
                pass

# Actually, the APC's main_columns is per-block, not per-instruction.
# Let's check for single-instruction blocks
single_instr_blocks = [a for a in apcs
                       if len(a["original_blocks"]) == 1
                       and len(a["original_blocks"][0]["instructions"]) == 1]
print(f"\n  Single-instruction APC blocks: {len(single_instr_blocks)}")
for a in single_instr_blocks[:10]:
    instr = a["original_blocks"][0]["instructions"][0]
    itype = instr.split()[0]
    mc = a["stats"]["before"]["main_columns"]
    print(f"    {itype:>10}: main_columns={mc}")

# For multi-instruction blocks, the width is the sum of per-instruction widths
# Let's check if the total is consistent
print(f"\n  Multi-instruction block width examples:")
for a in apcs[:5]:
    n_instrs = sum(len(b["instructions"]) for b in a["original_blocks"])
    mc = a["stats"]["before"]["main_columns"]
    print(f"    {n_instrs} instructions: main_columns={mc} (avg per instr: {mc/n_instrs:.1f})")

# ============================================================
# 12. Total width in APC analyzer vs metrics
# ============================================================

print()
print("=" * 70)
print("TOTAL WIDTH: APC sum of (main_columns * freq) / exec_freq")
print("=" * 70)

# weighted average width per instruction execution
apc_weighted_width = sum(a["stats"]["before"]["main_columns"] * a["execution_frequency"] for a in apcs)
print(f"  APC sum(main_cols * freq):     {apc_weighted_width:>14,}")
print(f"  APC total exec freq:           {apc_total_exec_freq:>14,}")
print(f"  APC avg main_cols per exec:    {apc_weighted_width / apc_total_exec_freq:.1f}")

# But this isn't per instruction - it's per APC execution (basic block)
# Each block has multiple instructions, which go to different AIRs
# So main_columns is the SUM of widths across all instructions in the block

# The total "software cells" from APC = sum(main_cols_before * freq)
# This should approximate sum over all instruction AIRs of (main_cols * actual_rows)
# i.e., instruction cells counted by main columns only, with actual row counts

# Let's compute that from the metrics side:
# For each instruction AIR: main_cols * total_rows_if_unpadded
# We don't know unpadded rows directly, but we can estimate from instruction frequencies

print()
print("=" * 70)
print("FINAL DECOMPOSITION")
print("=" * 70)

# Start with APC number: 1.54B
# Factor 1: APC doesn't count prep+perm columns
# For instruction AIRs in metrics:
#   main cells = sum(main_cols * rows) [with padded rows]
#   total cells = sum((main+prep+perm) * rows) [with padded rows]
print(f"\n  Starting point: APC software cells = {apc_software_cells:,.0f} ({apc_software_cells/1e9:.2f}B)")

# The APC analyzer uses actual execution count, not padded rows.
# main_cols in APC = main_cols in metrics (they're the same column count)
# But APC uses exec_freq while metrics uses padded rows.

# Factor 1: Padding
# If there were no padding, instr AIR cells would be:
#   sum(total_width * actual_instruction_count)
# With padding:
#   sum(total_width * padded_rows)

# Factor 2: Column types
# APC counts main_cols only
# Metrics counts main + prep + perm

# Factor 3: Non-instruction AIRs
non_instr_cells = 5766672260 - instr_total_cells
print(f"\n  Non-instruction AIR cells:     {non_instr_cells:>14,.0f}  ({non_instr_cells/5766672260*100:.1f}% of total)")

# Factor 4: The APC main_columns is the width of the WHOLE block
# which equals the sum of per-instruction main_columns
# This should match the sum of main_cols across instruction AIRs
# weighted by actual execution counts.

# Let's verify: APC software cells should ≈ sum over instr AIRs of (main_cols * unpadded_rows)
# We know padded rows, and we know instruction frequencies from APCs.

# For BaseAluCoreAir (air_id=18): handles ADD
#   main_cols = 40
#   padded_rows = 18,874,368
#   actual ADD executions (from APCs) = add_freq ≈ some number
#   But each ADD in a block contributes 1 row to this AIR

# So:
#   APC main_cols for ADD ≈ 40 (the main_cols of BaseAluCoreAir)
#   APC uses: 40 * add_freq_from_apcs
#   Metrics uses: 40 * padded_rows(BaseAluCoreAir)
#   The ratio is padded_rows / add_freq

print(f"\n  Decomposition of 3.74x ratio:")
print(f"    = (prep+perm columns factor) * (padding factor) * (non-instruction AIR factor)")

# Compute prep+perm factor for instruction AIRs
prep_perm_factor = instr_total_cells / instr_main_cells if instr_main_cells > 0 else 0
padding_factor = instr_main_cells / apc_software_cells if apc_software_cells > 0 else 0
non_instr_factor = 5766672260 / instr_total_cells if instr_total_cells > 0 else 0

print(f"    Prep+perm columns factor:    {prep_perm_factor:.3f}x  (instr cells / instr main-only cells)")
print(f"    Padding+other factor:        {padding_factor:.3f}x  (instr main cells [padded] / APC software cells)")
print(f"    Non-instruction AIR factor:  {non_instr_factor:.3f}x  (all cells / instr cells)")
print(f"    Product:                     {prep_perm_factor * padding_factor * non_instr_factor:.3f}x")
print(f"    Actual ratio:                {5766672260 / apc_software_cells:.3f}x")

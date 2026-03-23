#!/usr/bin/env python3
"""
Step 1: Independently compute the numbers shown in both viewers and confirm
Georg's observations about the discrepancies.
"""

import json
import sys
from collections import defaultdict

# ============================================================
# Load data
# ============================================================

with open("metrics_v2_pairing_combined.json") as f:
    metrics_data = json.load(f)

with open("apc_candidates_pairing_v2.json") as f:
    apc_data = json.load(f)

# We focus on the software-only run (apc000 = no autoprecompiles)
exp = metrics_data["metrics_v2_pairing_apc000"]

# ============================================================
# Part A: Recompute Metrics Viewer numbers for apc000
# (Using the same logic as spec.py / index.html)
# ============================================================

# Flatten entries
entries = []
for c in exp["counter"] + exp["gauge"]:
    obj = dict(c["labels"])
    obj["metric"] = c["metric"]
    obj["value"] = c["value"]
    entries.append(obj)

# Split by group
app = [e for e in entries if e.get("group", "").startswith("app_proof")]

# Trace cells = sum of total_cells for app group
app_total_cells = sum(float(e["value"]) for e in app if e["metric"] == "total_cells")

# Rows and segments by AIR (for weighted sums)
segments_by_air = defaultdict(int)
rows_by_air = defaultdict(float)
for e in app:
    if e["metric"] == "rows":
        segments_by_air[e["air_id"]] += 1
        rows_by_air[e["air_id"]] += float(e["value"])

# Constraint instances = sum(constraints_per_air * rows_per_air)
# Bus interaction messages = sum(interactions_per_air * rows_per_air)
constraint_instances = 0
bus_interaction_messages = 0
for e in entries:
    if e["metric"] == "constraints":
        constraint_instances += float(e["value"]) * rows_by_air.get(e["air_id"], 0)
    elif e["metric"] == "interactions":
        bus_interaction_messages += float(e["value"]) * rows_by_air.get(e["air_id"], 0)

# Also compute cells from per-AIR 'cells' metric (should match total_cells)
app_cells_per_air = sum(float(e["value"]) for e in app if e["metric"] == "cells")

print("=" * 70)
print("METRICS VIEWER NUMBERS (recomputed from raw data)")
print("=" * 70)
print(f"  Trace cells (total_cells):     {app_total_cells:,.0f}  ({app_total_cells/1e9:.2f}B)")
print(f"  Trace cells (sum of cells):    {app_cells_per_air:,.0f}  ({app_cells_per_air/1e9:.2f}B)")
print(f"  Constraint instances:          {constraint_instances:,.0f}  ({constraint_instances/1e9:.2f}B)")
print(f"  Bus interaction messages:      {bus_interaction_messages:,.0f}  ({bus_interaction_messages/1e9:.2f}B)")

# ============================================================
# Part B: Recompute APC Analyzer numbers
# ============================================================

apcs = apc_data["apcs"]

# The APC analyzer "software cost" = sum(cost_before * execution_frequency)
# where cost_before = main_columns before optimization (= width_before)
total_software_cells = sum(a["cost_before"] * a["execution_frequency"] for a in apcs)

# For constraints: sum(stats.before.constraints * execution_frequency)
total_software_constraints = sum(a["stats"]["before"]["constraints"] * a["execution_frequency"] for a in apcs)

# For bus interactions: sum(stats.before.bus_interactions * execution_frequency)
total_software_bus = sum(a["stats"]["before"]["bus_interactions"] * a["execution_frequency"] for a in apcs)

print()
print("=" * 70)
print("APC ANALYZER NUMBERS (recomputed from raw data)")
print("=" * 70)
print(f"  Software cost (trace cells):   {total_software_cells:,.0f}  ({total_software_cells/1e9:.2f}B)")
print(f"  Constraint instances:          {total_software_constraints:,.0f}  ({total_software_constraints/1e9:.2f}B)")
print(f"  Bus interaction messages:      {total_software_bus:,.0f}  ({total_software_bus/1e9:.2f}B)")

# ============================================================
# Part C: Show the ratios
# ============================================================

print()
print("=" * 70)
print("DISCREPANCY RATIOS (Metrics Viewer / APC Analyzer)")
print("=" * 70)
print(f"  Trace cells:                   {app_total_cells / total_software_cells:.2f}x")
print(f"  Constraint instances:          {constraint_instances / total_software_constraints:.2f}x")
print(f"  Bus interaction messages:      {bus_interaction_messages / total_software_bus:.2f}x")

# ============================================================
# Part D: Dig into what makes up the metrics viewer numbers
# ============================================================

print()
print("=" * 70)
print("METRICS VIEWER: BREAKDOWN BY AIR")
print("=" * 70)

# Collect cells, rows, constraints, interactions per AIR
air_info = {}
for e in entries:
    aid = e.get("air_id", "?")
    aname = e.get("air_name", "?")
    if aid not in air_info:
        air_info[aid] = {"name": aname, "cells": 0, "rows": 0, "constraints": 0,
                         "interactions": 0, "main_cols": 0, "prep_cols": 0, "perm_cols": 0,
                         "segments": 0}
    if e["metric"] == "cells" and e in app:
        air_info[aid]["cells"] += float(e["value"])
    if e["metric"] == "rows" and e in app:
        air_info[aid]["rows"] += float(e["value"])
        air_info[aid]["segments"] += 1
    if e["metric"] == "main_cols":
        air_info[aid]["main_cols"] = max(air_info[aid]["main_cols"], float(e["value"]))
    if e["metric"] == "prep_cols":
        air_info[aid]["prep_cols"] = max(air_info[aid]["prep_cols"], float(e["value"]))
    if e["metric"] == "perm_cols":
        air_info[aid]["perm_cols"] = max(air_info[aid]["perm_cols"], float(e["value"]))
    if e["metric"] == "constraints":
        air_info[aid]["constraints"] = float(e["value"])
    if e["metric"] == "interactions":
        air_info[aid]["interactions"] = float(e["value"])

# Sort by cells descending
sorted_airs = sorted(air_info.items(), key=lambda x: x[1]["cells"], reverse=True)

print(f"{'AIR ID':<8} {'Name':<55} {'Cells':>14} {'Rows':>12} {'Segs':>5} {'MainCols':>9} {'Constrs':>8} {'Interactions':>12}")
print("-" * 130)
total_rows = 0
for aid, info in sorted_airs[:30]:
    total_rows += info["rows"]
    name = info["name"][:54]
    print(f"{aid:<8} {name:<55} {info['cells']:>14,.0f} {info['rows']:>12,.0f} {info['segments']:>5} {info['main_cols']:>9.0f} {info['constraints']:>8.0f} {info['interactions']:>12.0f}")

print(f"\nTotal rows (all app AIRs): {total_rows:,.0f}")

# ============================================================
# Part E: Analyze total_cells vs cells
# ============================================================

print()
print("=" * 70)
print("TOTAL_CELLS ENTRIES (per-segment totals)")
print("=" * 70)

total_cells_entries = [e for e in app if e["metric"] == "total_cells"]
print(f"Number of total_cells entries: {len(total_cells_entries)}")
for e in total_cells_entries[:5]:
    print(f"  segment={e.get('segment', '?')}, value={float(e['value']):,.0f}")
if len(total_cells_entries) > 5:
    print(f"  ... ({len(total_cells_entries)} total entries)")
    total_tc = sum(float(e["value"]) for e in total_cells_entries)
    print(f"  Sum of all total_cells: {total_tc:,.0f}")

# ============================================================
# Part F: Check what fraction of cells come from instruction AIRs
# ============================================================

print()
print("=" * 70)
print("CELLS BY CATEGORY")
print("=" * 70)

import re

def is_normal_instruction_air(name):
    m = re.match(r"^VmAirWrapper<[^,]+,\s*([^>]+?)(?:<(\d+)(?:,\s*\d+)*>)?\s*>$", name or "")
    if not m:
        return False
    if m.group(1) == "FieldExpressionCoreAir":
        return False
    if m.group(2) and int(m.group(2)) != 4:
        return False
    return True

powdr_cells = 0
normal_instr_cells = 0
precompile_cells = 0
for aid, info in air_info.items():
    if info["name"].startswith("PowdrAir"):
        powdr_cells += info["cells"]
    elif is_normal_instruction_air(info["name"]):
        normal_instr_cells += info["cells"]
    else:
        precompile_cells += info["cells"]

total_cells_sum = powdr_cells + normal_instr_cells + precompile_cells
print(f"  Powdr AIR cells:              {powdr_cells:>14,.0f}  ({powdr_cells/total_cells_sum*100:.1f}%)")
print(f"  Normal instruction cells:     {normal_instr_cells:>14,.0f}  ({normal_instr_cells/total_cells_sum*100:.1f}%)")
print(f"  Other (precompile/non-instr): {precompile_cells:>14,.0f}  ({precompile_cells/total_cells_sum*100:.1f}%)")
print(f"  Sum:                          {total_cells_sum:>14,.0f}")

# ============================================================
# Part G: Understand padding - compare cells vs rows * columns
# ============================================================

print()
print("=" * 70)
print("PADDING ANALYSIS: cells vs rows * main_cols per AIR")
print("=" * 70)

total_cells_from_formula = 0
total_cells_actual = 0
padding_details = []
for aid, info in sorted_airs:
    if info["cells"] > 0 and info["rows"] > 0:
        # cells = rows * (main_cols + prep_cols + perm_cols), but rows is padded to next power of 2
        expected_cols = info["main_cols"] + info["prep_cols"] + info["perm_cols"]
        # Each segment has its own row count; 'cells' is the per-AIR cell count
        # The 'cells' metric = sum over segments of (padded_rows * total_cols)
        # The 'rows' metric = sum over segments of (padded_rows)
        # So cells / rows should give total_cols
        if info["rows"] > 0:
            effective_cols = info["cells"] / info["rows"]
            padding_details.append({
                "aid": aid,
                "name": info["name"],
                "cells": info["cells"],
                "rows": info["rows"],
                "effective_cols": effective_cols,
                "main_cols": info["main_cols"],
                "total_cols": expected_cols,
            })

# Show a few examples
print(f"{'AIR':<55} {'Cells':>14} {'Rows':>12} {'Eff.Cols':>10} {'MainCols':>10} {'TotalCols':>10}")
print("-" * 115)
for p in padding_details[:15]:
    name = p["name"][:54]
    print(f"{name:<55} {p['cells']:>14,.0f} {p['rows']:>12,.0f} {p['effective_cols']:>10.1f} {p['main_cols']:>10.0f} {p['total_cols']:>10.0f}")

# ============================================================
# Part H: Key insight - what does the APC analyzer count?
# The APC analyzer uses main_columns (= width_before) * execution_frequency.
# But cells in the metrics viewer = rows * (main_cols + prep_cols + perm_cols)
# And rows = padded to next power of 2 of actual execution rows.
# ============================================================

print()
print("=" * 70)
print("KEY COMPARISON: What does each tool count?")
print("=" * 70)
print("""
Metrics Viewer 'cells' metric:
  = sum over all AIRs, all segments of: padded_rows * (main_cols + prep_cols + perm_cols)
  This includes ALL AIRs, padding, and all column types.

APC Analyzer 'software cost' (trace cells):
  = sum over all APC candidates of: cost_before * execution_frequency
  where cost_before = main_columns (= width_before)
  This counts only main columns, only for instruction AIRs covered by APCs,
  and uses actual execution counts (no padding).
""")

# Let's check: does cells = rows * total_cols?
# (i.e., is 'rows' already padded or actual?)
# If cells / rows = total_cols exactly, then rows is the padded value.
print("Checking if cells = rows * total_cols (i.e., rows already padded):")
close_count = 0
for p in padding_details:
    ratio = p["cells"] / (p["rows"] * p["total_cols"]) if p["rows"] * p["total_cols"] > 0 else 0
    if 0.99 < ratio < 1.01:
        close_count += 1
print(f"  {close_count} / {len(padding_details)} AIRs have cells ≈ rows * total_cols")
print(f"  (This means 'rows' is the PADDED row count)")

# ============================================================
# Part I: Total rows in metrics viewer vs total execution freq in APC analyzer
# ============================================================

print()
print("=" * 70)
print("ROW COUNT COMPARISON")
print("=" * 70)

# Total rows across all instruction AIRs in metrics viewer
instr_rows = 0
instr_cells = 0
for aid, info in air_info.items():
    if is_normal_instruction_air(info["name"]):
        instr_rows += info["rows"]
        instr_cells += info["cells"]

# Total execution frequency across all APCs
total_exec_freq = sum(a["execution_frequency"] for a in apcs)

# Total instructions from APC candidates (each APC executes all its instructions)
total_instr_executions = sum(
    len(a["original_blocks"][0]["instructions"]) * a["execution_frequency"]
    for a in apcs
)

print(f"  Normal instruction AIR rows (metrics viewer, padded):  {instr_rows:>14,.0f}")
print(f"  Total APC execution frequency (APC analyzer):         {total_exec_freq:>14,.0f}")
print(f"  Total instruction executions (APC analyzer):          {total_instr_executions:>14,.0f}")
print(f"  Ratio (viewer rows / APC exec freq):                  {instr_rows / total_exec_freq:.2f}x")

print()
print("=" * 70)
print("SUMMARY OF OBSERVATIONS")
print("=" * 70)
print(f"""
Georg's observations confirmed:
  Metrics Viewer (apc000):
    Trace cells:              {app_total_cells/1e9:.2f}B
    Constraint instances:     {constraint_instances/1e9:.2f}B
    Bus interaction messages:  {bus_interaction_messages/1e9:.2f}B

  APC Analyzer (software cost):
    Trace cells:              {total_software_cells/1e9:.2f}B
    Constraint instances:     {total_software_constraints/1e9:.2f}B
    Bus interaction messages:  {total_software_bus/1e9:.2f}B

  Ratios:
    Trace cells:              {app_total_cells / total_software_cells:.2f}x
    Constraint instances:     {constraint_instances / total_software_constraints:.2f}x
    Bus interaction messages:  {bus_interaction_messages / total_software_bus:.2f}x
""")

#!/usr/bin/env python3
"""
Step 4: Investigate why cells/rows >> main_cols.
Hypothesis: cells includes interaction columns not reported in main_cols.
"""

import json
from collections import defaultdict

with open("metrics_v2_pairing_combined.json") as f:
    metrics_data = json.load(f)
with open("apc_candidates_pairing_v2.json") as f:
    apc_data = json.load(f)

exp = metrics_data["metrics_v2_pairing_apc000"]

# Flatten entries
entries = []
for c in exp["counter"] + exp["gauge"]:
    obj = dict(c["labels"])
    obj["metric"] = c["metric"]
    obj["value"] = c["value"]
    entries.append(obj)

app = [e for e in entries if e.get("group", "").startswith("app_proof")]

# Build per-AIR data using air_name (not air_id) to avoid collisions
app_air_names = set()
for e in app:
    if e.get("air_name"):
        app_air_names.add(e["air_name"])

air_data = {}
for e in app:
    name = e.get("air_name", "?")
    if name not in air_data:
        air_data[name] = {"cells": 0, "rows": 0, "main_cols": 0, "prep_cols": 0, "perm_cols": 0}
    if e["metric"] == "cells":
        air_data[name]["cells"] += float(e["value"])
    elif e["metric"] == "rows":
        air_data[name]["rows"] += float(e["value"])
    elif e["metric"] == "main_cols":
        air_data[name]["main_cols"] = max(air_data[name]["main_cols"], float(e["value"]))
    elif e["metric"] == "prep_cols":
        air_data[name]["prep_cols"] = max(air_data[name]["prep_cols"], float(e["value"]))
    elif e["metric"] == "perm_cols":
        air_data[name]["perm_cols"] = max(air_data[name]["perm_cols"], float(e["value"]))

# Get correct interaction counts per app AIR name
interaction_by_name = {}
for e in entries:
    if e["metric"] == "interactions" and e.get("air_name") in app_air_names:
        interaction_by_name[e["air_name"]] = float(e["value"])

# ============================================================
# Key hypothesis: cells = rows * (main_cols + K * interactions)
# ============================================================

print("=" * 70)
print("HYPOTHESIS: cells = rows * (main_cols + K * interactions)")
print("=" * 70)

print(f"\n{'AIR':<55} {'main':>5} {'inter':>6} {'cells/rows':>10} {'extra':>6} {'extra/inter':>11}")
print("-" * 100)

all_match_4 = True
for name in sorted(air_data.keys(), key=lambda x: air_data[x]["cells"], reverse=True):
    d = air_data[name]
    inter = interaction_by_name.get(name, 0)
    if d["rows"] > 0:
        effective_width = d["cells"] / d["rows"]
        extra = effective_width - d["main_cols"]
        ratio = extra / inter if inter > 0 else 0
        match = "4.0" if abs(ratio - 4.0) < 0.01 else f"{ratio:.2f} !!!"
        if abs(ratio - 4.0) >= 0.01:
            all_match_4 = False
        short_name = name[:54]
        print(f"{short_name:<55} {d['main_cols']:>5.0f} {inter:>6.0f} {effective_width:>10.1f} {extra:>6.0f} {match:>11}")

print(f"\n>>> ALL AIRs have extra = 4 * interactions: {all_match_4} <<<")

# ============================================================
# This means: cells = rows * (main_cols + 4 * interactions)
# The 4 extra columns per interaction are NOT reported in main/prep/perm_cols
# ============================================================

print()
print("=" * 70)
print("CONCLUSION: cells = rows * (main_cols + 4 * interactions)")
print("=" * 70)
print("""
In OpenVM V2, each bus interaction adds 4 columns to the trace that are
counted in the 'cells' metric but NOT reported in main_cols, prep_cols,
or perm_cols. These are likely LogUp/GKR interaction columns.

This means the 'Columns' metric in the viewer (sum of main+prep+perm)
underreports the actual column count, while 'Cells' correctly reflects
the total prover work.
""")

# ============================================================
# Now recompute the complete decomposition including this finding
# ============================================================

apcs = apc_data["apcs"]
apc_main_cells = sum(a["cost_before"] * a["execution_frequency"] for a in apcs)
apc_bus_interactions = sum(a["stats"]["before"]["bus_interactions"] * a["execution_frequency"] for a in apcs)
apc_true_cells = apc_main_cells + 4 * apc_bus_interactions

# Viewer instruction cells
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

instr_cells = sum(d["cells"] for name, d in air_data.items() if is_normal_instruction_air(name))
all_cells = sum(d["cells"] for d in air_data.values())

print("=" * 70)
print("COMPLETE DECOMPOSITION OF TRACE CELLS DISCREPANCY")
print("=" * 70)

print(f"\n  APC software cells (main_cols only):           {apc_main_cells:>14,.0f}  ({apc_main_cells/1e9:.2f}B)")
print(f"  + 4 * bus interaction columns:                 {4*apc_bus_interactions:>14,.0f}  ({4*apc_bus_interactions/1e9:.2f}B)")
print(f"  = APC true cells (incl. interaction cols):     {apc_true_cells:>14,.0f}  ({apc_true_cells/1e9:.2f}B)")
print()
print(f"  Instruction AIR cells (viewer):                {instr_cells:>14,.0f}  ({instr_cells/1e9:.2f}B)")
print(f"  All AIR cells (viewer):                        {all_cells:>14,.0f}  ({all_cells/1e9:.2f}B)")

print(f"\n  Decomposition:")
print(f"    Factor 1: Interaction columns (4 per bus interaction)")
print(f"      APC main_cols cells → APC true cells:      {apc_true_cells/apc_main_cells:.3f}x")
print(f"    Factor 2: Padding (rows padded to next power of 2 per segment)")
print(f"      APC true cells → viewer instr cells:       {instr_cells/apc_true_cells:.3f}x")
print(f"    Factor 3: Non-instruction AIRs")
print(f"      Viewer instr cells → all cells:            {all_cells/instr_cells:.3f}x")
print(f"    Product:                                     {(apc_true_cells/apc_main_cells)*(instr_cells/apc_true_cells)*(all_cells/instr_cells):.3f}x")
print(f"    Actual ratio:                                {all_cells/apc_main_cells:.3f}x")

# ============================================================
# Same analysis for constraint instances
# ============================================================

print()
print("=" * 70)
print("COMPLETE DECOMPOSITION OF CONSTRAINT INSTANCES DISCREPANCY")
print("=" * 70)

apc_constraint_instances = sum(a["stats"]["before"]["constraints"] * a["execution_frequency"] for a in apcs)
apc_bus_messages = sum(a["stats"]["before"]["bus_interactions"] * a["execution_frequency"] for a in apcs)

# Correct viewer constraint instances (using air_name matching)
rows_by_name = defaultdict(float)
for e in app:
    if e["metric"] == "rows":
        rows_by_name[e["air_name"]] += float(e["value"])

constraint_by_name = {}
for e in entries:
    if e["metric"] == "constraints" and e.get("air_name") in app_air_names:
        constraint_by_name[e["air_name"]] = float(e["value"])

correct_constraint_instances = sum(
    constraint_by_name.get(name, 0) * rows_by_name.get(name, 0)
    for name in app_air_names
)

correct_bus_messages = sum(
    interaction_by_name.get(name, 0) * rows_by_name.get(name, 0)
    for name in app_air_names
)

print(f"\n  APC constraint instances:                      {apc_constraint_instances:>14,.0f}  ({apc_constraint_instances/1e9:.2f}B)")
print(f"  Viewer constraint instances (BUGGY):           {6137364620:>14,.0f}  ({6137364620/1e9:.2f}B)")
print(f"  Viewer constraint instances (CORRECTED):       {correct_constraint_instances:>14,.0f}  ({correct_constraint_instances/1e9:.2f}B)")
print()
print(f"  APC bus interaction messages:                  {apc_bus_messages:>14,.0f}  ({apc_bus_messages/1e9:.2f}B)")
print(f"  Viewer bus messages (BUGGY):                   {1443791902:>14,.0f}  ({1443791902/1e9:.2f}B)")
print(f"  Viewer bus messages (CORRECTED):               {correct_bus_messages:>14,.0f}  ({correct_bus_messages/1e9:.2f}B)")

print(f"\n  Remaining ratio (corrected viewer / APC):")
print(f"    Constraint instances: {correct_constraint_instances/apc_constraint_instances:.2f}x  (≈ padding factor)")
print(f"    Bus messages:         {correct_bus_messages/apc_bus_messages:.2f}x  (≈ padding factor)")

# ============================================================
# Summary table
# ============================================================

print()
print("=" * 70)
print("FINAL SUMMARY")
print("=" * 70)
print(f"""
┌─────────────────────────┬───────────────┬───────────────┬───────────────┬──────────────┐
│ Metric                  │ APC Analyzer  │ Viewer (buggy)│ Viewer (fixed)│ Ratio (fixed)│
├─────────────────────────┼───────────────┼───────────────┼───────────────┼──────────────┤
│ Trace cells             │ {apc_main_cells/1e9:>8.2f}B    │ {all_cells/1e9:>8.2f}B    │ {all_cells/1e9:>8.2f}B    │ {all_cells/apc_main_cells:>8.2f}x   │
│ Constraint instances    │ {apc_constraint_instances/1e9:>8.2f}B    │ {6137364620/1e9:>8.2f}B    │ {correct_constraint_instances/1e9:>8.2f}B    │ {correct_constraint_instances/apc_constraint_instances:>8.2f}x   │
│ Bus interaction msgs    │ {apc_bus_messages/1e9:>8.2f}B    │ {1443791902/1e9:>8.2f}B    │ {correct_bus_messages/1e9:>8.2f}B    │ {correct_bus_messages/apc_bus_messages:>8.2f}x   │
└─────────────────────────┴───────────────┴───────────────┴───────────────┴──────────────┘

Sources of remaining discrepancy (after fixing the air_id collision bug):

  Trace cells (3.74x):
    1. Interaction columns: 4 columns per bus interaction, not in main_cols (3.07x)
    2. Padding: rows padded to next power of 2 per segment (~1.20x)
    3. Non-instruction AIRs: ~1.3% of total cells (1.01x)

  Constraint instances (1.26x after fix):
    1. Padding: ~1.20x
    2. Non-instruction AIRs + minor counting differences: ~1.05x

  Bus interaction messages (1.21x after fix):
    1. Padding: ~1.20x
    2. Non-instruction AIRs: ~1.01x

BUGS FOUND:
  1. [METRICS VIEWER] air_id collision: constraint/interaction entries have no
     group label, but air_ids are only unique within a proving phase. The viewer
     uses air_id to join constraints with rows, causing non-app AIR constraints
     to be multiplied by app AIR rows. This inflates constraint instances by
     5.45x and bus interaction messages by 1.50x.

  2. [EXPECTED DISCREPANCY] The 'cells' metric includes 4 * interactions
     additional columns per AIR that are not reported in main_cols/prep_cols/
     perm_cols. These are likely LogUp/GKR interaction columns in OpenVM V2.
     This is not necessarily a bug, but it means the APC analyzer's
     "software cost" significantly underestimates the actual trace area.
""")

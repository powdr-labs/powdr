#!/usr/bin/env python3
"""
Step 3: Investigate the air_id collision bug and its impact on constraint instances.
"""

import json
from collections import defaultdict

with open("metrics_v2_pairing_combined.json") as f:
    metrics_data = json.load(f)

exp = metrics_data["metrics_v2_pairing_apc000"]

# Flatten entries
entries = []
for c in exp["counter"] + exp["gauge"]:
    obj = dict(c["labels"])
    obj["metric"] = c["metric"]
    obj["value"] = c["value"]
    entries.append(obj)

app = [e for e in entries if e.get("group", "").startswith("app_proof")]

# ============================================================
# 1. Find all constraint/interaction entries and their air_ids
# ============================================================

print("=" * 70)
print("CONSTRAINT AND INTERACTION ENTRIES BY AIR_ID")
print("(These entries do NOT have a group label)")
print("=" * 70)

constraint_entries = [e for e in entries if e["metric"] == "constraints"]
interaction_entries = [e for e in entries if e["metric"] == "interactions"]

# Group by air_id to find collisions
by_air_id = defaultdict(list)
for e in constraint_entries:
    by_air_id[e["air_id"]].append(("constraints", e["air_name"], float(e["value"])))
for e in interaction_entries:
    by_air_id[e["air_id"]].append(("interactions", e["air_name"], float(e["value"])))

print(f"\nAIRs with MULTIPLE entries for same air_id (COLLISIONS):")
for aid in sorted(by_air_id.keys(), key=int):
    air_names = set(name for _, name, _ in by_air_id[aid])
    if len(air_names) > 1:
        print(f"\n  air_id={aid}:")
        for metric, name, val in sorted(by_air_id[aid]):
            print(f"    {metric:>15}: {name:<60} value={val:.0f}")

# ============================================================
# 2. Recompute constraint_instances WITHOUT the collision bug
# ============================================================

print()
print("=" * 70)
print("IMPACT OF AIR_ID COLLISION ON CONSTRAINT INSTANCES")
print("=" * 70)

# Build rows_by_app_air (same as viewer)
rows_by_app_air = defaultdict(float)
for e in app:
    if e["metric"] == "rows":
        rows_by_app_air[e["air_id"]] += float(e["value"])

# Buggy computation (as in the viewer): iterate all_entries
buggy_constraint_instances = 0
buggy_bus_messages = 0
for e in entries:
    if e["metric"] == "constraints":
        buggy_constraint_instances += float(e["value"]) * rows_by_app_air.get(e["air_id"], 0)
    elif e["metric"] == "interactions":
        buggy_bus_messages += float(e["value"]) * rows_by_app_air.get(e["air_id"], 0)

# Correct computation: only use constraint/interaction entries for app AIRs
# We need to match by air_name, not just air_id
app_air_names = set()
for e in app:
    if e.get("air_name"):
        app_air_names.add(e["air_name"])

# Build rows by (air_id, air_name) for app
rows_by_app_air_name = defaultdict(float)
for e in app:
    if e["metric"] == "rows":
        rows_by_app_air_name[(e["air_id"], e["air_name"])] += float(e["value"])

correct_constraint_instances = 0
correct_bus_messages = 0
for e in entries:
    if e["metric"] == "constraints":
        key = (e["air_id"], e["air_name"])
        if key in rows_by_app_air_name:
            correct_constraint_instances += float(e["value"]) * rows_by_app_air_name[key]
    elif e["metric"] == "interactions":
        key = (e["air_id"], e["air_name"])
        if key in rows_by_app_air_name:
            correct_bus_messages += float(e["value"]) * rows_by_app_air_name[key]

print(f"\n  Buggy constraint instances (viewer):     {buggy_constraint_instances:>16,.0f}  ({buggy_constraint_instances/1e9:.2f}B)")
print(f"  Correct constraint instances:            {correct_constraint_instances:>16,.0f}  ({correct_constraint_instances/1e9:.2f}B)")
print(f"  Inflation from bug:                      {buggy_constraint_instances/correct_constraint_instances:.2f}x")
print()
print(f"  Buggy bus interaction messages (viewer):  {buggy_bus_messages:>16,.0f}  ({buggy_bus_messages/1e9:.2f}B)")
print(f"  Correct bus interaction messages:         {correct_bus_messages:>16,.0f}  ({correct_bus_messages/1e9:.2f}B)")
print(f"  Inflation from bug:                      {buggy_bus_messages/correct_bus_messages:.2f}x")

# ============================================================
# 3. Which specific air_ids cause the inflation?
# ============================================================

print()
print("=" * 70)
print("PER-AIR_ID INFLATION BREAKDOWN")
print("=" * 70)

for aid in sorted(by_air_id.keys(), key=int):
    air_names = set(name for _, name, _ in by_air_id[aid])
    if len(air_names) <= 1:
        continue

    rows = rows_by_app_air.get(aid, 0)
    if rows == 0:
        continue

    # Buggy: sum of ALL constraints * rows
    buggy_c = sum(v for m, _, v in by_air_id[aid] if m == "constraints") * rows
    buggy_i = sum(v for m, _, v in by_air_id[aid] if m == "interactions") * rows

    # Correct: only the app AIR's constraints * rows
    app_names_for_id = set(e["air_name"] for e in app if e.get("air_id") == aid)
    correct_c = sum(v for m, n, v in by_air_id[aid] if m == "constraints" and n in app_names_for_id) * rows
    correct_i = sum(v for m, n, v in by_air_id[aid] if m == "interactions" and n in app_names_for_id) * rows

    print(f"\n  air_id={aid}, app rows={rows:,.0f}")
    print(f"    App AIR: {app_names_for_id}")
    c_infl = f"{buggy_c/correct_c:.2f}x" if correct_c > 0 else "N/A (correct=0)"
    i_infl = f"{buggy_i/correct_i:.2f}x" if correct_i > 0 else "N/A (correct=0)"
    print(f"    Constraint instances: buggy={buggy_c:,.0f}, correct={correct_c:,.0f}, inflation={c_infl}")
    print(f"    Bus messages:         buggy={buggy_i:,.0f}, correct={correct_i:,.0f}, inflation={i_infl}")

# ============================================================
# 4. Also check if constraints/interactions in the viewer differ
#    from APC analyzer's per-instruction counts
# ============================================================

print()
print("=" * 70)
print("VIEWER (corrected) vs APC ANALYZER: CONSTRAINT INSTANCES")
print("=" * 70)

with open("apc_candidates_pairing_v2.json") as f:
    apc_data = json.load(f)

apcs = apc_data["apcs"]
apc_constraint_instances = sum(a["stats"]["before"]["constraints"] * a["execution_frequency"] for a in apcs)
apc_bus_messages = sum(a["stats"]["before"]["bus_interactions"] * a["execution_frequency"] for a in apcs)

print(f"\n  APC analyzer constraint instances:       {apc_constraint_instances:>16,.0f}  ({apc_constraint_instances/1e9:.2f}B)")
print(f"  Corrected viewer constraint instances:   {correct_constraint_instances:>16,.0f}  ({correct_constraint_instances/1e9:.2f}B)")
print(f"  Ratio:                                   {correct_constraint_instances/apc_constraint_instances:.2f}x")
print()
print(f"  APC analyzer bus messages:               {apc_bus_messages:>16,.0f}  ({apc_bus_messages/1e9:.2f}B)")
print(f"  Corrected viewer bus messages:           {correct_bus_messages:>16,.0f}  ({correct_bus_messages/1e9:.2f}B)")
print(f"  Ratio:                                   {correct_bus_messages/apc_bus_messages:.2f}x")

# ============================================================
# 5. Understand what constraint counts differ:
#    APC gives per-block constraints, viewer gives per-AIR constraints
# ============================================================

print()
print("=" * 70)
print("CONSTRAINT COUNTS: VIEWER (per-AIR) vs APC (per-block)")
print("=" * 70)

# Viewer constraints per AIR (for app AIRs)
viewer_constraints_by_air = {}
for e in entries:
    if e["metric"] == "constraints" and e["air_name"] in app_air_names:
        viewer_constraints_by_air[e["air_name"]] = float(e["value"])

viewer_interactions_by_air = {}
for e in entries:
    if e["metric"] == "interactions" and e["air_name"] in app_air_names:
        viewer_interactions_by_air[e["air_name"]] = float(e["value"])

print("\nViewer constraints per AIR:")
for name, val in sorted(viewer_constraints_by_air.items(), key=lambda x: -x[1]):
    interactions = viewer_interactions_by_air.get(name, 0)
    print(f"  {name:<65} constraints={val:>5.0f}  interactions={interactions:>5.0f}")

# APC: constraints per block = sum of per-instruction constraints
# All APCs have the same constraint structure per instruction type
print("\nAPC per-block constraint examples:")
for a in apcs[:5]:
    n_instr = sum(len(b["instructions"]) for b in a["original_blocks"])
    c = a["stats"]["before"]["constraints"]
    bi = a["stats"]["before"]["bus_interactions"]
    print(f"  {n_instr:>3} instructions: constraints={c:>5.0f} ({c/n_instr:.1f}/instr), "
          f"bus_interactions={bi:>5.0f} ({bi/n_instr:.1f}/instr)")

# Average constraints per instruction in APC data
total_instrs = sum(sum(len(b["instructions"]) for b in a["original_blocks"]) for a in apcs)
total_constraints = sum(a["stats"]["before"]["constraints"] for a in apcs)
total_bus = sum(a["stats"]["before"]["bus_interactions"] for a in apcs)
print(f"\n  APC avg constraints per instruction: {total_constraints/total_instrs:.1f}")
print(f"  APC avg bus_interactions per instruction: {total_bus/total_instrs:.1f}")

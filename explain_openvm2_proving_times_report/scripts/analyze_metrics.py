#!/usr/bin/env python3
"""
Analyze OpenVM 2 metrics to confirm observations about APC scaling behavior.
Computes all statistics from the raw metrics JSON files.
"""

import json
import sys
import os

# Add the metrics viewer to path so we can reuse spec.py
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../openvm/metrics-viewer'))
from spec import extract_metrics, load_metrics_dataframes, sum_metric

DATA_DIR = os.path.join(os.path.dirname(__file__), '../data')


def load_combined(path):
    """Load combined metrics JSON and return dict of {name: metrics_json}."""
    with open(path) as f:
        return json.load(f)


def get_per_air_stats(metrics_json):
    """Get per-AIR statistics for deeper analysis."""
    all_entries, app, leaf, internal, compression = load_metrics_dataframes(metrics_json)

    # Collect per-AIR info
    air_info = {}
    for e in app:
        if e["metric"] == "rows":
            key = (e.get("air_id", "?"), e.get("air_name", "?"))
            if key not in air_info:
                air_info[key] = {"rows": 0, "segments": 0, "cells": 0,
                                 "main_cols": 0, "prep_cols": 0, "perm_cols": 0}
            air_info[key]["rows"] += float(e["value"])
            air_info[key]["segments"] += 1

    for e in app:
        key = (e.get("air_id", "?"), e.get("air_name", "?"))
        if key not in air_info:
            continue
        if e["metric"] == "cells":
            air_info[key]["cells"] += float(e["value"])
        elif e["metric"] == "main_cols":
            air_info[key]["main_cols"] = max(air_info[key]["main_cols"], float(e["value"]))
        elif e["metric"] == "prep_cols":
            air_info[key]["prep_cols"] = max(air_info[key]["prep_cols"], float(e["value"]))
        elif e["metric"] == "perm_cols":
            air_info[key]["perm_cols"] = max(air_info[key]["perm_cols"], float(e["value"]))

    # Get constraints and interactions per AIR (these don't have group/segment labels)
    for e in all_entries:
        if e["metric"] in ("constraints", "interactions"):
            key = (e.get("air_id", "?"), e.get("air_name", "?"))
            if key in air_info:
                air_info[key][e["metric"]] = float(e["value"])

    return air_info


def get_per_segment_stats(metrics_json):
    """Get per-segment timing breakdowns."""
    all_entries, app, leaf, internal, compression = load_metrics_dataframes(metrics_json)

    segments = {}
    for e in app:
        seg = e.get("segment", "?")
        if seg not in segments:
            segments[seg] = {}
        metric = e["metric"]
        if metric not in segments[seg]:
            segments[seg][metric] = 0
        segments[seg][metric] += float(e["value"])

    return segments


def count_unique_airs(metrics_json):
    """Count unique AIRs (by air_id, air_name)."""
    all_entries, app, leaf, internal, compression = load_metrics_dataframes(metrics_json)
    airs = set()
    for e in app:
        if e["metric"] == "rows":
            airs.add((e.get("air_id", "?"), e.get("air_name", "?")))
    return len(airs)


def count_powdr_airs(metrics_json):
    """Count PowdrAir instances."""
    all_entries, app, leaf, internal, compression = load_metrics_dataframes(metrics_json)
    airs = set()
    for e in app:
        if e["metric"] == "rows" and (e.get("air_name", "") or "").startswith("PowdrAir"):
            airs.add((e.get("air_id", "?"), e.get("air_name", "?")))
    return len(airs)


def fmt(v, unit=""):
    """Format a number nicely."""
    if v is None:
        return "N/A"
    if abs(v) >= 1e9:
        return f"{v/1e9:.2f}B{unit}"
    if abs(v) >= 1e6:
        return f"{v/1e6:.2f}M{unit}"
    if abs(v) >= 1e3:
        return f"{v/1e3:.1f}K{unit}"
    return f"{v:.1f}{unit}"


def fmt_ms(v):
    """Format milliseconds as seconds."""
    if v is None:
        return "N/A"
    return f"{v/1000:.2f}s"


def analyze_guest(name, metrics_path):
    """Analyze a guest benchmark."""
    print(f"\n{'='*80}")
    print(f"  {name} Guest Analysis")
    print(f"{'='*80}")

    combined = load_combined(metrics_path)

    # Sort experiments by name
    exp_names = sorted(combined.keys())
    print(f"\nExperiments: {', '.join(exp_names)}")

    # Extract metrics for all experiments
    all_metrics = {}
    for exp_name in exp_names:
        m = extract_metrics(exp_name, combined[exp_name])
        all_metrics[exp_name] = m

    # Find baseline (apc000 or similar)
    baseline_name = None
    for n in exp_names:
        if "000" in n or "baseline" in n.lower():
            baseline_name = n
            break
    if baseline_name is None:
        baseline_name = exp_names[0]

    baseline = all_metrics[baseline_name]

    # Print comparison table
    print(f"\n{'Experiment':<20} {'Segments':>8} {'AIRs':>8} {'PowdrAIRs':>10} {'Cells':>12} "
          f"{'Constraints':>14} {'Const.Inst':>14} {'BusInt':>12} {'BusIntMsg':>14}")
    print("-" * 130)

    for exp_name in exp_names:
        m = all_metrics[exp_name]
        n_airs = count_unique_airs(combined[exp_name])
        n_powdr = count_powdr_airs(combined[exp_name])
        print(f"{exp_name:<20} {m['num_segments']:>8.0f} {n_airs:>8} {n_powdr:>10} "
              f"{fmt(m['app_proof_cells']):>12} "
              f"{fmt(m.get('constraints')):>14} "
              f"{fmt(m.get('constraint_instances')):>14} "
              f"{fmt(m.get('bus_interactions')):>12} "
              f"{fmt(m.get('bus_interaction_messages')):>14}")

    # Print timing table
    print(f"\n{'Experiment':<20} {'AppProof':>10} {'STARK(-tr)':>10} {'Constraints':>12} "
          f"{'LogUpGKR':>10} {'Round0':>10} {'MLERounds':>10} {'Openings':>10} "
          f"{'WHIR':>10} {'StackRed':>10} {'TraceCommit':>10}")
    print("-" * 140)

    for exp_name in exp_names:
        m = all_metrics[exp_name]
        print(f"{exp_name:<20} "
              f"{fmt_ms(m['app_proof_time_ms']):>10} "
              f"{fmt_ms(m['app_proof_time_excluding_trace_ms']):>10} "
              f"{fmt_ms(m['app_rap_constraints_time_ms']):>12} "
              f"{fmt_ms(m['app_rap_logup_gkr_time_ms']):>10} "
              f"{fmt_ms(m['app_rap_round0_time_ms']):>10} "
              f"{fmt_ms(m['app_rap_mle_rounds_time_ms']):>10} "
              f"{fmt_ms(m['app_openings_time_ms']):>10} "
              f"{fmt_ms(m['app_openings_whir_time_ms']):>10} "
              f"{fmt_ms(m['app_openings_stacked_reduction_time_ms']):>10} "
              f"{fmt_ms(m['app_trace_commit_time_ms']):>10}")

    # Print ratios relative to baseline
    print(f"\n--- Ratios relative to {baseline_name} (>1 means reduction, <1 means increase) ---")
    print(f"\n{'Experiment':<20} {'Cells':>8} {'Const.Inst':>10} {'BusIntMsg':>10} "
          f"{'STARK(-tr)':>10} {'Constraints':>12} {'LogUpGKR':>10} {'Round0':>10} "
          f"{'MLERounds':>10} {'Openings':>10} {'WHIR':>10} {'StackRed':>10} {'TraceCommit':>10}")
    print("-" * 150)

    for exp_name in exp_names:
        m = all_metrics[exp_name]

        def ratio(key):
            b = baseline.get(key, 0)
            v = m.get(key, 0)
            if not v or not b:
                return "N/A"
            return f"{b/v:.2f}x"

        print(f"{exp_name:<20} "
              f"{ratio('app_proof_cells'):>8} "
              f"{ratio('constraint_instances'):>10} "
              f"{ratio('bus_interaction_messages'):>10} "
              f"{ratio('app_proof_time_excluding_trace_ms'):>10} "
              f"{ratio('app_rap_constraints_time_ms'):>12} "
              f"{ratio('app_rap_logup_gkr_time_ms'):>10} "
              f"{ratio('app_rap_round0_time_ms'):>10} "
              f"{ratio('app_rap_mle_rounds_time_ms'):>10} "
              f"{ratio('app_openings_time_ms'):>10} "
              f"{ratio('app_openings_whir_time_ms'):>10} "
              f"{ratio('app_openings_stacked_reduction_time_ms'):>10} "
              f"{ratio('app_trace_commit_time_ms'):>10}")

    # Additional stats
    print(f"\n--- Additional Statistics ---")
    print(f"\n{'Experiment':<20} {'#AIRs':>8} {'#PowdrAIRs':>10} {'#Segments':>10} "
          f"{'Constraints':>14} {'BusInteract':>14} "
          f"{'Columns':>10}")
    print("-" * 100)

    for exp_name in exp_names:
        m = all_metrics[exp_name]
        n_airs = count_unique_airs(combined[exp_name])
        n_powdr = count_powdr_airs(combined[exp_name])
        print(f"{exp_name:<20} {n_airs:>8} {n_powdr:>10} {m['num_segments']:>10.0f} "
              f"{fmt(m.get('constraints')):>14} "
              f"{fmt(m.get('bus_interactions')):>14} "
              f"{fmt(m['app_proof_cols']):>10}")

    # Per-segment average timing
    print(f"\n--- Per-Segment Average STARK Times ---")
    print(f"\n{'Experiment':<20} {'#Segs':>6} {'STARK/seg':>10} {'Constr/seg':>10} "
          f"{'Openings/seg':>12} {'TrCommit/seg':>12}")
    print("-" * 80)

    for exp_name in exp_names:
        m = all_metrics[exp_name]
        n = m['num_segments']
        if n > 0:
            print(f"{exp_name:<20} {n:>6.0f} "
                  f"{fmt_ms(m['app_proof_time_excluding_trace_ms']/n):>10} "
                  f"{fmt_ms(m['app_rap_constraints_time_ms']/n):>10} "
                  f"{fmt_ms(m['app_openings_time_ms']/n):>12} "
                  f"{fmt_ms(m['app_trace_commit_time_ms']/n):>12}")

    # Compute cost per cell, per constraint instance, per bus interaction message
    print(f"\n--- Cost Efficiency (time per unit) ---")
    print(f"\n{'Experiment':<20} {'STARK/Mcell':>12} {'STARK/Mconst':>12} {'STARK/Mbus':>12} "
          f"{'Constr/Mconst':>14} {'Open/Mcell':>12}")
    print("-" * 90)

    for exp_name in exp_names:
        m = all_metrics[exp_name]
        stark_ms = m['app_proof_time_excluding_trace_ms']
        cells = m['app_proof_cells']
        ci = m.get('constraint_instances', 0)
        bim = m.get('bus_interaction_messages', 0)

        stark_per_mcell = (stark_ms / (cells / 1e6)) if cells > 0 else 0
        stark_per_mconst = (stark_ms / (ci / 1e6)) if ci else 0
        stark_per_mbus = (stark_ms / (bim / 1e6)) if bim else 0
        constr_per_mconst = (m['app_rap_constraints_time_ms'] / (ci / 1e6)) if ci else 0
        open_per_mcell = (m['app_openings_time_ms'] / (cells / 1e6)) if cells > 0 else 0

        print(f"{exp_name:<20} "
              f"{stark_per_mcell:>10.2f}ms "
              f"{stark_per_mconst:>10.2f}ms "
              f"{stark_per_mbus:>10.2f}ms "
              f"{constr_per_mconst:>12.2f}ms "
              f"{open_per_mcell:>10.2f}ms")

    return all_metrics, combined


def analyze_per_air_detail(name, combined, exp_name):
    """Detailed per-AIR analysis for a specific experiment."""
    print(f"\n--- Per-AIR Detail for {name}/{exp_name} ---")

    air_info = get_per_air_stats(combined[exp_name])

    # Sort by cells (descending)
    sorted_airs = sorted(air_info.items(), key=lambda x: x[1].get("cells", 0), reverse=True)

    print(f"\n{'AIR Name':<60} {'Rows':>12} {'Cols':>6} {'Cells':>12} "
          f"{'Constraints':>12} {'Interactions':>12} {'Segments':>8}")
    print("-" * 130)

    for (air_id, air_name), info in sorted_airs[:30]:
        name_short = air_name[:58] if len(air_name) > 58 else air_name
        cols = info["main_cols"] + info["prep_cols"] + info["perm_cols"]
        print(f"{name_short:<60} "
              f"{fmt(info['rows']):>12} "
              f"{cols:>6.0f} "
              f"{fmt(info['cells']):>12} "
              f"{fmt(info.get('constraints', 0)):>12} "
              f"{fmt(info.get('interactions', 0)):>12} "
              f"{info['segments']:>8}")

    if len(sorted_airs) > 30:
        print(f"  ... and {len(sorted_airs) - 30} more AIRs")

    # Summary stats
    total_cells = sum(v.get("cells", 0) for v in air_info.values())
    total_constraints = sum(v.get("constraints", 0) for v in air_info.values())
    total_interactions = sum(v.get("interactions", 0) for v in air_info.values())
    max_cols = max((v["main_cols"] + v["prep_cols"] + v["perm_cols"]) for v in air_info.values())
    avg_cols = sum((v["main_cols"] + v["prep_cols"] + v["perm_cols"]) for v in air_info.values()) / len(air_info) if air_info else 0

    powdr_airs = {k: v for k, v in air_info.items() if k[1].startswith("PowdrAir")}
    if powdr_airs:
        powdr_max_cols = max((v["main_cols"] + v["prep_cols"] + v["perm_cols"]) for v in powdr_airs.values())
        powdr_avg_cols = sum((v["main_cols"] + v["prep_cols"] + v["perm_cols"]) for v in powdr_airs.values()) / len(powdr_airs)
        powdr_avg_constraints = sum(v.get("constraints", 0) for v in powdr_airs.values()) / len(powdr_airs)
        powdr_avg_interactions = sum(v.get("interactions", 0) for v in powdr_airs.values()) / len(powdr_airs)

        print(f"\n  PowdrAir stats:")
        print(f"    Count: {len(powdr_airs)}")
        print(f"    Avg columns: {powdr_avg_cols:.1f}, Max: {powdr_max_cols:.0f}")
        print(f"    Avg constraints: {powdr_avg_constraints:.1f}")
        print(f"    Avg interactions: {powdr_avg_interactions:.1f}")

    print(f"\n  All AIR stats:")
    print(f"    Total unique AIRs: {len(air_info)}")
    print(f"    Total cells: {fmt(total_cells)}")
    print(f"    Sum of constraints (per-AIR): {total_constraints:.0f}")
    print(f"    Sum of interactions (per-AIR): {total_interactions:.0f}")
    print(f"    Max columns: {max_cols:.0f}, Avg: {avg_cols:.1f}")


def main():
    print("OpenVM 2 Metrics Analysis")
    print("=" * 80)

    # Analyze Pairing guest
    pairing_metrics, pairing_combined = analyze_guest(
        "Pairing", os.path.join(DATA_DIR, 'pairing_metrics.json'))

    # Analyze Keccak guest
    keccak_metrics, keccak_combined = analyze_guest(
        "Keccak", os.path.join(DATA_DIR, 'keccak_metrics.json'))

    # Per-AIR detail for key experiments
    for exp_name in sorted(pairing_combined.keys()):
        analyze_per_air_detail("Pairing", pairing_combined, exp_name)

    for exp_name in sorted(keccak_combined.keys()):
        analyze_per_air_detail("Keccak", keccak_combined, exp_name)


if __name__ == "__main__":
    main()

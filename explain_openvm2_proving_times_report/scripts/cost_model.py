#!/usr/bin/env python3
"""
Cost model for OpenVM 2 STARK proving time.

Fits a model to the observed data and attempts to predict STARK proving time
from AIR statistics. The model captures both cell-proportional costs and
per-AIR/per-segment overhead.
"""

import json
import os
import sys
import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../openvm/metrics-viewer'))
from spec import extract_metrics, load_metrics_dataframes, sum_metric

DATA_DIR = os.path.join(os.path.dirname(__file__), '../data')


def load_combined(path):
    with open(path) as f:
        return json.load(f)


def get_stats(combined, exp_name):
    """Extract all relevant statistics for an experiment."""
    metrics_json = combined[exp_name]
    m = extract_metrics(exp_name, metrics_json)
    all_entries, app, leaf, internal, compression = load_metrics_dataframes(metrics_json)

    # Count unique AIRs
    airs = set()
    air_heights = {}
    for e in app:
        if e["metric"] == "rows":
            key = (e.get("air_id", "?"), e.get("air_name", "?"))
            airs.add(key)
            h = float(e["value"])
            if key not in air_heights:
                air_heights[key] = 0
            air_heights[key] += h  # sum across segments

    n_airs = len(airs)

    # Count PowdrAirs
    n_powdr = sum(1 for (_, name) in airs if name.startswith("PowdrAir"))

    # Get per-AIR constraint/interaction counts
    constraints_per_air = {}
    interactions_per_air = {}
    for e in all_entries:
        key = (e.get("air_id", "?"), e.get("air_name", "?"))
        if key in airs:
            if e["metric"] == "constraints":
                constraints_per_air[key] = float(e["value"])
            elif e["metric"] == "interactions":
                interactions_per_air[key] = float(e["value"])

    # Sum of constraints and interactions (not weighted by segments)
    sum_constraints = sum(constraints_per_air.values())
    sum_interactions = sum(interactions_per_air.values())

    # Total columns (sum across all AIRs)
    total_main_cols = sum_metric(app, "main_cols")
    total_prep_cols = sum_metric(app, "prep_cols")
    total_perm_cols = sum_metric(app, "perm_cols")
    total_cols = total_main_cols + total_prep_cols + total_perm_cols

    return {
        "name": exp_name,
        "n_segments": m["num_segments"],
        "n_airs": n_airs,
        "n_powdr": n_powdr,
        "total_cells": m["app_proof_cells"],
        "total_cols": total_cols,
        "constraint_instances": m.get("constraint_instances", 0) or 0,
        "bus_interaction_messages": m.get("bus_interaction_messages", 0) or 0,
        "sum_constraints": sum_constraints,
        "sum_interactions": sum_interactions,
        "n_air_instances": m["num_air_instances"],
        # Timing
        "stark_time_ms": m["app_proof_time_excluding_trace_ms"],
        "constraints_time_ms": m["app_rap_constraints_time_ms"],
        "logup_gkr_time_ms": m["app_rap_logup_gkr_time_ms"],
        "round0_time_ms": m["app_rap_round0_time_ms"],
        "mle_rounds_time_ms": m["app_rap_mle_rounds_time_ms"],
        "openings_time_ms": m["app_openings_time_ms"],
        "whir_time_ms": m["app_openings_whir_time_ms"],
        "stacked_reduction_time_ms": m["app_openings_stacked_reduction_time_ms"],
        "trace_commit_time_ms": m["app_trace_commit_time_ms"],
        "stark_other_ms": m["app_stark_other_ms"],
    }


def fit_linear_model(data, target_key, feature_keys):
    """Fit a simple linear model: target = sum(coeff_i * feature_i) + intercept."""
    n = len(data)
    X = np.zeros((n, len(feature_keys) + 1))  # +1 for intercept
    y = np.zeros(n)
    for i, d in enumerate(data):
        for j, fk in enumerate(feature_keys):
            X[i, j] = d[fk]
        X[i, -1] = 1  # intercept
        y[i] = d[target_key]

    # Least squares fit
    coeffs, residuals, rank, sv = np.linalg.lstsq(X, y, rcond=None)

    # Predictions and R²
    y_pred = X @ coeffs
    ss_res = np.sum((y - y_pred) ** 2)
    ss_tot = np.sum((y - np.mean(y)) ** 2)
    r_squared = 1 - ss_res / ss_tot if ss_tot > 0 else 0

    return coeffs, r_squared, y_pred


def main():
    print("=" * 80)
    print("  OpenVM 2 STARK Proving Time Cost Model")
    print("=" * 80)

    # Load all experiments
    all_data = []
    for guest_name, filename in [("pairing", "pairing_metrics.json"), ("keccak", "keccak_metrics.json")]:
        combined = load_combined(os.path.join(DATA_DIR, filename))
        for exp_name in sorted(combined.keys()):
            stats = get_stats(combined, exp_name)
            stats["guest"] = guest_name
            all_data.append(stats)

    # Print raw data table
    print(f"\n{'Name':<40} {'Segs':>4} {'AIRs':>5} {'Cols':>8} "
          f"{'Cells(M)':>10} {'CI(M)':>10} {'BIM(M)':>10} "
          f"{'STARK(s)':>8} {'R0(s)':>6} {'MLE(s)':>6} {'WHIR(s)':>7}")
    print("-" * 130)
    for d in all_data:
        print(f"{d['name']:<40} {d['n_segments']:>4} {d['n_airs']:>5} {d['total_cols']:>8.0f} "
              f"{d['total_cells']/1e6:>10.1f} {d['constraint_instances']/1e6:>10.1f} {d['bus_interaction_messages']/1e6:>10.1f} "
              f"{d['stark_time_ms']/1000:>8.2f} {d['round0_time_ms']/1000:>6.2f} {d['mle_rounds_time_ms']/1000:>6.2f} "
              f"{d['whir_time_ms']/1000:>7.2f}")

    # =========================================================================
    # Model 1: Simple - STARK time as function of cells only
    # =========================================================================
    print("\n\n" + "=" * 80)
    print("  Model 1: STARK_time = a * cells + b")
    print("=" * 80)

    coeffs, r2, y_pred = fit_linear_model(all_data, "stark_time_ms", ["total_cells"])
    print(f"  Coefficients: {coeffs[0]:.6f} ms/cell, intercept: {coeffs[1]:.1f} ms")
    print(f"  R² = {r2:.4f}")

    print(f"\n  {'Name':<40} {'Actual(s)':>10} {'Predicted(s)':>12} {'Error':>8}")
    for d, pred in zip(all_data, y_pred):
        actual = d['stark_time_ms'] / 1000
        predicted = pred / 1000
        error = (predicted - actual) / actual * 100
        print(f"  {d['name']:<40} {actual:>10.2f} {predicted:>12.2f} {error:>7.1f}%")

    # =========================================================================
    # Model 2: cells + per-AIR-instance overhead
    # =========================================================================
    print("\n\n" + "=" * 80)
    print("  Model 2: STARK_time = a * cells + b * n_air_instances + c")
    print("=" * 80)

    coeffs, r2, y_pred = fit_linear_model(all_data, "stark_time_ms", ["total_cells", "n_air_instances"])
    print(f"  Coefficients:")
    print(f"    cells:          {coeffs[0]*1e6:.4f} ms per Mcell")
    print(f"    AIR instances:  {coeffs[1]:.2f} ms per AIR instance")
    print(f"    intercept:      {coeffs[2]:.1f} ms")
    print(f"  R² = {r2:.4f}")

    print(f"\n  {'Name':<40} {'Actual(s)':>10} {'Predicted(s)':>12} {'Error':>8}")
    for d, pred in zip(all_data, y_pred):
        actual = d['stark_time_ms'] / 1000
        predicted = pred / 1000
        error = (predicted - actual) / actual * 100
        print(f"  {d['name']:<40} {actual:>10.2f} {predicted:>12.2f} {error:>7.1f}%")

    # =========================================================================
    # Model 3: cells + cols + air_instances
    # =========================================================================
    print("\n\n" + "=" * 80)
    print("  Model 3: STARK_time = a * cells + b * n_air_instances + c * total_cols + d")
    print("=" * 80)

    coeffs, r2, y_pred = fit_linear_model(all_data, "stark_time_ms",
                                           ["total_cells", "n_air_instances", "total_cols"])
    print(f"  Coefficients:")
    print(f"    cells:          {coeffs[0]*1e6:.4f} ms per Mcell")
    print(f"    AIR instances:  {coeffs[1]:.2f} ms per AIR instance")
    print(f"    total_cols:     {coeffs[2]:.4f} ms per column")
    print(f"    intercept:      {coeffs[3]:.1f} ms")
    print(f"  R² = {r2:.4f}")

    print(f"\n  {'Name':<40} {'Actual(s)':>10} {'Predicted(s)':>12} {'Error':>8}")
    for d, pred in zip(all_data, y_pred):
        actual = d['stark_time_ms'] / 1000
        predicted = pred / 1000
        error = (predicted - actual) / actual * 100
        print(f"  {d['name']:<40} {actual:>10.2f} {predicted:>12.2f} {error:>7.1f}%")

    # =========================================================================
    # Model 4: Per-sub-component models
    # =========================================================================
    print("\n\n" + "=" * 80)
    print("  Model 4: Sub-component models")
    print("=" * 80)

    sub_components = [
        ("round0_time_ms", "Round 0", ["n_air_instances", "sum_constraints", "sum_interactions"]),
        ("mle_rounds_time_ms", "MLE Rounds", ["n_air_instances", "total_cells"]),
        ("logup_gkr_time_ms", "LogUp GKR", ["bus_interaction_messages", "n_air_instances"]),
        ("whir_time_ms", "WHIR", ["total_cells", "total_cols"]),
        ("stacked_reduction_time_ms", "Stacked Reduction", ["total_cols", "n_air_instances"]),
        ("trace_commit_time_ms", "Trace Commit", ["total_cells"]),
    ]

    for target, label, features in sub_components:
        coeffs, r2, y_pred = fit_linear_model(all_data, target, features)
        print(f"\n  {label}: {target}")
        print(f"    Features: {', '.join(features)}")
        for i, f in enumerate(features):
            print(f"      {f}: {coeffs[i]:.6f}")
        print(f"      intercept: {coeffs[-1]:.2f}")
        print(f"    R² = {r2:.4f}")

    # =========================================================================
    # Model 5: Best overall model
    # =========================================================================
    print("\n\n" + "=" * 80)
    print("  Model 5: Best overall model")
    print("  STARK_time = a*cells + b*n_air_instances + c*constraint_instances + d*bus_interaction_messages + e*total_cols + f")
    print("=" * 80)

    features = ["total_cells", "n_air_instances", "constraint_instances",
                 "bus_interaction_messages", "total_cols"]
    coeffs, r2, y_pred = fit_linear_model(all_data, "stark_time_ms", features)
    print(f"  Coefficients:")
    for i, f in enumerate(features):
        print(f"    {f:<30} {coeffs[i]:.8f}")
    print(f"    {'intercept':<30} {coeffs[-1]:.1f}")
    print(f"  R² = {r2:.4f}")

    print(f"\n  {'Name':<40} {'Actual(s)':>10} {'Predicted(s)':>12} {'Error':>8}")
    for d, pred in zip(all_data, y_pred):
        actual = d['stark_time_ms'] / 1000
        predicted = pred / 1000
        error = (predicted - actual) / actual * 100
        print(f"  {d['name']:<40} {actual:>10.2f} {predicted:>12.2f} {error:>7.1f}%")

    # =========================================================================
    # Analysis: Per-AIR overhead breakdown
    # =========================================================================
    print("\n\n" + "=" * 80)
    print("  Per-AIR Overhead Analysis")
    print("=" * 80)

    # For pairing: compare apc000 vs apc500
    pairing_data = [d for d in all_data if d["guest"] == "pairing"]
    if len(pairing_data) >= 2:
        base = pairing_data[0]
        for d in pairing_data[1:]:
            delta_airs = d["n_air_instances"] - base["n_air_instances"]
            delta_cells = d["total_cells"] - base["total_cells"]
            delta_stark = d["stark_time_ms"] - base["stark_time_ms"]
            delta_r0 = d["round0_time_ms"] - base["round0_time_ms"]
            delta_mle = d["mle_rounds_time_ms"] - base["mle_rounds_time_ms"]
            delta_sr = d["stacked_reduction_time_ms"] - base["stacked_reduction_time_ms"]
            delta_whir = d["whir_time_ms"] - base["whir_time_ms"]
            delta_cols = d["total_cols"] - base["total_cols"]

            print(f"\n  {d['name']} vs {base['name']}:")
            print(f"    Delta AIR instances: {delta_airs:+.0f}")
            print(f"    Delta cells: {delta_cells/1e6:+.1f}M")
            print(f"    Delta columns: {delta_cols:+.0f}")
            print(f"    Delta STARK time: {delta_stark:+.0f}ms")
            print(f"    Delta Round 0: {delta_r0:+.0f}ms")
            print(f"    Delta MLE rounds: {delta_mle:+.0f}ms")
            print(f"    Delta Stacked Reduction: {delta_sr:+.0f}ms")
            print(f"    Delta WHIR: {delta_whir:+.0f}ms")
            if delta_airs > 0:
                print(f"    Round 0 per extra AIR instance: {delta_r0/delta_airs:.2f}ms")
                print(f"    Stacked Red per extra AIR instance: {delta_sr/delta_airs:.2f}ms")


if __name__ == "__main__":
    main()

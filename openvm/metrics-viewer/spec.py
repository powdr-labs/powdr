#!/usr/bin/env python3
"""
Audit script for OpenVM metrics viewer.

Recomputes the experiment details table from a metrics JSON file,
printing all computed values so they can be verified against the web UI.

Usage:
    python3 audit_metrics.py <metrics_file_or_url> [experiment_name]

The source can be a local file path or an HTTP(S) URL.
GitHub blob URLs are auto-converted to raw URLs.

If the input is a combined metrics file and no experiment is given,
lists available experiments.
"""

from __future__ import annotations

import json
import re
import sys
import urllib.request
from typing import Any, Callable, Literal

# A single flattened metric entry: {"group": ..., "air_name": ..., "air_id": ..., "metric": ..., "value": ..., ...}
Entry = dict[str, str]
# Raw metrics JSON with "counter" and "gauge" arrays
MetricsJson = dict[str, Any]
# Computed metrics dict returned by extract_metrics
Metrics = dict[str, Any]


# ============================================================
# Computation — this is the code that needs to be audited.
# It must match the JS in index.html exactly.
# ============================================================

def load_metrics_dataframes(
    metrics_json: MetricsJson,
) -> tuple[list[Entry], list[Entry], list[Entry], list[Entry], list[Entry]]:
    """Port of loadMetricsDataframes: flatten entries, split by group prefix."""
    entries: list[Entry] = []
    for c in metrics_json["counter"] + metrics_json["gauge"]:
        obj = dict(c["labels"])
        obj["metric"] = c["metric"]
        obj["value"] = c["value"]
        entries.append(obj)

    app = [e for e in entries if e.get("group", "").startswith("app_proof")]
    if not app:
        app = [e for e in entries if e.get("group", "").startswith("reth")]
    leaf = [e for e in entries if e.get("group", "").startswith("leaf")]
    internal = [e for e in entries if e.get("group", "").startswith("internal")]
    compression = [e for e in entries if e.get("group", "") == "compression"]

    return entries, app, leaf, internal, compression


def is_normal_instruction_air(name: str) -> bool:
    """Port of isNormalInstructionAir."""
    m = re.match(r"^VmAirWrapper<[^,]+,\s*([^>]+?)(?:<(\d+)(?:,\s*\d+)*>)?\s*>$", name or "")
    if not m:
        return False
    if m.group(1) == "FieldExpressionCoreAir":
        return False
    if m.group(2) and int(m.group(2)) != 4:
        return False
    return True


def sum_metric(entries: list[Entry], metric_name: str) -> float:
    """Sum values for entries matching metric_name."""
    return sum(float(e["value"]) for e in entries if e["metric"] == metric_name)


def unique_metric(entries: list[Entry], metric_name: str) -> float:
    """Get the value of a metric that must appear exactly once."""
    matches = [e for e in entries if e["metric"] == metric_name]
    assert len(matches) == 1, f"Expected exactly 1 entry for '{metric_name}', found {len(matches)}"
    return float(matches[0]["value"])


def detect_version(metrics_json: MetricsJson) -> Literal[1, 2]:
    """Returns 2 if metrics come from OpenVM 2 (GPU or CPU backend), else 1.
    V2 GPU has 'logup_gkr' in metric names; V2 CPU has 'prove_zerocheck_and_logup_time_ms'."""
    names = {e["metric"] for e in metrics_json["counter"] + metrics_json["gauge"]}
    if any("logup_gkr" in n for n in names):
        return 2
    if "prove_zerocheck_and_logup_time_ms" in names:
        return 2
    return 1


def detect_v2_backend(metrics_json: MetricsJson) -> Literal["gpu", "cpu", None]:
    """For V2 runs, identify whether they came from the GPU or CPU prover backend.
    Returns None for V1 runs."""
    names = {e["metric"] for e in metrics_json["counter"] + metrics_json["gauge"]}
    if any("logup_gkr" in n for n in names):
        return "gpu"
    if "prove_zerocheck_and_logup_time_ms" in names:
        return "cpu"
    return None


def extract_metrics(run_name: str, metrics_json: MetricsJson) -> Metrics:
    """Port of extractMetrics from index.html. Returns dict of all computed values."""
    all_entries, app, leaf, internal, compression = load_metrics_dataframes(metrics_json)
    m: Metrics = {}
    m["name"] = run_name

    # --- Classify app AIRs ---
    powdr_air = [e for e in app if (e.get("air_name") or "").startswith("PowdrAir")]
    non_powdr = [e for e in app if not (e.get("air_name") or "").startswith("PowdrAir")]
    normal_air = [e for e in non_powdr if is_normal_instruction_air(e.get("air_name", ""))]
    precompile_air = [e for e in non_powdr if not is_normal_instruction_air(e.get("air_name", ""))]

    # --- Basic stats ---
    m["app_proof_cols"] = sum_metric(app, "main_cols") + sum_metric(app, "prep_cols") + sum_metric(app, "perm_cols")
    segments = [int(e["segment"]) for e in app if "segment" in e]
    m["num_segments"] = max(segments, default=-1) + 1
    m["num_air_instances"] = len([e for e in app if e["metric"] == "rows"])
    m["app_proof_cells"] = sum_metric(app, "total_cells")
    m["app_proof_cells_used"] = sum_metric(app, "total_cells_used")  # V1 only

    # --- Constraints & bus interactions ---
    has_constraints = any(e["metric"] == "constraints" for e in all_entries)
    has_interactions = any(e["metric"] == "interactions" for e in all_entries)

    # Rows & segments by AIR, summed over all segments.
    # We key by (air_id, air_name) because air_id alone is only unique within a proving
    # phase — different phases (app, leaf, compression) reuse the same air_id for
    # unrelated AIRs. Keying by the pair is a pragmatic fix: it would break if the same
    # (air_id, air_name) tuple appeared in two different phases, but that is unlikely
    # since each phase uses a distinct AIR set.
    segments_by_app_air: dict[str, float] = {}
    rows_by_app_air: dict[str, float] = {}
    for e in app:
        # Rows are indicated per segment and AIR
        if e["metric"] == "rows":
            key = f"{e['air_id']}:{e.get('air_name', '')}"
            segments_by_app_air[key] = segments_by_app_air.get(key, 0) + 1
            rows_by_app_air[key] = rows_by_app_air.get(key, 0) + float(e["value"])

    # Constraints and interactions are listed per AIR.
    # For the number of constraints and interactions, we weight by the number of segments for that AIR;
    # for the number of instances and messages, we weight by the number of rows (across all segments).
    def weighted_sum(metric_name: str, weights: dict[str, float]) -> float:
        return sum(
            float(e["value"]) * weights.get(f"{e['air_id']}:{e.get('air_name', '')}", 0)
            for e in all_entries if e["metric"] == metric_name
        )

    m["constraints"] = weighted_sum("constraints", segments_by_app_air) if has_constraints else None
    m["bus_interactions"] = weighted_sum("interactions", segments_by_app_air) if has_interactions else None
    m["constraint_instances"] = weighted_sum("constraints", rows_by_app_air) if has_constraints else None
    m["bus_interaction_messages"] = weighted_sum("interactions", rows_by_app_air) if has_interactions else None

    # --- Proof times by phase ---
    # execute_metered runs *before* segment proving and is outside per-segment
    # total_proof_time_ms. We report it as a separate top-level phase.
    m["execute_metered_time_ms"] = sum_metric(app, "execute_metered_time_ms")
    m["app_proof_time_ms"] = sum_metric(app, "total_proof_time_ms")
    m["leaf_proof_time_ms"] = sum_metric(leaf, "total_proof_time_ms")
    m["inner_recursion_proof_time_ms"] = sum_metric(internal, "total_proof_time_ms")
    m["compression_proof_time_ms"] = sum_metric(compression, "total_proof_time_ms")
    m["total_proof_time_ms"] = (m["execute_metered_time_ms"] + m["app_proof_time_ms"]
        + m["leaf_proof_time_ms"] + m["inner_recursion_proof_time_ms"]
        + m["compression_proof_time_ms"])

    # --- STARK time excluding trace ---
    m["app_proof_time_excluding_trace_ms"] = sum_metric(app, "stark_prove_excluding_trace_time_ms")

    # --- App time sub-components ---
    m["app_execute_preflight_time_ms"] = sum_metric(app, "execute_preflight_time_ms")
    m["app_trace_gen_time_ms"] = sum_metric(app, "trace_gen_time_ms")
    m["app_set_initial_memory_time_ms"] = sum_metric(app, "set_initial_memory_time_ms")  # V2 only

    # --- V2: STARK sub-components ---
    # GPU backend uses hierarchical prover.* names; CPU backend uses flat names.
    # We check both and take whichever is nonzero.
    gpu_trace_commit = sum_metric(app, "prover.main_trace_commit_time_ms")
    cpu_trace_commit = sum_metric(app, "trace_commit_cpu_time_ms")
    m["app_trace_commit_time_ms"] = gpu_trace_commit or cpu_trace_commit

    gpu_constraints = sum_metric(app, "prover.rap_constraints_time_ms")
    cpu_constraints = sum_metric(app, "prove_zerocheck_and_logup_time_ms")
    m["app_rap_constraints_time_ms"] = gpu_constraints or cpu_constraints

    gpu_openings = sum_metric(app, "prover.openings_time_ms")
    cpu_whir = sum_metric(app, "prove_whir_opening_cpu_time_ms")
    cpu_stacked = sum_metric(app, "prove_stacked_opening_reduction_time_ms")
    m["app_openings_time_ms"] = gpu_openings or (cpu_whir + cpu_stacked)

    m["app_stark_other_ms"] = (m["app_proof_time_excluding_trace_ms"]
        - m["app_trace_commit_time_ms"] - m["app_rap_constraints_time_ms"] - m["app_openings_time_ms"])

    # --- V2: rap_constraints sub-components ---
    # GPU: prover.rap_constraints.logup_gkr_time_ms, .round0_time_ms, .mle_rounds_time_ms
    # CPU: fractional_sumcheck_time_ms, (no separate round0), prover.batch_constraints.mle_rounds_time_ms
    gpu_logup_gkr = sum_metric(app, "prover.rap_constraints.logup_gkr_time_ms")
    cpu_logup_gkr = sum_metric(app, "fractional_sumcheck_time_ms")
    m["app_rap_logup_gkr_time_ms"] = gpu_logup_gkr or cpu_logup_gkr

    m["app_rap_round0_time_ms"] = sum_metric(app, "prover.rap_constraints.round0_time_ms")

    gpu_mle = sum_metric(app, "prover.rap_constraints.mle_rounds_time_ms")
    cpu_mle = sum_metric(app, "prover.batch_constraints.mle_rounds_time_ms")
    m["app_rap_mle_rounds_time_ms"] = gpu_mle or cpu_mle

    m["app_rap_other_ms"] = (m["app_rap_constraints_time_ms"]
        - m["app_rap_logup_gkr_time_ms"] - m["app_rap_round0_time_ms"] - m["app_rap_mle_rounds_time_ms"])

    # --- V2: openings sub-components ---
    # GPU: prover.openings.whir_time_ms, prover.openings.stacked_reduction_time_ms
    # CPU: prove_whir_opening_cpu_time_ms, prove_stacked_opening_reduction_time_ms
    gpu_whir = sum_metric(app, "prover.openings.whir_time_ms")
    m["app_openings_whir_time_ms"] = gpu_whir or cpu_whir

    gpu_stacked = sum_metric(app, "prover.openings.stacked_reduction_time_ms")
    m["app_openings_stacked_reduction_time_ms"] = gpu_stacked or cpu_stacked

    m["app_openings_other_ms"] = (m["app_openings_time_ms"]
        - m["app_openings_whir_time_ms"] - m["app_openings_stacked_reduction_time_ms"])

    # --- App other (residual) ---
    # execute_metered is a separate top-level phase, not inside app_proof_time_ms.
    m["app_other_ms"] = (m["app_proof_time_ms"]
        - m["app_proof_time_excluding_trace_ms"]
        - m["app_execute_preflight_time_ms"]
        - m["app_trace_gen_time_ms"] - m["app_set_initial_memory_time_ms"])

    # --- Cell ratios ---
    total = m["app_proof_cells"]
    m["powdr_ratio"] = sum_metric(powdr_air, "cells") / total if total > 0 else 0
    m["normal_instruction_ratio"] = sum_metric(normal_air, "cells") / total if total > 0 else 0
    m["openvm_precompile_ratio"] = sum_metric(precompile_air, "cells") / total if total > 0 else 0

    return m


# ============================================================
# Presentation — formatting and printing (not part of audit)
# ============================================================

Formatter = Callable[[float], str]

# Basic stats row: (key, label, formatter)
BasicRow = tuple[str, str, Formatter]
# Proof time row: (key, label, indent, flags)  — flags: b=bold, r=residual
ProofRow = tuple[str, str, int, str]
# Union of row types used by print_section
Row = BasicRow | ProofRow


def fmt_ms(ms: float) -> str:
    return f"{ms / 1000:.2f}s ({ms:.0f} ms)"

def fmt_cells(v: float) -> str:
    for threshold, suffix in [(1e9, "B"), (1e6, "M"), (1e3, "K")]:
        if v >= threshold:
            return f"{v / threshold:.2f}{suffix} ({v:,.0f})"
    return f"{v:,.0f}"

def fmt_int(v: float) -> str:
    return f"{v:,.0f}"

def fmt_pct(v: float) -> str:
    return f"{v * 100:.1f}%"


BASIC_STATS_V1: list[BasicRow] = [
    ("num_segments",            "Segments",                     lambda v: str(int(v))),
    ("num_air_instances",       "AIR Instances",                fmt_int),
    ("app_proof_cols",          "Columns",                  fmt_int),
    ("app_proof_cells",         "Cells",                    fmt_cells),
    ("app_proof_cells_used",    "Cells (without padding)",  fmt_cells),
    ("constraints",             "Constraints",                  fmt_int),
    ("constraint_instances",    "Constraint Instances",         fmt_cells),
    ("bus_interactions",        "Bus Interactions",              fmt_int),
    ("bus_interaction_messages", "Bus Interaction Messages",     fmt_cells),
]

BASIC_STATS_V2: list[BasicRow] = [r for r in BASIC_STATS_V1 if r[0] != "app_proof_cells_used"]

PROOF_TIME_V1: list[ProofRow] = [
    ("execute_metered_time_ms",          "Metered Execution",     0, ""),
    ("app_proof_time_ms",                "App Proof Time",        0, ""),
    ("app_proof_time_excluding_trace_ms","  STARK (excl. trace)", 1, ""),
    ("app_execute_preflight_time_ms",    "  Preflight Execution", 1, ""),
    ("app_trace_gen_time_ms",            "  Trace Gen",           1, ""),
    ("app_other_ms",                     "  Other / Overlap",     1, "r"),
    ("leaf_proof_time_ms",               "Leaf Recursion",        0, ""),
    ("inner_recursion_proof_time_ms",    "Inner Recursion",       0, ""),
    ("total_proof_time_ms",              "Total",                 0, ""),
]

PROOF_TIME_V2: list[ProofRow] = [
    ("execute_metered_time_ms",              "Metered Execution",     0, ""),
    ("app_proof_time_ms",                    "App Proof Time",        0, ""),
    ("app_proof_time_excluding_trace_ms",    "  STARK (excl. trace)", 1, ""),
    ("app_rap_constraints_time_ms",          "    Constraints",       2, ""),
    ("app_rap_logup_gkr_time_ms",           "      LogUp GKR",       3, ""),
    ("app_rap_round0_time_ms",              "      Round 0",         3, "z"),
    ("app_rap_mle_rounds_time_ms",          "      MLE Rounds",      3, ""),
    ("app_rap_other_ms",                    "      Other",           3, "r"),
    ("app_openings_time_ms",                "    Openings",          2, ""),
    ("app_openings_whir_time_ms",           "      WHIR",            3, ""),
    ("app_openings_stacked_reduction_time_ms","      Stacked Reduction", 3, ""),
    ("app_openings_other_ms",               "      Other",           3, "r"),
    ("app_trace_commit_time_ms",            "    Trace Commit",      2, ""),
    ("app_stark_other_ms",                  "    Other",             2, "r"),
    ("app_execute_preflight_time_ms",       "  Preflight Execution", 1, ""),
    ("app_set_initial_memory_time_ms",      "  Set Initial Memory",  1, "z"),
    ("app_trace_gen_time_ms",               "  Trace Gen",           1, ""),
    ("app_other_ms",                        "  Other",               1, "r"),
    ("leaf_proof_time_ms",                  "Leaf Recursion",        0, ""),
    ("inner_recursion_proof_time_ms",       "Inner Recursion",       0, ""),
    ("compression_proof_time_ms",           "Compression",           0, ""),
    ("total_proof_time_ms",                 "Total",                 0, ""),
]

CELL_DISTRIBUTION: list[BasicRow] = [
    ("powdr_ratio",               "Powdr",              fmt_pct),
    ("normal_instruction_ratio",  "Normal Instructions", fmt_pct),
    ("openvm_precompile_ratio",   "OpenVM Precompiles",  fmt_pct),
]


def print_section(
    title: str, rows: list[Row], m: Metrics, *, pct_of_key: str | None = None
) -> None:
    print(f"\n  {title}")
    print(f"  {'─' * 58}")
    width = max(len(r[1]) for r in rows)
    total = m.get(pct_of_key, 0) if pct_of_key else 0

    for row in rows:
        key, label = row[0], row[1]
        val: float | None = m.get(key)

        if key == "total_proof_time_ms":
            print(f"  {'─' * 58}")

        # Determine formatter and flags
        if len(row) == 3:
            fmt: Formatter = row[2]  # type: ignore[assignment]
            flags = ""
        else:
            fmt = fmt_ms
            flags: str = row[3]  # type: ignore[no-redef]

        # Hide rows flagged with 'z' when value is zero or missing
        if "z" in flags and (val is None or val == 0):
            continue

        if val is None:
            print(f"  {label:<{width}}  N/A")
            continue

        suffix = " (residual)" if "r" in flags else ""

        pct = f"  ({val / total * 100:5.1f}%)" if total > 0 else ""
        print(f"  {label:<{width}}  {fmt(val)}{pct}{suffix}")


# ============================================================
# Data loading (IO)
# ============================================================

def load_data(source: str) -> tuple[dict[str, Any], str]:
    """Load JSON from a file path or URL. Returns (data, source_label)."""
    if source.startswith("http://") or source.startswith("https://"):
        url = re.sub(r"github\.com/(.+)/blob/", r"raw.githubusercontent.com/\1/", source)
        with urllib.request.urlopen(url) as resp:
            return json.loads(resp.read()), url.split("/")[-1]
    else:
        with open(source) as f:
            return json.load(f), source.split("/")[-1]


def resolve_experiments(
    data: dict[str, Any], source_label: str, experiment: str | None
) -> dict[str, MetricsJson]:
    """Normalize raw/combined input and select experiment(s). Returns dict of {name: json}."""
    if "counter" in data and "gauge" in data:
        name = experiment or source_label.replace(".json", "")
        return {name: data}

    if experiment:
        if experiment not in data:
            sys.exit(f"Error: '{experiment}' not found. Available: {', '.join(sorted(data))}")
        return {experiment: data[experiment]}

    if len(data) == 1:
        return data

    print(f"Combined file with {len(data)} experiments:")
    for name in sorted(data):
        print(f"  - {name}")
    print(f"\nUsage: {sys.argv[0]} {sys.argv[1]} <experiment_name>")
    sys.exit(0)


# ============================================================
# Main
# ============================================================

def main() -> None:
    if len(sys.argv) < 2:
        print(__doc__.strip())
        sys.exit(1)

    data, source_label = load_data(sys.argv[1])
    experiment = sys.argv[2] if len(sys.argv) > 2 else None
    runs = resolve_experiments(data, source_label, experiment)

    for run_name, metrics_json in runs.items():
        version = detect_version(metrics_json)
        backend = detect_v2_backend(metrics_json)
        m = extract_metrics(run_name, metrics_json)

        version_str = f"OpenVM {version}"
        if backend:
            version_str += f" ({backend.upper()})"
        print(f"\nExperiment: {run_name}  ({version_str})")

        basic = BASIC_STATS_V2 if version == 2 else BASIC_STATS_V1
        proof = PROOF_TIME_V2 if version == 2 else PROOF_TIME_V1

        print_section("App Proof Basic Stats", basic, m)
        print_section("Proof Time", proof, m, pct_of_key="total_proof_time_ms")
        print_section("Trace Cell Distribution", CELL_DISTRIBUTION, m)


if __name__ == "__main__":
    main()

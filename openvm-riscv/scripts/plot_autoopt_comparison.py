#!/usr/bin/env python3
"""Bar charts comparing STARK proving time (excl. trace generation) between the
baseline and autoopt GPU provers.

Reads the `total_proof_time_excluding_trace_ms` column from each experiment's
`basic_metrics.csv` in two result trees (baseline + autoopt) and emits grouped
bar charts (two bars — baseline vs autoopt — per experiment).

Usage:
    plot_autoopt_comparison.py <baseline-dir> <autoopt-dir> <output-dir>
"""
from __future__ import annotations

import csv
import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

# (label, experiment subdir, run-name prefix). The run file is
# "<prefix>apcNNN/metrics.json" for guests, "<prefix>apcNNN.json" for reth.
EXPERIMENTS = [
    ("keccak", "keccak", ""),
    ("sha256", "sha256", ""),
    ("u256", "u256", ""),
    ("matmul", "matmul", ""),
    ("pairing", "pairing", ""),
    ("ecc-proj", "ecc", "projective-"),
    ("ecc-affine", "ecc", "affine-hint-"),
    ("ecrecover", "ecrecover", ""),
    ("reth", "reth_gpu", ""),
]

# APC counts present for every experiment (guests fail >100 in some cases; reth
# goes higher but those counts aren't shared, so the reth sweep gets its own chart).
SHARED_APCS = [0, 3, 10, 30, 100]
RETH_APCS = [0, 3, 10, 30, 100, 300, 500]

BASELINE_COLOR = "#9e9e9e"
AUTOOPT_COLOR = "#1f77b4"
PROOF_COL = "total_proof_time_excluding_trace_ms"


def normalize(filename: str) -> str:
    """Reduce a metrics filename to its run id (e.g. 'apc010', 'projective-apc010').

    The two result trees store different filename forms for the same run
    ('reth_gpu/apc000.json' vs 'apc000.json', 'apc010/metrics.json'), so match
    on the run id rather than the raw path.
    """
    parts = filename.split("/")
    if parts[-1] == "metrics.json":
        return parts[-2]
    return parts[-1].removesuffix(".json")


def load_proof_secs(root: Path, subdir: str) -> dict[str, float]:
    """Map run id -> STARK-excl-trace seconds for one experiment."""
    csv_path = root / subdir / "basic_metrics.csv"
    out: dict[str, float] = {}
    if not csv_path.exists():
        return out
    with open(csv_path) as f:
        for row in csv.DictReader(f):
            if row.get("filename") and row.get(PROOF_COL):
                out[normalize(row["filename"])] = float(row[PROOF_COL]) / 1000.0
    return out


def run_key(subdir: str, prefix: str, apc: int) -> str:
    return f"{prefix}apc{apc:03d}"


def grouped_bars(ax, labels, baseline, autoopt, title):
    """Two bars per label; annotate seconds, and % delta on the autoopt bar."""
    xs = range(len(labels))
    w = 0.4
    nan = float("nan")
    ax.bar([x - w / 2 for x in xs], [v if v is not None else nan for v in baseline], w,
           label="baseline", color=BASELINE_COLOR)
    ax.bar([x + w / 2 for x in xs], [v if v is not None else nan for v in autoopt], w,
           label="autoopt", color=AUTOOPT_COLOR)

    top = max([v for v in baseline + autoopt if v is not None], default=1.0)
    for x, b, a in zip(xs, baseline, autoopt):
        if b is not None:
            ax.text(x - w / 2, b + top * 0.01, f"{b:.1f}", ha="center", va="bottom", fontsize=7)
        if a is not None:
            label = f"{a:.1f}"
            if b:
                delta = (a / b - 1) * 100
                color = "#2e7d32" if delta < 0 else "#c62828"
                ax.text(x + w / 2, a + top * 0.07, f"{delta:+.0f}%", ha="center",
                        va="bottom", fontsize=7, color=color, fontweight="bold")
            ax.text(x + w / 2, a + top * 0.01, label, ha="center", va="bottom", fontsize=7)

    ax.set_xticks(list(xs))
    ax.set_xticklabels(labels, rotation=30, ha="right", fontsize=8)
    ax.set_ylabel("STARK proving time\n(excl. trace gen) [s]", fontsize=8)
    ax.set_ylim(0, top * 1.2)
    ax.set_title(title, fontsize=10)
    ax.grid(axis="y", linestyle=":", alpha=0.4)


def main() -> None:
    baseline_dir, autoopt_dir, out_dir = (Path(p) for p in sys.argv[1:4])
    out_dir.mkdir(parents=True, exist_ok=True)

    base = {label: load_proof_secs(baseline_dir, sub) for label, sub, _ in EXPERIMENTS}
    auto = {label: load_proof_secs(autoopt_dir, sub) for label, sub, _ in EXPERIMENTS}

    # Chart 1: one subplot per shared APC count, x = experiment, two bars each.
    fig, axes = plt.subplots(len(SHARED_APCS), 1, figsize=(11, 3.1 * len(SHARED_APCS)))
    for ax, apc in zip(axes, SHARED_APCS):
        labels, b_vals, a_vals = [], [], []
        for label, sub, prefix in EXPERIMENTS:
            key = run_key(sub, prefix, apc)
            labels.append(label)
            b_vals.append(base[label].get(key))
            a_vals.append(auto[label].get(key))
        grouped_bars(ax, labels, b_vals, a_vals,
                     f"STARK proving time excl. trace — {apc} autoprecompiles")
    axes[0].legend(loc="upper right", fontsize=8)
    fig.suptitle("Baseline vs. autoopt GPU prover (per experiment, per APC count)", fontsize=12)
    fig.tight_layout(rect=(0, 0, 1, 0.99))
    fig.savefig(out_dir / "stark_compare_by_apc.png", dpi=130)
    plt.close(fig)

    # Chart 2: reth across its full sweep (its autoopt win lives at 300/500,
    # which aren't shared with the guests, so it gets a dedicated chart).
    rb = base["reth"]
    ra = auto["reth"]
    labels = [str(apc) for apc in RETH_APCS]
    b_vals = [rb.get(run_key("reth_gpu", "", apc)) for apc in RETH_APCS]
    a_vals = [ra.get(run_key("reth_gpu", "", apc)) for apc in RETH_APCS]
    fig, ax = plt.subplots(figsize=(9, 4.5))
    grouped_bars(ax, labels, b_vals, a_vals,
                 "reth — STARK proving time excl. trace, baseline vs. autoopt")
    ax.set_xlabel("autoprecompiles")
    ax.legend(loc="upper left", fontsize=8)
    fig.tight_layout()
    fig.savefig(out_dir / "stark_compare_reth_sweep.png", dpi=130)
    plt.close(fig)

    print(f"wrote {out_dir}/stark_compare_by_apc.png and stark_compare_reth_sweep.png")


if __name__ == "__main__":
    main()

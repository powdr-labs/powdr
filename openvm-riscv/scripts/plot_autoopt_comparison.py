#!/usr/bin/env python3
"""Bar charts comparing STARK proving time (excl. trace generation) between the
baseline and autoopt GPU provers.

Emits one chart per experiment (one per metrics-viewer link): x-axis = number of
autoprecompiles, two bars per count (baseline vs autoopt), y = STARK proving
time excluding trace generation. ecc has two variants (projective, affine-hint)
under a single experiment dir, so its chart gets one subplot per variant.

Usage:
    plot_autoopt_comparison.py <baseline-dir> <autoopt-dir> <output-dir>
"""
from __future__ import annotations

import csv
import re
import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

# (output-file label, experiment subdir, [(variant label, run-name prefix), ...]).
# Order mirrors the readme's metrics-viewer links (reth first, then alphabetical).
EXPERIMENTS = [
    ("reth", "reth_gpu", [("", "")]),
    ("ecc", "ecc", [("projective", "projective-"), ("affine-hint", "affine-hint-")]),
    ("ecrecover", "ecrecover", [("", "")]),
    ("keccak", "keccak", [("", "")]),
    ("matmul", "matmul", [("", "")]),
    ("pairing", "pairing", [("", "")]),
    ("sha256", "sha256", [("", "")]),
    ("u256", "u256", [("", "")]),
]

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


def apc_counts(d: dict[str, float], prefix: str) -> set[int]:
    pat = re.compile(rf"^{re.escape(prefix)}apc(\d+)$")
    return {int(m.group(1)) for k in d if (m := pat.match(k))}


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
            ax.text(x - w / 2, b + top * 0.01, f"{b:.1f}", ha="center", va="bottom", fontsize=8)
        if a is not None:
            if b:
                delta = (a / b - 1) * 100
                color = "#2e7d32" if delta < 0 else "#c62828"
                ax.text(x + w / 2, a + top * 0.07, f"{delta:+.0f}%", ha="center",
                        va="bottom", fontsize=8, color=color, fontweight="bold")
            ax.text(x + w / 2, a + top * 0.01, f"{a:.1f}", ha="center", va="bottom", fontsize=8)

    ax.set_xticks(list(xs))
    ax.set_xticklabels(labels, fontsize=9)
    ax.set_xlabel("autoprecompiles", fontsize=9)
    ax.set_ylabel("STARK proving time\n(excl. trace gen) [s]", fontsize=9)
    ax.set_ylim(0, top * 1.22)
    ax.set_title(title, fontsize=11)
    ax.grid(axis="y", linestyle=":", alpha=0.4)


def main() -> None:
    baseline_dir, autoopt_dir, out_dir = (Path(p) for p in sys.argv[1:4])
    out_dir.mkdir(parents=True, exist_ok=True)

    for label, subdir, variants in EXPERIMENTS:
        base = load_proof_secs(baseline_dir, subdir)
        auto = load_proof_secs(autoopt_dir, subdir)

        fig, axes = plt.subplots(1, len(variants), squeeze=False)
        for ax, (vlabel, prefix) in zip(axes[0], variants):
            counts = sorted(apc_counts(base, prefix) | apc_counts(auto, prefix))
            keys = [f"{prefix}apc{c:03d}" for c in counts]
            b_vals = [base.get(k) for k in keys]
            a_vals = [auto.get(k) for k in keys]
            title = label if not vlabel else f"{label} ({vlabel})"
            grouped_bars(ax, [str(c) for c in counts], b_vals, a_vals,
                         f"{title} — STARK proving time excl. trace")
            ax.legend(loc="upper left", fontsize=8)
        # ~3.7 in per APC count per subplot column, clamped to a sane range.
        ncols = len(variants)
        width = sum(max(5.0, 0.9 * len(apc_counts(base, p) | apc_counts(auto, p)) + 2.0)
                    for _, p in variants)
        fig.set_size_inches(width, 4.6)
        fig.tight_layout()
        fig.savefig(out_dir / f"stark_{label}.png", dpi=130)
        plt.close(fig)

    labels = ", ".join(e[0] for e in EXPERIMENTS)
    print(f"wrote {out_dir}/stark_<experiment>.png for: {labels}")


if __name__ == "__main__":
    main()

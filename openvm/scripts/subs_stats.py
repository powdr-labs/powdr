#!/usr/bin/env python3
"""
Aggregate APC substitution stats from NDJSON produced by the CPU trace generator.

Per-air histograms:
- Bins: post-optimization widths
- Height: occurrences of that width multiplied by the APC's num_calls
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from pathlib import Path
from typing import Dict, Iterable, Tuple

import matplotlib

# Non-interactive backend for headless environments
matplotlib.use("Agg")
import matplotlib.pyplot as plt


def load_entries(path: Path) -> Iterable[dict]:
    with path.open() as f:
        for idx, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            try:
                yield json.loads(line)
            except json.JSONDecodeError as err:
                print(f"[WARN] Skipping line {idx}: {err}", file=sys.stderr)


def aggregate_per_air(entries: Iterable[dict]) -> Dict[str, Tuple[int, Counter]]:
    per_air: Dict[str, Tuple[int, Counter]] = {}
    for entry in entries:
        num_calls = entry.get("num_calls", 1)
        airs = entry.get("airs", {})
        if not isinstance(airs, dict):
            continue

        for air_name, pair in airs.items():
            if not (isinstance(pair, list) and len(pair) == 2):
                continue
            before_width, after_widths = pair
            if not isinstance(after_widths, list):
                continue

            if air_name not in per_air:
                per_air[air_name] = (before_width, Counter())
            else:
                known_before, _ = per_air[air_name]
                if known_before != before_width:
                    print(
                        f"[WARN] Inconsistent before_width for {air_name}: "
                        f"{known_before} vs {before_width}; keeping first.",
                        file=sys.stderr,
                    )
                    before_width = known_before

            _, hist = per_air[air_name]
            for width in after_widths:
                hist[width] += num_calls

    return per_air


def log_per_air(per_air: Dict[str, Tuple[int, Counter]]) -> None:
    for air_name in sorted(per_air):
        before_width, hist = per_air[air_name]
        print(f"AIR {air_name} (before width {before_width}):")
        for width, freq in sorted(hist.items()):
            print(f"  after width {width}: freq {freq}")


def sanitize_name(name: str) -> str:
    return "".join(ch if ch.isalnum() or ch in "-._" else "_" for ch in name)


def write_histograms(per_air: Dict[str, Tuple[int, Counter]], out_dir: Path) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    for air_name, (before_width, hist) in per_air.items():
        data = {
            "air": air_name,
            "before_width": before_width,
            "histogram": dict(hist),
        }
        base = out_dir / sanitize_name(air_name)

        # JSON dump
        with (base.with_suffix(".json")).open("w") as f:
            json.dump(data, f)
            f.write("\n")

        # Graph output
        if hist:
            widths, freqs = zip(*sorted(hist.items()))
            max_x = before_width
            total = sum(freqs)
            plt.figure(figsize=(8, 4.5))
            plt.bar(widths, freqs, width=0.8, align="center", color="#4C72B0")
            plt.title(f"{air_name}\n(before width {before_width})")
            plt.xlabel("Post-optimization width")
            plt.ylabel("Weighted frequency (occurrences * num_calls)")
            plt.grid(True, axis="y", alpha=0.3)
            plt.xlim(-0.5, max_x + 0.5)
            plt.xticks(range(0, max_x + 1, 1))

            for x, freq in zip(widths, freqs):
                pct = (freq / total * 100.0) if total else 0.0
                plt.text(
                    x,
                    freq,
                    f"{pct:.1f}%",
                    ha="center",
                    va="bottom",
                    fontsize=8,
                    rotation=0,
                )

            plt.tight_layout()
            plt.savefig(base.with_suffix(".png"))
            plt.close()


def write_summary(per_air: Dict[str, Tuple[int, Counter]], out_dir: Path) -> None:
    if not per_air:
        return

    summary = Counter()
    max_before = 0
    for _, (before_width, hist) in per_air.items():
        max_before = max(max_before, before_width)
        for width, freq in hist.items():
            summary[width] += freq * before_width

    if not summary:
        return

    widths, freqs = zip(*sorted(summary.items()))
    total = sum(freqs)
    plt.figure(figsize=(8, 4.5))
    plt.bar(widths, freqs, width=0.8, align="center", color="#DD8452")
    plt.title("Summary (weighted by before width)")
    plt.xlabel("Post-optimization width")
    plt.ylabel("Weighted frequency (freq * before_width * num_calls)")
    plt.grid(True, axis="y", alpha=0.3)
    plt.xlim(-0.5, max_before + 0.5)
    plt.xticks(range(0, max_before + 1, 1))

    for x, freq in zip(widths, freqs):
        pct = (freq / total * 100.0) if total else 0.0
        plt.text(
            x,
            freq,
            f"{pct:.1f}%",
            ha="center",
            va="bottom",
            fontsize=8,
            rotation=0,
        )

    plt.tight_layout()

    out_dir.mkdir(parents=True, exist_ok=True)
    plt.savefig(out_dir / "summary.png")
    plt.close()


def main() -> None:
    default_path = Path(__file__).resolve().parent.parent / "apc_subs_stats.json"
    default_out_dir = Path(__file__).resolve().parent.parent / "apc_subs_stats"
    parser = argparse.ArgumentParser(
        description="Aggregate per-air histograms from APC subs NDJSON."
    )
    parser.add_argument(
        "path",
        nargs="?",
        type=Path,
        default=default_path,
        help=f"NDJSON input file (default: {default_path})",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=default_out_dir,
        help=f"Directory for per-air histogram files (default: {default_out_dir})",
    )
    args = parser.parse_args()

    if not args.path.exists():
        print(f"[ERROR] File not found: {args.path}", file=sys.stderr)
        sys.exit(1)

    entries = load_entries(args.path)
    per_air = aggregate_per_air(entries)
    log_per_air(per_air)
    write_histograms(per_air, args.out_dir)
    write_summary(per_air, args.out_dir)


if __name__ == "__main__":
    main()

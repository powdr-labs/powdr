#!/usr/bin/env python3
"""Compare APC effectiveness of this Lean run against a nightly run.

For every apc_candidates.json in this directory, the script fetches the file at
the same relative path from the given nightly run (on the bench-results gh-pages
branch) and computes the mean main-column and bus-interaction effectiveness for
both, using the same weighted-mean formula as the APC effectiveness analyzer
(autoprecompile-analyzer/index.html).

The analyzer computes, per cost metric:

    metric_before = stats.before[metric] * execution_frequency
    metric_after  = stats.after[metric]  * execution_frequency
    effectiveness = stats.before[metric] / stats.after[metric]
    mean          = sum(effectiveness * metric_after) / sum(metric_after)

which is algebraically sum(metric_before) / sum(metric_after).

Usage:
    python3 compare_to_nightly.py [nightly-run-id]

Default nightly-run-id: 2026-07-15-0446
"""

import json
import sys
import urllib.request
from pathlib import Path
from urllib.parse import quote

RAW_BASE = "https://raw.githubusercontent.com/powdr-labs/bench-results/gh-pages/results"
HERE = Path(__file__).resolve().parent

ANALYZER = "https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data="
NIGHTLY_BLOB_BASE = "https://github.com/powdr-labs/bench-results/blob/gh-pages/results"
# This Lean run lives alongside the nightly runs on the bench-results gh-pages branch.
LEAN_BLOB_BASE = f"{NIGHTLY_BLOB_BASE}/2026-07-20-0557-lean"


def analyzer_link(blob_url):
    return ANALYZER + quote(blob_url, safe="")


def load_apcs(obj):
    """Return the list of APC entries, supporting the version-4 format used here."""
    if isinstance(obj, dict):
        return obj["apcs"]
    return obj  # bare list (older formats)


def mean_effectiveness(apcs, metric):
    """Weighted-mean effectiveness for a cost metric, exactly as the analyzer does."""
    total_before = 0.0
    total_after = 0.0
    for apc in apcs:
        freq = apc["execution_frequency"]
        total_before += apc["stats"]["before"][metric] * freq
        total_after += apc["stats"]["after"][metric] * freq
    return total_before / total_after if total_after else float("nan")


def fetch_nightly(nightly_id, relpath):
    url = f"{RAW_BASE}/{nightly_id}/{relpath}"
    with urllib.request.urlopen(url) as resp:
        return json.loads(resp.read().decode())


def main():
    nightly_id = sys.argv[1] if len(sys.argv) > 1 else "2026-07-15-0446"

    rows = []
    for path in sorted(HERE.rglob("apc_candidates.json")):
        relpath = path.relative_to(HERE).as_posix()
        parts = relpath.split("/")
        benchmark = parts[0]
        run = parts[1] if len(parts) > 2 else "-"

        lean_apcs = load_apcs(json.loads(path.read_text()))
        try:
            nightly_apcs = load_apcs(fetch_nightly(nightly_id, relpath))
        except Exception as e:  # noqa: BLE001 - report and skip missing counterparts
            print(f"skip {relpath}: nightly not available ({e})", file=sys.stderr)
            continue

        rows.append({
            "benchmark": benchmark,
            "run": run,
            "col_nightly": mean_effectiveness(nightly_apcs, "main_columns"),
            "col_lean": mean_effectiveness(lean_apcs, "main_columns"),
            "bus_nightly": mean_effectiveness(nightly_apcs, "bus_interactions"),
            "bus_lean": mean_effectiveness(lean_apcs, "bus_interactions"),
            "nightly_link": analyzer_link(f"{NIGHTLY_BLOB_BASE}/{nightly_id}/{relpath}"),
            "lean_link": analyzer_link(f"{LEAN_BLOB_BASE}/{relpath}"),
        })

    print(f"Comparison vs nightly {nightly_id}\n")
    header = (
        "| Benchmark | Run | Column eff. (nightly → lean) "
        "| Bus eff. (nightly → lean) | Analyzer |"
    )
    print(header)
    print("| --- | --- | --- | --- | --- |")
    for r in rows:
        col = fmt_pair(r["col_nightly"], r["col_lean"])
        bus = fmt_pair(r["bus_nightly"], r["bus_lean"])
        links = f"[nightly]({r['nightly_link']}) · [lean]({r['lean_link']})"
        print(f"| {r['benchmark']} | {r['run']} | {col} | {bus} | {links} |")


def fmt_pair(nightly, lean):
    """Format 'nightly → lean', bolding the larger value (neither if tied)."""
    n, l = f"{nightly:.2f}", f"{lean:.2f}"
    if n != l:
        if nightly > lean:
            n = f"**{n}**"
        else:
            l = f"**{l}**"
    return f"{n} → {l}"


if __name__ == "__main__":
    main()

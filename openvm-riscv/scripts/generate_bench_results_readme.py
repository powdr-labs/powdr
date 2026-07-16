from __future__ import annotations

from argparse import ArgumentParser
from pathlib import Path
from urllib.parse import quote

BENCH_RESULTS_BLOB_BASE = "https://github.com/powdr-labs/bench-results/blob/gh-pages"
BENCH_RESULTS_TREE_BASE = "https://github.com/powdr-labs/bench-results/tree/gh-pages"
APC_ANALYZER_BASE = "https://powdr-labs.github.io/powdr/autoprecompile-analyzer/"
METRICS_VIEWER_BASE = "https://powdr-labs.github.io/powdr/openvm/metrics-viewer/"


def github_blob_url(relative_path: Path, run_id: str) -> str:
    path = Path("results") / run_id / relative_path
    return f"{BENCH_RESULTS_BLOB_BASE}/{path.as_posix()}"


def github_tree_url(run_id: str, subdir: str | None = None) -> str:
    path = Path("results") / run_id
    if subdir:
        path = path / subdir
    return f"{BENCH_RESULTS_TREE_BASE}/{path.as_posix()}"


def viewer_url(viewer_base: str, data_url: str) -> str:
    return f"{viewer_base}?data={quote(data_url, safe='')}"


def find_apc_candidates(experiment_dir: Path) -> Path | None:
    # All apc_candidates.json files within an experiment should be identical
    # (with pgo=cell, all APCs are computed regardless of how many are selected),
    # so we just pick any one deterministically.
    candidates = sorted(experiment_dir.glob("**/apc_candidates.json"))
    if not candidates:
        return None

    return min(
        candidates,
        key=lambda path: (len(path.relative_to(experiment_dir).parts), path.as_posix()),
    )


def fmt_hms(seconds: int) -> str:
    m, s = divmod(seconds, 60)
    return f"{m}m{s:02d}s" if m else f"{s}s"


def apc_generation_time_lines(results_dir: Path) -> list[str]:
    """Table of recorded APC generation times, or [] if none were recorded.

    The bench scripts write `apc_generation_time_s.txt` (integer seconds) into
    each APC run dir when `POWDR_BENCH_TIME_APCS=1`. Absent that env var (e.g. a
    normal nightly), no files exist and this section is omitted.
    """
    rows: list[tuple[str, str, int]] = []
    for experiment_dir in sorted(p for p in results_dir.iterdir() if p.is_dir()):
        for time_file in sorted(experiment_dir.glob("**/apc_generation_time_s.txt")):
            run = time_file.parent.relative_to(experiment_dir).as_posix()
            run = "all APCs" if run == "." else run
            try:
                seconds = int(time_file.read_text().strip())
            except ValueError:
                continue
            rows.append((experiment_dir.name, run, seconds))

    if not rows:
        return []

    total = sum(s for _, _, s in rows)
    lines = [
        "## APC generation time per benchmark",
        "",
        "Wall-clock time of the APC generation stage (build + rank + optimize",
        "candidates) per benchmark:",
        "",
        "| Benchmark | Run | APC generation time |",
        "| --- | --- | --- |",
    ]
    lines += [f"| {exp} | {run} | {fmt_hms(sec)} |" for exp, run, sec in rows]
    lines += [f"| **total** | | **{fmt_hms(total)}** |", ""]
    return lines


def generate_readme(results_dir: Path, run_id: str, note: str | None = None) -> str:
    experiments: list[dict[str, str]] = []

    for experiment_dir in sorted(path for path in results_dir.iterdir() if path.is_dir()):
        name = experiment_dir.name
        metrics_path = experiment_dir / "combined_metrics.json"
        apc_path = find_apc_candidates(experiment_dir)

        entry: dict[str, str] = {"name": name}

        if metrics_path.exists():
            metrics_data_url = github_blob_url(metrics_path.relative_to(results_dir), run_id)
            entry["metrics_url"] = viewer_url(METRICS_VIEWER_BASE, metrics_data_url)

        if apc_path is not None:
            apc_data_url = github_blob_url(apc_path.relative_to(results_dir), run_id)
            entry["apc_url"] = viewer_url(APC_ANALYZER_BASE, apc_data_url)

        entry["tree_url"] = github_tree_url(run_id, name)

        experiments.append(entry)

    # Put reth first if present, then the rest alphabetically.
    experiments.sort(key=lambda e: (0 if e["name"] == "reth" else 1, e["name"]))

    lines = [
        f"# Bench results — {run_id}",
        "",
    ]

    if note:
        lines += [note, ""]

    for exp in experiments:
        name = exp["name"]
        links = [f"📂 [Raw data]({exp['tree_url']})"]
        if "metrics_url" in exp:
            links.append(f"📊 [Metrics Viewer]({exp['metrics_url']})")
        if "apc_url" in exp:
            links.append(f"🔍 [APC Analyzer]({exp['apc_url']})")
        lines.append(f"**{name}**: " + " &nbsp;|&nbsp; ".join(links))
        lines.append("")

    lines += apc_generation_time_lines(results_dir)

    return "\n".join(lines)


def main() -> None:
    parser = ArgumentParser(description="Generate a README for a published bench-results run.")
    parser.add_argument("results_dir", type=Path)
    parser.add_argument("run_id")
    parser.add_argument("--output", type=Path, default=None)
    parser.add_argument(
        "--note",
        default=None,
        help="Optional markdown note inserted below the title (e.g. how a Lean run relates to nightly).",
    )
    args = parser.parse_args()

    readme = generate_readme(args.results_dir, args.run_id, args.note)

    if args.output is None:
        print(readme, end="")
    else:
        args.output.write_text(readme)


if __name__ == "__main__":
    main()

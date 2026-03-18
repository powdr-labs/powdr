from __future__ import annotations

from argparse import ArgumentParser
from pathlib import Path
from urllib.parse import quote

BENCH_RESULTS_BLOB_BASE = "https://github.com/powdr-labs/bench-results/blob/gh-pages"
APC_ANALYZER_BASE = "https://powdr-labs.github.io/powdr/autoprecompile-analyzer/"
METRICS_VIEWER_BASE = "https://powdr-labs.github.io/powdr/openvm/metrics-viewer/"


def github_blob_url(relative_path: Path, run_id: str) -> str:
    path = Path("results") / run_id / relative_path
    return f"{BENCH_RESULTS_BLOB_BASE}/{path.as_posix()}"


def viewer_url(viewer_base: str, data_url: str) -> str:
    return f"{viewer_base}?data={quote(data_url, safe='')}"


def find_apc_candidates(experiment_dir: Path) -> Path | None:
    candidates = sorted(experiment_dir.glob("**/apc_candidates.json"))
    if not candidates:
        return None

    return min(
        candidates,
        key=lambda path: (len(path.relative_to(experiment_dir).parts), path.as_posix()),
    )


def generate_readme(results_dir: Path, run_id: str) -> str:
    rows: list[str] = []

    for experiment_dir in sorted(path for path in results_dir.iterdir() if path.is_dir()):
        metrics_path = experiment_dir / "combined_metrics.json"
        apc_path = find_apc_candidates(experiment_dir)

        metrics_link = "—"
        if metrics_path.exists():
            metrics_data_url = github_blob_url(metrics_path.relative_to(results_dir), run_id)
            metrics_link = (
                f"[metrics viewer]({viewer_url(METRICS_VIEWER_BASE, metrics_data_url)})"
            )

        apc_link = "—"
        if apc_path is not None:
            apc_data_url = github_blob_url(apc_path.relative_to(results_dir), run_id)
            apc_link = f"[apc analyzer]({viewer_url(APC_ANALYZER_BASE, apc_data_url)})"

        rows.append(f"| {experiment_dir.name} | {metrics_link} | {apc_link} |")

    lines = [
        f"# Nightly bench results for {run_id}",
        "",
        "This file links each experiment in this nightly run to the hosted metrics viewers.",
        "",
        "| Experiment | OpenVM metrics viewer | APC analyzer |",
        "| --- | --- | --- |",
        *rows,
    ]
    return "\n".join(lines) + "\n"


def main() -> None:
    parser = ArgumentParser(description="Generate a README for a published bench-results run.")
    parser.add_argument("results_dir", type=Path)
    parser.add_argument("run_id")
    parser.add_argument("--output", type=Path, default=None)
    args = parser.parse_args()

    readme = generate_readme(args.results_dir, args.run_id)

    if args.output is None:
        print(readme, end="")
    else:
        args.output.write_text(readme)


if __name__ == "__main__":
    main()

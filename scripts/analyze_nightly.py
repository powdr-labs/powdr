#!/usr/bin/env python3
"""
Nightly regression analyzer for benchmark results.

This script analyzes the latest nightly benchmark results and compares them
to the previous nightly run. It reports any performance regressions.

Results are fetched from: https://github.com/powdr-labs/bench-results/tree/gh-pages/results
"""

import argparse
import re
import sys
from dataclasses import dataclass
from io import StringIO
from typing import Optional
from urllib.request import urlopen, Request
from urllib.error import URLError, HTTPError
import json

import pandas as pd


GITHUB_API_BASE = "https://api.github.com/repos/powdr-labs/bench-results"
RAW_CONTENT_BASE = "https://raw.githubusercontent.com/powdr-labs/bench-results/gh-pages"

# Benchmarks to analyze
BENCHMARKS = ["keccak", "sha256", "pairing", "u256", "matmul", "ecc", "ecrecover", "reth"]

# Date pattern for result directories (YYYY-MM-DD-HHMM)
DATE_PATTERN = re.compile(r"^\d{4}-\d{2}-\d{2}-\d{4}$")

# Pattern to extract APC count from config name (e.g., "apc030" -> 30)
APC_PATTERN = re.compile(r"apc(\d+)")


def is_apc_config(config: str) -> bool:
    """Check if a config uses APCs (apc count > 0)."""
    match = APC_PATTERN.search(config)
    if match:
        return int(match.group(1)) > 0
    return False


def get_apc_count(config: str) -> Optional[int]:
    """Extract APC count from config name, or None if not an APC config."""
    match = APC_PATTERN.search(config)
    if match:
        return int(match.group(1))
    return None


@dataclass
class BenchmarkResult:
    """Holds the best result for a benchmark."""
    benchmark: str
    best_config: str
    best_time_ms: float
    all_results: dict[str, float]


@dataclass
class ComparisonResult:
    """Holds the comparison between two benchmark runs."""
    benchmark: str
    latest_time_ms: float
    latest_config: str
    previous_time_ms: float
    previous_config: str
    change_percent: float
    is_regression: bool
    config_changed: bool  # True if best config differs between runs
    apc_regression: bool  # True if regressed from APC to non-APC config


def fetch_url(url: str, headers: Optional[dict] = None) -> str:
    """Fetch content from a URL.

    Raises:
        URLError: If the URL cannot be reached.
        HTTPError: If the server returns an error status code.
    """
    req = Request(url)
    if headers:
        for key, value in headers.items():
            req.add_header(key, value)

    with urlopen(req, timeout=60) as response:
        return response.read().decode('utf-8')


def get_results_directories() -> list[str]:
    """Get list of result directories from GitHub, sorted by date descending.

    Raises:
        URLError: If the GitHub API cannot be reached.
        HTTPError: If the GitHub API returns an error status code.
        json.JSONDecodeError: If the API response is not valid JSON.
    """
    url = f"{GITHUB_API_BASE}/contents/results?ref=gh-pages"
    headers = {"Accept": "application/vnd.github.v3+json"}

    content = fetch_url(url, headers)
    entries = json.loads(content)

    # Filter to only date-formatted directories
    dirs = [
        entry["name"] for entry in entries
        if entry["type"] == "dir" and DATE_PATTERN.match(entry["name"])
    ]

    # Sort by date descending (lexicographic works for YYYY-MM-DD-HHMM format)
    dirs.sort(reverse=True)
    return dirs


def fetch_benchmark_results(run_dir: str, benchmark: str) -> Optional[BenchmarkResult]:
    """Fetch and parse results for a specific benchmark from a run."""
    url = f"{RAW_CONTENT_BASE}/results/{run_dir}/{benchmark}/basic_metrics.csv"

    try:
        content = fetch_url(url)
    except (URLError, HTTPError) as e:
        print(f"Warning: Could not fetch {benchmark} results from {run_dir}: {e}", file=sys.stderr)
        return None

    try:
        df = pd.read_csv(StringIO(content))
        results: dict[str, float] = {
            str(row['filename']): float(row['total_proof_time_ms'])
            for _, row in df.iterrows()
        }

        if not results:
            return None

        # Find the best (lowest) total_proof_time_ms
        best_config = min(results, key=lambda k: results[k])
        best_time = results[best_config]

        return BenchmarkResult(
            benchmark=benchmark,
            best_config=best_config,
            best_time_ms=best_time,
            all_results=results
        )
    except (KeyError, ValueError) as e:
        print(f"Warning: Malformed CSV for {benchmark} in {run_dir}: {e}", file=sys.stderr)
        return None


def compare_results(
    latest: BenchmarkResult,
    previous: BenchmarkResult,
    regression_threshold: float = 0.0
) -> ComparisonResult:
    """Compare latest results to previous results."""
    if previous.best_time_ms == 0:
        change_percent = 0.0
        is_regression = False
    else:
        change_percent = (
            (latest.best_time_ms - previous.best_time_ms) / previous.best_time_ms
        ) * 100
        is_regression = change_percent > regression_threshold

    # Check if best config changed
    config_changed = latest.best_config != previous.best_config

    # Check if regressed from APC to non-APC
    # This is an error if previous used APCs but latest doesn't
    previous_uses_apc = is_apc_config(previous.best_config)
    latest_uses_apc = is_apc_config(latest.best_config)
    apc_regression = previous_uses_apc and not latest_uses_apc

    return ComparisonResult(
        benchmark=latest.benchmark,
        latest_time_ms=latest.best_time_ms,
        latest_config=latest.best_config,
        previous_time_ms=previous.best_time_ms,
        previous_config=previous.best_config,
        change_percent=change_percent,
        is_regression=is_regression,
        config_changed=config_changed,
        apc_regression=apc_regression,
    )


def format_change_percent(change: float) -> str:
    """Format a percentage change with appropriate sign."""
    if change == 0.0:
        return "0.0%"
    elif change > 0:
        return f"+{change:.1f}%"
    else:
        return f"{change:.1f}%"


def format_report(
    latest_run: str,
    previous_run: str,
    comparisons: list[ComparisonResult],
    errors: list[str],
    warnings: list[str]
) -> str:
    """Format the comparison report as markdown."""
    lines: list[str] = []

    def add_table_section(title: str, items: list[ComparisonResult]) -> None:
        """Add a markdown table section for comparison results."""
        if not items:
            return
        lines.append(f"## {title}")
        lines.append("")
        lines.append("| Benchmark | Latest (ms) | Previous (ms) | Change |")
        lines.append("|-----------|-------------|---------------|--------|")
        for r in items:
            lines.append(
                f"| {r.benchmark} | {r.latest_time_ms:.0f} ({r.latest_config}) | "
                f"{r.previous_time_ms:.0f} ({r.previous_config}) | "
                f"{format_change_percent(r.change_percent)} |"
            )
        lines.append("")

    lines.append("# Nightly Benchmark Comparison Report")
    lines.append("")
    lines.append(f"**Latest run:** {latest_run}")
    lines.append(f"**Previous run:** {previous_run}")
    lines.append("")

    if errors:
        lines.append("## Errors")
        lines.append("")
        for error in errors:
            lines.append(f"- {error}")
        lines.append("")

    if warnings:
        lines.append("## Warnings")
        lines.append("")
        for warning in warnings:
            lines.append(f"- {warning}")
        lines.append("")

    regressions = [c for c in comparisons if c.is_regression]
    improvements = [c for c in comparisons if c.change_percent < 0]
    stable = [c for c in comparisons if not c.is_regression and c.change_percent >= 0]

    add_table_section("Regressions", regressions)
    add_table_section("Improvements", improvements)
    add_table_section("Stable", stable)

    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Analyze nightly benchmark results and report regressions."
    )
    parser.add_argument(
        "--regression-threshold",
        type=float,
        default=0.0,
        help="Percentage threshold above which a change is considered a regression (default: 0.0)"
    )
    parser.add_argument(
        "--latest",
        type=str,
        help="Specific run directory to use as latest (default: auto-detect)"
    )
    parser.add_argument(
        "--previous",
        type=str,
        help="Specific run directory to use as previous (default: auto-detect)"
    )
    parser.add_argument(
        "--benchmarks",
        type=str,
        nargs="+",
        default=BENCHMARKS,
        help=f"Benchmarks to analyze (default: {' '.join(BENCHMARKS)})"
    )
    parser.add_argument(
        "--output-format",
        choices=["markdown", "json"],
        default="markdown",
        help="Output format (default: markdown)"
    )
    args = parser.parse_args()

    # Get result directories
    print("Fetching results directories...", file=sys.stderr)
    try:
        result_dirs = get_results_directories()
    except (URLError, HTTPError) as e:
        print(f"Error: Could not fetch results directories: {e}", file=sys.stderr)
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"Error: Failed to parse GitHub API response: {e}", file=sys.stderr)
        sys.exit(1)

    if len(result_dirs) < 2:
        print("Error: Need at least 2 result directories to compare", file=sys.stderr)
        sys.exit(1)

    latest_run = args.latest or result_dirs[0]
    previous_run = args.previous or result_dirs[1]

    print(f"Comparing {latest_run} (latest) vs {previous_run} (previous)", file=sys.stderr)

    # Fetch results for each benchmark
    comparisons = []
    errors = []
    warnings = []

    for benchmark in args.benchmarks:
        print(f"Analyzing {benchmark}...", file=sys.stderr)

        latest_result = fetch_benchmark_results(latest_run, benchmark)
        previous_result = fetch_benchmark_results(previous_run, benchmark)

        if latest_result is None:
            errors.append(f"{benchmark}: Latest results not found or malformed")
            continue

        if previous_result is None:
            errors.append(f"{benchmark}: Previous results not found or malformed")
            continue

        comparison = compare_results(
            latest_result,
            previous_result,
            args.regression_threshold
        )
        comparisons.append(comparison)

        # Check for config changes and APC regressions
        if comparison.apc_regression:
            errors.append(
                f"{benchmark}: APC regression - best config changed from APC "
                f"({comparison.previous_config}) to non-APC ({comparison.latest_config})"
            )
        elif comparison.config_changed:
            warnings.append(
                f"{benchmark}: Best config changed from {comparison.previous_config} "
                f"to {comparison.latest_config}"
            )

    # Generate report
    if args.output_format == "json":
        output = {
            "latest_run": latest_run,
            "previous_run": previous_run,
            "comparisons": [
                {
                    "benchmark": c.benchmark,
                    "latest_time_ms": c.latest_time_ms,
                    "latest_config": c.latest_config,
                    "previous_time_ms": c.previous_time_ms,
                    "previous_config": c.previous_config,
                    "change_percent": c.change_percent,
                    "is_regression": c.is_regression,
                    "config_changed": c.config_changed,
                    "apc_regression": c.apc_regression,
                }
                for c in comparisons
            ],
            "errors": errors,
            "warnings": warnings,
            "has_regressions": any(c.is_regression for c in comparisons),
            "has_errors": len(errors) > 0,
            "has_warnings": len(warnings) > 0,
        }
        print(json.dumps(output, indent=2))
    else:
        report = format_report(latest_run, previous_run, comparisons, errors, warnings)
        print(report)

    # Exit with error code if there are regressions or errors
    has_regressions = any(c.is_regression for c in comparisons)
    has_errors = len(errors) > 0
    has_warnings = len(warnings) > 0

    if has_errors:
        print("\nErrors were encountered during analysis.", file=sys.stderr)
        sys.exit(2)

    if has_regressions:
        print("\nRegressions detected!", file=sys.stderr)
        sys.exit(1)

    if has_warnings:
        print("\nWarnings were generated (see report).", file=sys.stderr)

    print("\nNo regressions detected.", file=sys.stderr)
    sys.exit(0)


if __name__ == "__main__":
    main()

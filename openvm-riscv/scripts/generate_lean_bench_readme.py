from __future__ import annotations

from argparse import ArgumentParser
from pathlib import Path


def collect_gen_times(results_dir: Path) -> list[tuple[str, str, int]]:
    """Return (experiment, run, seconds) for every recorded APC generation time.

    `run_guest_benches.sh` writes `apc_generation_time_s.txt` (integer seconds)
    into each APC-enabled run dir when `POWDR_BENCH_TIME_APCS=1`.
    """
    rows: list[tuple[str, str, int]] = []
    for experiment_dir in sorted(p for p in results_dir.iterdir() if p.is_dir()):
        for time_file in sorted(experiment_dir.glob("**/apc_generation_time_s.txt")):
            run = time_file.parent.relative_to(experiment_dir).as_posix()
            try:
                seconds = int(time_file.read_text().strip())
            except ValueError:
                continue
            rows.append((experiment_dir.name, run, seconds))
    return rows


def fmt_hms(seconds: int) -> str:
    m, s = divmod(seconds, 60)
    return f"{m}m{s:02d}s" if m else f"{s}s"


def generate_readme(results_dir: Path, run_id: str, powdr_sha: str, apc_rev: str) -> str:
    rows = collect_gen_times(results_dir)
    total = sum(s for _, _, s in rows)

    lines = [
        f"# Lean-optimizer bench results — {run_id}",
        "",
        "## What this is",
        "",
        "This run is **identical to the nightly guest-benchmark job**",
        "(`openvm-riscv/scripts/run_guest_benches.sh`, the `test_apc_guest` job in",
        "`.github/workflows/nightly-tests.yml`) with the same guests, inputs, APC",
        "counts, and environment — except that APC optimization is routed through the",
        "Lean4 verified [apc-optimizer](https://github.com/powdr-labs/apc-optimizer)",
        "via FFI instead of the native Rust optimizer. Concretely, the only",
        "differences from nightly are:",
        "",
        "- the `powdr_openvm_riscv` binary is built with the `lean-optimizer` feature, and",
        "- `POWDR_USE_LEAN_OPTIMIZER=1` is set at runtime.",
        "",
        "Proving is unchanged; only the APC **generation** step differs. The times",
        "below are therefore the interesting, Lean-specific signal — in nightly the",
        "same APCs are produced by the native Rust optimizer.",
        "",
        f"- powdr: `{powdr_sha}`",
        f"- apc-optimizer: `{apc_rev}`",
        "",
        "## APC generation time per benchmark",
        "",
        "Wall-clock time of the `generate-apcs` stage (build + rank + optimize all",
        "candidates) for each APC-enabled run, measured with the Lean optimizer:",
        "",
    ]

    if rows:
        lines += [
            "| Benchmark | Run | APC generation time |",
            "| --- | --- | --- |",
        ]
        lines += [
            f"| {exp} | {run} | {fmt_hms(sec)} |" for exp, run, sec in rows
        ]
        lines += [
            f"| **total** | | **{fmt_hms(total)}** |",
            "",
        ]
    else:
        lines += [
            "_No APC generation times were recorded (was `POWDR_BENCH_TIME_APCS=1`",
            "set, and were any APC-enabled runs executed?)._",
            "",
        ]

    return "\n".join(lines)


def main() -> None:
    parser = ArgumentParser(description="Generate a README for a local Lean-optimizer bench run.")
    parser.add_argument("results_dir", type=Path)
    parser.add_argument("run_id")
    parser.add_argument("--powdr-sha", default="unknown")
    parser.add_argument("--apc-rev", default="unknown")
    parser.add_argument("--output", type=Path, default=None)
    args = parser.parse_args()

    readme = generate_readme(args.results_dir, args.run_id, args.powdr_sha, args.apc_rev)

    if args.output is None:
        print(readme, end="")
    else:
        args.output.write_text(readme)


if __name__ == "__main__":
    main()

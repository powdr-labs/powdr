#!/usr/bin/env python3

import argparse
from collections import OrderedDict
import pandas as pd
import matplotlib.pyplot as plt
from metrics_utils import load_metrics_dataframes, is_normal_instruction_air

def extract_metrics(filename):
    app, leaf, internal = load_metrics_dataframes(filename)
    metrics = OrderedDict()

    powdr_air = app[app["air_name"].fillna('').str.startswith("PowdrAir")]
    non_powdr_air = app[~app["air_name"].fillna('').str.startswith("PowdrAir")]
    
    # Split non_powdr_air into normal instructions and openvm precompiles
    is_normal_instruction = non_powdr_air["air_name"].fillna('').apply(is_normal_instruction_air)
    normal_instruction_air = non_powdr_air[is_normal_instruction]
    openvm_precompile_air = non_powdr_air[~is_normal_instruction]

    def get_metric(df, metric_name):
        return pd.to_numeric(df[df["metric"] == metric_name]["value"]).sum()

    # Compute total proof times
    app_proof_time_ms = get_metric(app, "total_proof_time_ms")
    leaf_proof_time_ms = get_metric(leaf, "total_proof_time_ms")
    internal_proof_time_ms = get_metric(internal, "total_proof_time_ms")
    total_proof_time_ms = app_proof_time_ms + leaf_proof_time_ms + internal_proof_time_ms

    app_proof_time_excluding_trace_ms = get_metric(app, "stark_prove_excluding_trace_time_ms")
    leaf_proof_time_excluding_trace_ms = get_metric(leaf, "stark_prove_excluding_trace_time_ms")
    internal_proof_time_excluding_trace_ms = get_metric(internal, "stark_prove_excluding_trace_time_ms")
    total_proof_time_excluding_trace_ms = app_proof_time_excluding_trace_ms + leaf_proof_time_excluding_trace_ms + internal_proof_time_excluding_trace_ms

    # Compute total column counts
    # Note that this sums the columns over *all* segments.
    # This metric should roughly correlate with leaf proof time.
    main_cols = get_metric(app, "main_cols")
    prep_cols = get_metric(app, "prep_cols")
    perm_cols = get_metric(app, "perm_cols")
    app_proof_cols = main_cols + prep_cols + perm_cols

    num_segments = int(pd.to_numeric(app["segment"]).max()) + 1

    metrics["filename"] = filename
    metrics["num_segments"] = num_segments
    metrics["app_proof_cells"] = get_metric(app, "total_cells")
    metrics["app_proof_cols"] = app_proof_cols
    metrics["total_proof_time_ms"] = total_proof_time_ms
    metrics["total_proof_time_excluding_trace_ms"] = total_proof_time_excluding_trace_ms
    metrics["app_proof_time_ms"] = app_proof_time_ms
    metrics["app_proof_time_excluding_trace_ms"] = app_proof_time_excluding_trace_ms
    metrics["app_execute_preflight_time_ms"] = get_metric(app, "execute_preflight_time_ms")
    metrics["app_execute_metered_time_ms"] = get_metric(app, "execute_metered_time_ms")
    metrics["app_trace_gen_time_ms"] = get_metric(app, "trace_gen_time_ms")
    metrics["leaf_proof_time_ms"] = leaf_proof_time_ms
    metrics["leaf_proof_time_excluding_trace_ms"] = leaf_proof_time_excluding_trace_ms
    metrics["inner_recursion_proof_time_ms"] = internal_proof_time_ms
    metrics["inner_recursion_proof_time_excluding_trace_ms"] = internal_proof_time_excluding_trace_ms

    normal_instruction_cells = get_metric(normal_instruction_air, "cells")
    openvm_precompile_cells = get_metric(openvm_precompile_air, "cells")
    powdr_cells = get_metric(powdr_air, "cells")
    assert(metrics["app_proof_cells"] == powdr_cells + normal_instruction_cells + openvm_precompile_cells)

    metrics["normal_instruction_ratio"] = normal_instruction_cells / metrics["app_proof_cells"]
    metrics["openvm_precompile_ratio"] = openvm_precompile_cells / metrics["app_proof_cells"]
    metrics["powdr_ratio"] = powdr_cells / metrics["app_proof_cells"]
    metrics["powdr_rows"] = get_metric(powdr_air, "rows")

    return metrics

def summary_table(metrics_files, csv):
    file_metrics = [ extract_metrics(filename) for filename in metrics_files ]

    df = pd.DataFrame(file_metrics)
    if csv:
        print(df.to_csv(index=False))
    else:
        print(df.to_string(index=False))

def plot(metrics_files, output):
    file_metrics = [ extract_metrics(filename) for filename in metrics_files ]
    df = pd.DataFrame(file_metrics)

    # Compute app "other" time
    df["app_other_ms"] = (
        df["app_proof_time_ms"]
        - df["app_proof_time_excluding_trace_ms"]
        - df["app_execute_preflight_time_ms"]
        - df["app_execute_metered_time_ms"]
        - df["app_trace_gen_time_ms"]
    )

    # Stack components (bottom to top) with colors
    # App components use shades of blue, others use distinct colors
    components = [
        ("inner_recursion_proof_time_ms", "Inner recursion proof", "#ff6e0e"),        # orange
        ("leaf_proof_time_ms", "Leaf proof", "#ffb70e"),                              # yellow
        ("app_proof_time_excluding_trace_ms", "App prove (excl. trace)", "#1f77b4"),  # blue
        ("app_trace_gen_time_ms", "App trace gen", "#6baed6"),                        # light blue
        ("app_execute_preflight_time_ms", "App execute preflight", "#9ecae1"),        # lighter blue
        ("app_execute_metered_time_ms", "App execute metered", "#c6dbef"),            # very light blue
        ("app_other_ms", "App other", "#08519c"),                                     # dark blue
    ]

    # Extract labels from filenames (use parent directory name)
    import os
    labels = [os.path.basename(os.path.dirname(f)) for f in df["filename"]]

    fig, ax = plt.subplots(figsize=(10, 6))

    bottom = [0.0] * len(df)
    for col, label, color in components:
        values = [v / 1000 for v in df[col].tolist()]  # Convert ms to seconds
        ax.bar(labels, values, bottom=bottom, label=label, color=color)
        bottom = [b + v for b, v in zip(bottom, values)]

    ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: f'{x:.2f}'))
    ax.set_ylabel("Proof time (s)")
    ax.set_xlabel("Configuration")
    ax.set_title("Proof Time Breakdown")
    # Reverse legend so top of legend matches top of stack
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(handles[::-1], labels[::-1], loc="upper right")

    plt.tight_layout()
    if output:
        plt.savefig(output)
        print(f"Plot saved to {output}")
    else:
        plt.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Print basic metrics from a set of metrics JSON files.")
    subparsers = parser.add_subparsers(dest="command", required=True)

    summary_parser = subparsers.add_parser("summary-table", help="Print a summary table of metrics")
    summary_parser.add_argument('metrics_files', nargs='+', help='Paths to the metrics JSON files')
    summary_parser.add_argument('--csv', action='store_true', help='Output in CSV format')

    plot_parser = subparsers.add_parser("plot", help="Plot a stacked bar chart of proof time breakdown")
    plot_parser.add_argument('metrics_files', nargs='+', help='Paths to the metrics JSON files')
    plot_parser.add_argument('--output', '-o', help='Output file path (if not specified, displays interactively)')

    args = parser.parse_args()
    if args.command == "summary-table":
        summary_table(args.metrics_files, args.csv)
    elif args.command == "plot":
        plot(args.metrics_files, args.output)

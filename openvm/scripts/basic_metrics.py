#!/usr/bin/env python3

import argparse
from collections import OrderedDict
import pandas as pd
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
    metrics["leaf_proof_time_excluding_trace_ms"] = leaf_proof_time_excluding_trace_ms
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

def main(metrics_files, csv):
    file_metrics = [ extract_metrics(filename) for filename in metrics_files ]

    df = pd.DataFrame(file_metrics)
    if csv:
        print(df.to_csv(index=False))
    else:
        print(df.to_string(index=False))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Print basic metrics from a set of metrics JSON files.")
    parser.add_argument('metrics_files', nargs=argparse.REMAINDER, help='Paths to the metrics JSON files')
    parser.add_argument('--csv', action='store_true', help='Output in CSV format')
    args = parser.parse_args()
    main(args.metrics_files, args.csv)

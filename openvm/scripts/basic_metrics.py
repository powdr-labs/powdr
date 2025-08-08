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

    metrics["filename"] = filename

    metrics["num_segments"] = pd.to_numeric(app[app["metric"] == "num_segments"]["value"]).sum()

    metrics["app_proof_cells"] = pd.to_numeric(app[app["metric"] == "total_cells"]["value"]).sum()

    metrics["app_proof_time_ms"] = pd.to_numeric(app[app["metric"] == "stark_prove_excluding_trace_time_ms"]["value"]).sum()

    metrics["app_execute_time_ms"] = pd.to_numeric(app[app["metric"] == "execute_time_ms"]["value"]).sum()

    metrics["app_trace_gen_time_ms"] = pd.to_numeric(app[app["metric"] == "trace_gen_time_ms"]["value"]).sum()

    metrics["leaf_proof_time_ms"] = pd.to_numeric(leaf[leaf["metric"] == "stark_prove_excluding_trace_time_ms"]["value"]).sum()
    metrics["inner_recursion_proof_time_ms"] = pd.to_numeric(internal[internal["metric"] == "stark_prove_excluding_trace_time_ms"]["value"]).sum()

    normal_instruction_cells = pd.to_numeric(normal_instruction_air[normal_instruction_air["metric"] == "cells"]["value"]).sum()
    openvm_precompile_cells = pd.to_numeric(openvm_precompile_air[openvm_precompile_air["metric"] == "cells"]["value"]).sum()
    powdr_cells = pd.to_numeric(powdr_air[powdr_air["metric"] == "cells"]["value"]).sum()
    assert(metrics["app_proof_cells"] == powdr_cells + normal_instruction_cells + openvm_precompile_cells)

    metrics["normal_instruction_ratio"] = normal_instruction_cells / metrics["app_proof_cells"]
    metrics["openvm_precompile_ratio"] = openvm_precompile_cells / metrics["app_proof_cells"]
    metrics["powdr_ratio"] = powdr_cells / metrics["app_proof_cells"]

    metrics["powdr_rows"] = pd.to_numeric(powdr_air[powdr_air["metric"] == "rows"]["value"]).sum()

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

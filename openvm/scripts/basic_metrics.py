#!/usr/bin/env python3

import argparse
from collections import OrderedDict
import pandas as pd
from metrics_utils import load_metrics_dataframes

def extract_metrics(filename):
    app, leaf, internal = load_metrics_dataframes(filename)
    metrics = OrderedDict()

    powdr_air = app[app["air_name"].fillna('').str.startswith("PowdrAir")]
    non_powdr_air = app[~app["air_name"].fillna('').str.startswith("PowdrAir")]

    metrics["filename"] = filename

    metrics["num_segments"] = pd.to_numeric(app[app["metric"] == "num_segments"]["value"]).sum()

    metrics["app_proof_cells"] = pd.to_numeric(app[app["metric"] == "total_cells"]["value"]).sum()

    metrics["app_proof_time_ms"] = pd.to_numeric(app[app["metric"] == "stark_prove_excluding_trace_time_ms"]["value"]).sum()

    metrics["app_execute_time_ms"] = pd.to_numeric(app[app["metric"] == "execute_time_ms"]["value"]).sum()

    metrics["app_trace_gen_time_ms"] = pd.to_numeric(app[app["metric"] == "trace_gen_time_ms"]["value"]).sum()

    metrics["leaf_proof_time_ms"] = pd.to_numeric(leaf[leaf["metric"] == "stark_prove_excluding_trace_time_ms"]["value"]).sum()
    metrics["inner_recursion_proof_time_ms"] = pd.to_numeric(internal[internal["metric"] == "stark_prove_excluding_trace_time_ms"]["value"]).sum()

    metrics["non_powdr_cells"] = pd.to_numeric(non_powdr_air[non_powdr_air["metric"] == "cells"]["value"]).sum()
    metrics["powdr_cells"] = pd.to_numeric(powdr_air[powdr_air["metric"] == "cells"]["value"]).sum()
    # Not sure why the following equality doesn't always hold:
    # assert(metrics["powdr_cells"] == metrics["app_proof_cells"] - metrics["non_powdr_cells"])
    metrics["non_powdr_ratio"] = metrics["non_powdr_cells"] / metrics["app_proof_cells"]

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

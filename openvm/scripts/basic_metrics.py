#!/usr/bin/env python3

import json
import argparse
from collections import OrderedDict
import pandas as pd

def extract_metrics(filename):
    with open(filename) as f:
        metrics_json = json.load(f)
    metrics = OrderedDict()

    entries = [
        (dict(c["labels"]), { "metric": c["metric"], "value": c["value"] })
        for c in metrics_json["counter"] + metrics_json["gauge"]
    ]

    metrics["filename"] = filename

    metrics["num_segments"] = [metric["value"]
        for _, metric in entries
        if metric["metric"] == "num_segments"][0]

    metrics["app_proof_cells"] = sum([int(metric["value"])
        for labels, metric in entries
        if labels.get("group") == "app_proof"
        and metric["metric"] == "total_cells"])

    # TODO: I think the metrics for PowdrAir airs are wrong because they
    # override each other due to having the same name

    # metrics["powdr_cells"] = sum([int(metric["value"])
    #     for labels, metric in entries
    #     if labels.get("group") == "app_proof"
    #     and labels.get("air_name")
    #     and labels.get("air_name").startswith("PowdrAir")
    #     and metric["metric"] == "cells"])

    metrics["app_proof_time_ms"] = sum([int(metric["value"])
        for labels, metric in entries
        if labels.get("group") == "app_proof"
        and metric["metric"] == "stark_prove_excluding_trace_time_ms"])

    metrics["leaf_recursion_time_ms"] = sum([int(metric["value"])
        for labels, metric in entries
        if labels.get("group", "").startswith("leaf")
        and metric["metric"] == "stark_prove_excluding_trace_time_ms"])

    metrics["inner_recursion_time_ms"] = sum([int(metric["value"])
        for labels, metric in entries
        if labels.get("group", "").startswith("internal")
        and metric["metric"] == "stark_prove_excluding_trace_time_ms"])

    metrics["app_trace_gen_time_ms"] = sum([int(metric["value"])
        for labels, metric in entries
        if labels.get("group") == "app_proof"
        and metric["metric"] == "trace_gen_time_ms"])

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

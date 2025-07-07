#!/usr/bin/env python3

import sys
import json
import pandas as pd

def load_metrics_dataframes(filename):
    """Load metrics JSON file and return app, leaf, and internal dataframes.
    
    Each dataframe has a "metric" and "value" column, along with optional columns
    like "air_name", or "segment".
    """
    with open(filename) as f:
        metrics_json = json.load(f)

    entries = [
        dict(c["labels"]) | { "metric": c["metric"], "value": c["value"] }
        for c in metrics_json["counter"] + metrics_json["gauge"]
    ]

    df = pd.DataFrame(entries)

    # "group" has different values if coming from reth benchmark or the powdr cli
    app = df[df["group"].fillna('').str.startswith("app_proof")]
    if len(app) == 0:
        app = df[df["group"].fillna('').str.startswith("reth")]
    if len(app) == 0:
        print("Invalid metrics.json", file=sys.stderr)
        exit(1)

    leaf = df[df["group"].fillna('').str.startswith("leaf")]
    internal = df[df["group"].fillna('').str.startswith("internal")]
    
    return app, leaf, internal
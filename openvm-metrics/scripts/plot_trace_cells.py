#!/usr/bin/env python3

import json
import pandas as pd
import matplotlib.pyplot as plt
import argparse

def autopct_with_billions(pct, total):
    val = pct * total / 100
    return f'{pct:.1f}%\n{val/1e9:.2f}B'

def main(metrics_path, output_path=None, subtitle=None):
    with open(metrics_path) as f:
        metrics = json.load(f)

    # metrics["counter"] has entries of the form:
    # {
    #     "metric": "cells", // This is the only one we're interested in
    #     "value": "<value>", // The number of cells
    #     "labels": [
    #         ["segment", "<segment>"],
    #         ["air_name", "<air_name>"],
    #         ...
    #     ],
    # }

    cell_entries = [
        (dict(c["labels"]), c["value"])
        for c in metrics["counter"]
        if c["metric"] == "cells"
    ]
    rows = [
        (int(labels["segment"]), labels["air_name"], int(cells))
        for (labels, cells) in cell_entries
    ]

    df = pd.DataFrame(rows, columns=["segment", "air_name", "cells"])

    # Group and threshold
    cells_by_air = df.groupby('air_name')['cells'].sum().sort_values(ascending=False)
    threshold_ratio = 0.02
    threshold = threshold_ratio * cells_by_air.sum()
    large = cells_by_air[cells_by_air >= threshold]
    small = cells_by_air[cells_by_air < threshold]

    if not small.empty:
        large['Other'] = small.sum()

    _, ax = plt.subplots(figsize=(7.5, 7.5))
    plot_title = "Trace cells by AIR" if subtitle is None else f"Trace cells by AIR ({subtitle})"
    ax.set_title(plot_title)
    total = large.sum()
    ax.pie(
        large,
        autopct=lambda pct: autopct_with_billions(pct, total),
        labeldistance=1.05,
        startangle=90
    )
    ax.legend(
        large.index,
        title="AIRs",
        loc="upper center",
        bbox_to_anchor=(0.5, 0),
        ncol=1,
        fontsize='small',
        title_fontsize='medium',
        frameon=False
    )
    plt.ylabel('')
    plt.tight_layout(pad=5.0)
    if output_path:
        print(f"Saving plot to {output_path}")
        plt.savefig(output_path, bbox_inches="tight")
    else:
        plt.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Visualize AIR cell metrics from a JSON file.")
    parser.add_argument("metrics_path", help="Path to the metrics.json file")
    parser.add_argument("-o", "--output", help="Optional path to save the output image")
    parser.add_argument("-s", "--subtitle", help="Optional subtitle for the plot")
    args = parser.parse_args()

    main(args.metrics_path, args.output, args.subtitle)
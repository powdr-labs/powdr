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

    total_cells = sum(
        int(c["value"]) for c in metrics["counter"] if c["metric"] == "total_cells"
    )
    print(f"Total cells: {total_cells/1e9:.2f}B")

    cell_entries = [
        (dict(c["labels"]), c["value"])
        for c in metrics["counter"]
        if c["metric"] == "cells"
    ]
    rows = [
        (int(labels.get("segment", 0)), labels["air_name"], int(cells))
        for (labels, cells) in cell_entries
    ]

    df = pd.DataFrame(rows, columns=["segment", "air_name", "cells"])

    # Group and threshold
    cells_by_air = df.groupby('air_name')['cells'].sum().sort_values(ascending=False)
    print("Cells by AIR:")
    print(cells_by_air)

    # Sanity check: #cells should match total_cells
    # Doesn't currently match exactly (not sure why), but should be close
    total_cells2 = cells_by_air.sum()
    diff = abs(total_cells - total_cells2)
    relative_diff = diff / total_cells
    if relative_diff > 0.01:
        print(f"\nWarning: Total cells mismatch: {total_cells} vs {total_cells2} (diff: {relative_diff* 100:.2f}%)")

    threshold_ratio = 0.015
    threshold = threshold_ratio * cells_by_air.sum()
    large = cells_by_air[cells_by_air >= threshold]
    small = cells_by_air[cells_by_air < threshold]

    if not small.empty:
        large['Other'] = small.sum()

    _, ax = plt.subplots(figsize=(7.5, 7.5))
    plot_title = "Trace cells by AIR" if subtitle is None else f"Trace cells by AIR ({subtitle})"
    ax.set_title(plot_title)
    total = large.sum()
    colors = plt.get_cmap("tab20")(range(len(large)))
    def autopct_filtered(pct):
        return autopct_with_billions(pct, total) if pct > 5 else ''

    wedges, texts, autotexts = ax.pie(
        large,
        autopct=autopct_filtered,
        startangle=90,
        colors=colors
    )
    percentages = 100 * large / total
    legend_labels = [f"{percent:.1f}% - {label}" for label, percent in zip(large.index, percentages)]
    ax.legend(
        wedges,
        legend_labels,
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
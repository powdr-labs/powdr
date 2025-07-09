#!/usr/bin/env python3

import pandas as pd
import matplotlib.pyplot as plt
import argparse
from metrics_utils import load_metrics_dataframes

def autopct_with_billions(pct, total):
    val = pct * total / 100
    return f'{pct:.1f}%\n{val/1e9:.2f}B'

def main(metrics_path, output_path=None, subtitle=None):
    # Load only the app dataframe
    app, _, _ = load_metrics_dataframes(metrics_path)
    
    # Get total cells from app dataframe
    total_cells_df = app[app["metric"] == "total_cells"]
    total_cells = pd.to_numeric(total_cells_df["value"]).sum()
    print(f"Total cells: {total_cells/1e9:.2f}B")
    
    # Get cell entries from app dataframe
    cells_df = app[app["metric"] == "cells"].copy()
    cells_df["segment"] = pd.to_numeric(cells_df["segment"].fillna(0))
    cells_df["cells"] = pd.to_numeric(cells_df["value"])
    
    # Create dataframe with required columns
    df = cells_df[["segment", "air_name", "cells"]]

    # Group and threshold
    cells_by_air = df.groupby('air_name')['cells'].sum().sort_values(ascending=False)
    print("Cells by AIR:")
    print(cells_by_air)

    # Sanity check: #cells should match total_cells
    assert total_cells == cells_by_air.sum()

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

    wedges, _, _ = ax.pie(
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
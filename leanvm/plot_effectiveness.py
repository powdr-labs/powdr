#!/usr/bin/env python3
"""Plot a histogram of optimization effectiveness (columns_before / columns_after),
weighted by num_instructions."""

import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

if len(sys.argv) < 2:
    print(f"Usage: {sys.argv[0]} <csv_path>")
    sys.exit(1)

csv_path = sys.argv[1]
df = pd.read_csv(csv_path)

# Filter out rows where columns_before is 0
n_before = df["num_instructions"].sum()
df = df[df["columns_before"] > 0]
n_after = df["num_instructions"].sum()
pct_filtered = (n_before - n_after) / n_before * 100
print(f"Filtered out {n_before - n_after} / {n_before} instructions ({pct_filtered:.1f}%) with columns_before == 0")

df["effectiveness"] = df["columns_before"] / df["columns_after"]

# Compute weighted 98th percentile
sorted_idx = np.argsort(df["effectiveness"].values)
sorted_eff = df["effectiveness"].values[sorted_idx]
sorted_w = df["num_instructions"].values[sorted_idx]
cum_w = np.cumsum(sorted_w)
cum_w_norm = cum_w / cum_w[-1]
hi = sorted_eff[np.searchsorted(cum_w_norm, 0.98)]

mask = df["effectiveness"] <= hi
df_filtered = df[mask]

plt.figure(figsize=(10, 6))
plt.hist(df_filtered["effectiveness"], bins=50, weights=df_filtered["num_instructions"], edgecolor="black", linewidth=0.3)
plt.xlabel("Effectiveness (columns before / columns after)")
plt.ylabel("Instruction count")
name = csv_path.rsplit("/", 1)[-1].rsplit(".", 1)[0]
out_path = f"{name}_effectiveness.png"
plt.title(f"Effectiveness of {name}")
plt.tight_layout()
plt.savefig(out_path, dpi=150)
plt.show()
print(f"Saved to {out_path}")

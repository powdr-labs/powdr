#!/usr/bin/env python3

import json
import pandas as pd
import matplotlib.pyplot as plt
import argparse
import numpy as np

def load_apc_data(json_path):
    """Load APC candidates and compute effectiveness."""
    with open(json_path, 'r') as f:
        data = json.load(f)
    
    return pd.DataFrame([{
        'effectiveness': item['total_width_before'] / item['total_width_after'],
        'instructions': len(item['original_instructions']),
        'frequency': item['execution_frequency'],
        'total_width_before': item['total_width_before'],
        'software_version_cells': item['total_width_before'] * item['execution_frequency']
    } for item in data])

def weighted_quantile(values, weights, quantile):
    """Calculate weighted quantile."""
    indices = np.argsort(values)
    sorted_values = values[indices]
    sorted_weights = weights[indices]
    cumsum_weights = np.cumsum(sorted_weights)
    total_weight = cumsum_weights[-1]
    
    # Find the value at the given quantile
    target_weight = quantile * total_weight
    idx = np.searchsorted(cumsum_weights, target_weight)
    
    if idx >= len(sorted_values):
        return sorted_values[-1]
    return sorted_values[idx]

def remove_outliers(df, column, weight_column, factor=1.1):
    """Remove outliers using weighted IQR method."""
    values = df[column].values
    weights = df[weight_column].values
    
    percentile5 = weighted_quantile(values, weights, 0.05)
    percentile95 = weighted_quantile(values, weights, 0.95)
    IQR = percentile95 - percentile5
    lower_bound = percentile5 - factor * IQR
    upper_bound = percentile95 + factor * IQR
    return df[(df[column] >= lower_bound) & (df[column] <= upper_bound)]

def format_cell_count(count):
    """Format cell count with appropriate units."""
    if count >= 1e9:
        return f"{count/1e9:.1f}B"
    elif count >= 1e6:
        return f"{count/1e6:.1f}M"
    elif count >= 1e3:
        return f"{count/1e3:.1f}K"
    else:
        return f"{count:.0f}"

def group_instruction_count(count):
    """Group instruction counts into ranges."""
    if count <= 9:
        return str(count)
    elif count <= 14:
        return '10-14'
    elif count <= 19:
        return '15-19'
    elif count <= 29:
        return '20-29'
    elif count <= 49:
        return '30-49'
    elif count <= 100:
        return '50-100'
    else:
        return '>100'

def prepare_histogram_data(df, bins):
    """Prepare weighted histogram data grouped by instruction count."""
    df['inst_group'] = df['instructions'].apply(group_instruction_count)
    
    group_order = ['1', '2', '3', '4', '5', '6', '7', '8', '9', 
                   '10-14', '15-19', '20-29', '30-49', '50-100', '>100']
    existing_groups = [g for g in group_order if g in df['inst_group'].values]
    
    hist_data = []
    for group in existing_groups:
        group_df = df[df['inst_group'] == group]
        software_version_cells = group_df['software_version_cells'].values
        values = group_df['effectiveness'].values
        
        hist = np.histogram(values, bins=bins, weights=software_version_cells)[0] if len(values) > 0 else np.zeros(len(bins) - 1)
        hist_data.append(hist)
    
    return existing_groups, hist_data

def plot_effectiveness(json_path, filename=None):
    """Generate stacked histogram of effectiveness data."""
    df = load_apc_data(json_path)
    
    # Calculate mean from full dataset
    total_software_version_cells = df['software_version_cells'].sum()
    mean_effectiveness = (df['effectiveness'] * df['software_version_cells']).sum() / total_software_version_cells
    
    # Remove outliers for visualization
    df_clean = remove_outliers(df, 'effectiveness', 'software_version_cells')
    
    # Calculate trace cells statistics
    total_cells_full = df['software_version_cells'].sum()
    total_cells_clean = df_clean['software_version_cells'].sum()
    percentage = (total_cells_clean / total_cells_full) * 100
    
    # Prepare histogram
    bins = np.linspace(df_clean['effectiveness'].min(), df_clean['effectiveness'].max(), 20)
    existing_groups, hist_data = prepare_histogram_data(df_clean, bins)
    
    # Create plot with extra space for legend
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Stacked bar chart
    bottom = np.zeros(len(bins) - 1)
    colors = plt.cm.viridis(np.linspace(0, 1, len(existing_groups)))
    
    for i, (group, data) in enumerate(zip(existing_groups, hist_data)):
        label = f'{group} instruction{"s" if group != "1" else ""}'
        ax.bar(bins[:-1], data, width=np.diff(bins), bottom=bottom, 
               label=label, color=colors[i], 
               edgecolor='black', linewidth=0.5, alpha=0.8)
        bottom += data
    
    # Formatting
    ax.set_xlabel('Effectiveness', fontsize=12)
    ax.set_ylabel('Instruction trace cells (software version)', fontsize=12)
    
    cells_str = format_cell_count(total_cells_clean)
    ax.set_title(f'Distribution of Effectiveness\n(Accounting for {cells_str} instruction trace cells ({percentage:.0f}%))', 
                 fontsize=14)
    ax.grid(True, alpha=0.3, axis='y')
    # Place legend outside the plot area
    ax.legend(title='Basic Block size', loc='center left', bbox_to_anchor=(1, 0.5), 
              frameon=True, fancybox=True, shadow=True)
    
    # Add mean line
    ax.axvline(mean_effectiveness, color='red', linestyle='--', linewidth=2, alpha=0.7)
    
    # Add statistics text
    stats_text = f'Mean: {mean_effectiveness:.2f}'
    props = dict(boxstyle='round,pad=0.5', facecolor='wheat', alpha=0.8)
    ax.text(0.02, 0.97, stats_text, transform=ax.transAxes, fontsize=10,
            verticalalignment='top', bbox=props)
    
    plt.tight_layout()
    plt.subplots_adjust(right=0.85)  # Make room for the legend
    
    # Save or show
    if filename:
        plt.savefig(filename, dpi=300, bbox_inches='tight')
    else:
        plt.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Plot effectiveness analysis from APC candidates JSON file.")
    parser.add_argument("json_path", help="Path to the APC candidates JSON file")
    parser.add_argument("-o", "--output", help="Optional file name to save the plot", default=None)
    args = parser.parse_args()
    
    plot_effectiveness(args.json_path, args.output)
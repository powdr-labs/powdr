#!/usr/bin/env python3

import json
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import argparse
import re

def load_apc_data(json_path, effectiveness_type='cost'):
    """Load APC candidates and compute effectiveness."""
    with open(json_path, 'r') as f:
        data = json.load(f)
    
    def calculate_effectiveness(item, eff_type):
        if eff_type == 'cost':
            return item['cost_before'] / item['cost_after']
        elif eff_type == 'main_columns':
            return item['stats']['before']['main_columns'] / item['stats']['after']['main_columns']
        elif eff_type == 'constraints':
            return item['stats']['before']['constraints'] / item['stats']['after']['constraints']
        elif eff_type == 'bus_interactions':
            return item['stats']['before']['bus_interactions'] / item['stats']['after']['bus_interactions']
        else:
            raise ValueError(f"Unknown effectiveness type: {eff_type}")
    
    return pd.DataFrame([{
        'start_pc': item['original_block']['start_pc'],
        'effectiveness': calculate_effectiveness(item, effectiveness_type),
        'instructions': len(item['original_block']['statements']),
        'software_version_cells': item['width_before'] * item['execution_frequency'],
        'width_before': item['width_before']
    } for item in data])

def load_tmp_register_data(log_path):
    """Parse temporary register log file and extract removable register counts per block."""
    removable_regs = {}
    pattern = r'Block at (?:0x)?([0-9a-fA-F]+).*removable: (\d+)'
    with open(log_path, 'r') as f:
        for line in f:
            match = re.search(pattern, line)
            if match:
                pc = int(match.group(1), 16)
                count = int(match.group(2))
                removable_regs[pc] = count
    return removable_regs

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

def plot_effectiveness(json_path, filename=None, effectiveness_type='cost', tmp_reg_log=None):
    """Generate bar plot of effectiveness data."""
    df = load_apc_data(json_path, effectiveness_type)
    total_cells = df['software_version_cells'].sum()

    # Load temporary register data if provided
    if tmp_reg_log:
        removable_regs = load_tmp_register_data(tmp_reg_log)
        df['removable_regs'] = df['start_pc'].map(removable_regs).fillna(0).astype(int)
        df['width_after'] = df['width_before'] / df['effectiveness']
        df['width_after_improved'] = (df['width_after'] - df['removable_regs'] * 6).clip(lower=1)
        df['effectiveness_improved'] = df['width_before'] / df['width_after_improved']
        df['effectiveness_increase'] = df['effectiveness_improved'] - df['effectiveness']

    # Print top 10 basic blocks
    cols = ['start_pc', 'software_version_cells', 'effectiveness']
    if tmp_reg_log:
        cols += ['effectiveness_improved', 'removable_regs']
    cols += ['instructions', 'width_before']
    top10 = df.nlargest(10, 'software_version_cells')[cols].copy()
    top10['software_version_cells'] = top10['software_version_cells'].apply(format_cell_count)

    col_names = ['Start PC', 'Trace Cells', 'Eff']
    if tmp_reg_log:
        col_names += ['Eff (improved)', 'Removable']
    col_names += ['Instructions', 'Width Before']
    top10.columns = col_names

    print(f"\nTop 10 Basic Blocks by Trace Cells (Effectiveness: {effectiveness_type}):")
    print(top10.to_string(index=False))
    print()

    # Calculate weighted mean effectiveness
    mean_effectiveness = (df['effectiveness'] * df['software_version_cells']).sum() / total_cells
    if tmp_reg_log:
        mean_improved = (df['effectiveness_improved'] * df['software_version_cells']).sum() / total_cells
        print(f"Mean effectiveness: {mean_effectiveness:.2f} -> {mean_improved:.2f} (improvement: {mean_improved - mean_effectiveness:.2f})\n")
    else:
        mean_improved = None
    
    # Separate large and small APCs (< 0.1% threshold)
    threshold = total_cells * 0.001
    df_large = df[df['software_version_cells'] >= threshold].copy()
    df_small = df[df['software_version_cells'] < threshold]
    
    # Sort large APCs by cost
    df_large = df_large.sort_values('software_version_cells', ascending=False)
    
    # Create 'Other' entry if there are small APCs
    if len(df_small) > 0:
        other_cells = df_small['software_version_cells'].sum()
        other_effectiveness = (df_small['effectiveness'] * df_small['software_version_cells']).sum() / other_cells
        other_row = pd.DataFrame([{
            'effectiveness': other_effectiveness,
            'software_version_cells': other_cells,
            'instructions': -1,  # Special marker for Other
            'is_other': True
        }])
        df_plot = pd.concat([df_large.assign(is_other=False), other_row], ignore_index=True)
    else:
        df_plot = df_large.assign(is_other=False)
    
    # Create plot
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Set up color mapping with log scale
    valid_instructions = df_plot[~df_plot['is_other']]['instructions']
    if len(valid_instructions) > 0:
        norm = mcolors.LogNorm(vmin=valid_instructions.min(), vmax=valid_instructions.max())
        cmap = plt.cm.RdYlGn  # Red-Yellow-Green colormap
    
    # Plot bars
    x_pos = 0
    for idx, row in df_plot.iterrows():
        width = row['software_version_cells']

        if row.get('is_other', False):
            color = 'lightgray'
        else:
            color = cmap(norm(row['instructions']))

        ax.bar(x_pos + width/2, row['effectiveness'], width=width,
               color=color, edgecolor='black', linewidth=0.5, alpha=0.8)

        # If we have improved effectiveness, plot the increase on top
        if tmp_reg_log and 'effectiveness_increase' in row and row['effectiveness_increase'] > 0:
            ax.bar(x_pos + width/2, row['effectiveness_increase'],
                   width=width, bottom=row['effectiveness'],
                   color='blue', edgecolor='black', linewidth=0.5, alpha=0.6)

        # Label 'Other' box if it's wide enough
        if row.get('is_other', False) and width > total_cells * 0.02:  # Only label if > 2% of total width
            ax.text(x_pos + width/2, row['effectiveness']/2,
                   f'Other\n({len(df_small)} APCs)',
                   ha='center', va='center', fontsize=10,
                   color='black', weight='bold')

        x_pos += width
    
    # Formatting
    ax.set_xlabel('Cumulative instruction trace cells (software version)', fontsize=12)
    ax.set_ylabel('Effectiveness', fontsize=12)
    title = f"Effectiveness by Basic Block (reduction in {effectiveness_type})"
    if tmp_reg_log:
        title += " - Blue shows improvement from register removal"
    ax.set_title(title, fontsize=14)
    ax.grid(True, alpha=0.3, axis='y')
    ax.axhline(mean_effectiveness, color='red', linestyle='--', linewidth=2, alpha=0.7, label='Mean')
    if tmp_reg_log and mean_improved:
        ax.axhline(mean_improved, color='darkblue', linestyle='--', linewidth=2, alpha=0.7, label='Mean (improved)')
        ax.legend(loc='upper right')

    # Format x-axis
    ax.set_xlim(0, total_cells)
    x_ticks = ax.get_xticks()
    ax.set_xticks(x_ticks)
    ax.set_xticklabels([format_cell_count(x) for x in x_ticks])

    # Add colorbar for instruction count
    if len(valid_instructions) > 0:
        sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
        sm.set_array([])
        cbar = plt.colorbar(sm, ax=ax, pad=0.02)
        cbar.set_label('Instructions (log scale)', rotation=270, labelpad=20)

    # Add mean text
    if tmp_reg_log and mean_improved:
        mean_text = f'Mean: {mean_effectiveness:.2f}\nImproved: {mean_improved:.2f}'
    else:
        mean_text = f'Mean: {mean_effectiveness:.2f}'
    ax.text(0.02, 0.97, mean_text,
            transform=ax.transAxes, fontsize=10, verticalalignment='top',
            bbox=dict(boxstyle='round,pad=0.5', facecolor='wheat', alpha=0.8))
    
    plt.tight_layout()
    
    # Save or show
    if filename:
        plt.savefig(filename, dpi=300, bbox_inches='tight')
    else:
        plt.show()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Plot effectiveness analysis from APC candidates JSON file.")
    parser.add_argument("json_path", help="Path to the APC candidates JSON file")
    parser.add_argument("-o", "--output", help="Optional file name to save the plot", default=None)
    parser.add_argument("-e", "--effectiveness",
                       choices=['cost', 'main_columns', 'constraints', 'bus_interactions'],
                       default='cost',
                       help="Type of effectiveness calculation (default: cost_before/cost_after)")
    parser.add_argument("--tmp-reg-log", help="Path to temporary register log file", default=None)
    args = parser.parse_args()

    plot_effectiveness(args.json_path, args.output, args.effectiveness, args.tmp_reg_log)

#!/usr/bin/env python3

import json
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import argparse
import numpy as np

def load_apc_data(json_path):
    """Load APC candidates and compute effectiveness."""
    with open(json_path, 'r') as f:
        data = json.load(f)
    
    return pd.DataFrame([{
        'opcode': item['opcode'],
        'effectiveness': item['total_width_before'] / item['total_width_after'],
        'instructions': len(item['original_instructions']),
        'software_version_cells': item['total_width_before'] * item['execution_frequency']
    } for item in data])

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

def get_instruction_group(count):
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

def plot_effectiveness(json_path, filename=None):
    """Generate bar plot of effectiveness data."""
    df = load_apc_data(json_path)
    total_cells = df['software_version_cells'].sum()
    
    # Print top 10 basic blocks
    top10 = df.nlargest(10, 'software_version_cells')[['opcode', 'software_version_cells', 'effectiveness', 'instructions']]
    top10['software_version_cells'] = top10['software_version_cells'].apply(format_cell_count)
    top10.columns = ['Opcode', 'Trace Cells', 'Effectiveness', 'Instructions']
    print("\nTop 10 Basic Blocks by Trace Cells:")
    print(top10.to_string(index=False))
    print()
    
    # Calculate weighted mean effectiveness
    mean_effectiveness = (df['effectiveness'] * df['software_version_cells']).sum() / total_cells
    
    # Separate large and small APCs (< 0.1% threshold)
    threshold = total_cells * 0.001
    df_large = df[df['software_version_cells'] >= threshold].copy()
    df_small = df[df['software_version_cells'] < threshold]
    
    # Sort large APCs by cost
    df_large = df_large.sort_values('software_version_cells', ascending=False)
    df_large['inst_group'] = df_large['instructions'].apply(get_instruction_group)
    
    # Create 'Other' entry if there are small APCs
    if len(df_small) > 0:
        other_cells = df_small['software_version_cells'].sum()
        other_effectiveness = (df_small['effectiveness'] * df_small['software_version_cells']).sum() / other_cells
        other_row = pd.DataFrame([{
            'effectiveness': other_effectiveness,
            'software_version_cells': other_cells,
            'inst_group': 'Other'
        }])
        df_plot = pd.concat([df_large, other_row], ignore_index=True)
    else:
        df_plot = df_large
    
    # Create color map
    inst_groups = ['1', '2', '3', '4', '5', '6', '7', '8', '9', 
                   '10-14', '15-19', '20-29', '30-49', '50-100', '>100']
    colors = plt.cm.viridis(np.linspace(0, 1, len(inst_groups)))
    color_map = dict(zip(inst_groups, colors))
    color_map['Other'] = 'lightgray'
    
    # Create plot
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Plot bars
    x_pos = 0
    for idx, row in df_plot.iterrows():
        width = row['software_version_cells']
        ax.bar(x_pos + width/2, row['effectiveness'], width=width,
               color=color_map[row['inst_group']], 
               edgecolor='black', linewidth=0.5, alpha=0.8)
        
        # Label 'Other' box
        if row['inst_group'] == 'Other':
            ax.text(x_pos + width/2, row['effectiveness']/2, 
                   f'Other\n({len(df_small)} APCs)',
                   ha='center', va='center', fontsize=10, 
                   color='black', weight='bold')
        
        x_pos += width
    
    # Formatting
    ax.set_xlabel('Cumulative instruction trace cells (software version)', fontsize=12)
    ax.set_ylabel('Effectiveness', fontsize=12)
    ax.set_title("Effectiveness by Basic Block", fontsize=14)
    ax.grid(True, alpha=0.3, axis='y')
    ax.axhline(mean_effectiveness, color='red', linestyle='--', linewidth=2, alpha=0.7)
    
    # Format x-axis
    ax.set_xlim(0, total_cells)
    x_ticks = ax.get_xticks()
    ax.set_xticklabels([format_cell_count(x) for x in x_ticks])
    
    # Create legend
    legend_elements = []
    present_groups = df_plot['inst_group'].unique()
    for group in inst_groups:
        if group in present_groups:
            label = f'{group} instruction{"s" if group != "1" else ""}'
            legend_elements.append(mpatches.Patch(color=color_map[group], label=label))
    if 'Other' in present_groups:
        legend_elements.append(mpatches.Patch(color=color_map['Other'], label='Other'))
    
    ax.legend(handles=legend_elements, title='Basic Block size', 
              loc='center left', bbox_to_anchor=(1, 0.5), 
              frameon=True, fancybox=True, shadow=True)
    
    # Add mean text
    ax.text(0.02, 0.97, f'Mean: {mean_effectiveness:.2f}', 
            transform=ax.transAxes, fontsize=10, verticalalignment='top',
            bbox=dict(boxstyle='round,pad=0.5', facecolor='wheat', alpha=0.8))
    
    plt.tight_layout()
    plt.subplots_adjust(right=0.85)
    
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
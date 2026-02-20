#!/usr/bin/env python3
"""
Simple APC Candidates JSON Parser

This script parses the apc_candidates.json file and extracts key information
in a concise format.
"""

import json
import sys
import argparse
from pathlib import Path
from tabulate import tabulate


def main():
    """Parse APC candidates and show key information."""
    parser = argparse.ArgumentParser(description="Parse APC candidates and show key information.")
    parser.add_argument("json_file", help="Path to the APC candidates JSON file")
    parser.add_argument("-o", "--output", help="Output file (default: stdout)", default=None)
    args = parser.parse_args()
    
    json_file = Path(args.json_file)
    output_file = args.output
    
    if not json_file.exists():
        print(f"Error: File {json_file} not found!")
        sys.exit(1)
    
    try:
        with open(json_file, 'r') as f:
            data = json.load(f)["apcs"]
    except Exception as e:
        print(f"Error reading file: {e}")
        sys.exit(1)
    
    # Capture output to write to file
    output_lines = []
    
    # Process and calculate densitys for each candidate
    candidates_with_densitys = []
    
    for i, candidate in enumerate(data):
        start_pc = candidate["original_block"]["start_pc"]
        freq = candidate["execution_frequency"]
        num_instructions = len(candidate["original_block"]["instructions"])
        
        # Get optimization stats
        before_constraints = candidate["stats"]["before"]["constraints"]
        after_constraints = candidate["stats"]["after"]["constraints"]
        before_main_columns = candidate["stats"]["before"]["main_columns"]
        after_main_columns = candidate["stats"]["after"]["main_columns"]
        before_bus_interactions = candidate["stats"]["before"]["bus_interactions"]
        after_bus_interactions = candidate["stats"]["after"]["bus_interactions"]
        value = candidate["value"]
        cost_before = candidate["cost_before"]
        cost_after = candidate["cost_after"]
        
        # Calculate improvements as factors (before/after ratios)
        cost_improvement_factor = cost_before / cost_after
        constraint_improvement_factor = before_constraints / after_constraints
        main_columns_improvement_factor = before_main_columns / after_main_columns
        bus_interactions_improvement_factor = before_bus_interactions / after_bus_interactions

        # Calculate density used for ranking candidates
        density = value / cost_after
        
        candidates_with_densitys.append({
            'index': i + 1,
            'start_pc': start_pc,
            'freq': freq,
            'num_instructions': num_instructions,
            'before_constraints': before_constraints,
            'after_constraints': after_constraints,
            'before_main_columns': before_main_columns,
            'after_main_columns': after_main_columns,
            'before_bus_interactions': before_bus_interactions,
            'after_bus_interactions': after_bus_interactions,
            'cost_improvement_factor': cost_improvement_factor,
            'constraint_improvement_factor': constraint_improvement_factor,
            'main_columns_improvement_factor': main_columns_improvement_factor,
            'bus_interactions_improvement_factor': bus_interactions_improvement_factor,
            'value': value,
            'cost_before': cost_before,
            'cost_after': cost_after,
            'density': density,
        })
    
    # Sort by descending density
    candidates_with_densitys.sort(key=lambda x: x['density'], reverse=True)
    
    # Summary statistics (moved to top)
    output_lines.append("")
    output_lines.append("=" * 120)
    output_lines.append(f"SUMMARY STATISTICS OVER ALL APC CANDIDATES")
    output_lines.append("=" * 120)
    
    total_candidates = len(data)
    total_instructions = sum(len(c["original_block"]["instructions"]) for c in data)
    
    total_cost_before = sum(c["cost_before"] for c in data)
    total_cost_after = sum(c["cost_after"] for c in data)
    total_cost_improvement_factor = total_cost_before / total_cost_after
    
    total_before_constraints = sum(c["stats"]["before"]["constraints"] for c in data)
    total_after_constraints = sum(c["stats"]["after"]["constraints"] for c in data)
    total_constraint_improvement_factor = total_before_constraints / total_after_constraints
    
    total_before_main_columns = sum(c["stats"]["before"]["main_columns"] for c in data)
    total_after_main_columns = sum(c["stats"]["after"]["main_columns"] for c in data)
    main_columns_improvement_factor = total_before_main_columns / total_after_main_columns
    
    total_before_bus_interactions = sum(c["stats"]["before"]["bus_interactions"] for c in data)
    total_after_bus_interactions = sum(c["stats"]["after"]["bus_interactions"] for c in data)
    total_bus_interactions_improvement_factor = total_before_bus_interactions / total_after_bus_interactions
    
    output_lines.append(f"# of APC Candidates: {total_candidates}")
    output_lines.append(f"Sum of Instructions: {total_instructions}")
    output_lines.append(f"Average Instructions per APC Candidate: {total_instructions / total_candidates:.1f}")
    output_lines.append("")
    output_lines.append(f"Sum of Cost: {total_cost_before} → {total_cost_after} ({total_cost_improvement_factor:.2f}x reduction)")
    output_lines.append(f"Sum of Main Columns: {total_before_main_columns} → {total_after_main_columns} ({main_columns_improvement_factor:.2f}x reduction)")
    output_lines.append(f"Sum of Constraints: {total_before_constraints} → {total_after_constraints} ({total_constraint_improvement_factor:.2f}x reduction)")
    output_lines.append(f"Sum of Bus Interactions: {total_before_bus_interactions} → {total_after_bus_interactions} ({total_bus_interactions_improvement_factor:.2f}x reduction)")
    
    # Statement count distribution
    stmt_dist = {}
    for c in data:
        stmt_count = len(c["original_block"]["instructions"])
        stmt_dist[stmt_count] = stmt_dist.get(stmt_count, 0) + 1
    
    output_lines.append("")
    output_lines.append("# of Instructions Distribution:")
    stmt_table_data = []
    for stmt_count in sorted(stmt_dist.keys()):
        count = stmt_dist[stmt_count]
        percentage = (count / total_candidates) * 100
        stmt_table_data.append([stmt_count, count, f"{percentage:.1f}%"])
    
    stmt_table_headers = ["Instructions", "# of Candidates", "Percentage"]
    stmt_table_output = tabulate(stmt_table_data, headers=stmt_table_headers, tablefmt="grid")
    output_lines.append(stmt_table_output)
    
    # Frequency distribution
    freq_dist = {}
    for c in data:
        freq = c["execution_frequency"]
        freq_dist[freq] = freq_dist.get(freq, 0) + 1
    
    output_lines.append("")
    output_lines.append("Execution Frequency Distribution:")
    freq_table_data = []
    for freq in sorted(freq_dist.keys()):
        count = freq_dist[freq]
        percentage = (count / total_candidates) * 100
        freq_table_data.append([f"{freq}x", count, f"{percentage:.1f}%"])
    
    freq_table_headers = ["Frequency", "# of Candidates", "Percentage"]
    freq_table_output = tabulate(freq_table_data, headers=freq_table_headers, tablefmt="grid")
    output_lines.append(freq_table_output)
    
    # Show sorted candidates by density using tabulate
    output_lines.append("")
    output_lines.append("=" * 120)
    output_lines.append("APC CANDIDATES RANKED BY DENSITY (VALUE / COST_AFTER)")
    output_lines.append("=" * 120)
    
    # Prepare table data for tabulate
    table_headers = [
        "Rank", "Start PC", "# of Instr", "Freq", "Value", "Cost Before -> After (Redux)", 
        "Density", "Main Cols Before -> After (Redux)",
        "Constraints Before -> After (Redux)", "Bus Int Before -> After (Redux)"
    ]
    
    table_data = []
    for i, candidate in enumerate(candidates_with_densitys):
        row = [
            i + 1,
            f"{candidate['start_pc']:.0f}",
            candidate['num_instructions'],
            f"{candidate['freq']}x",
            f"{candidate['value']:.0f}",
            f"{candidate['cost_before']:.0f} -> {candidate['cost_after']:.0f} ({candidate['cost_improvement_factor']:.1f}x)",
            f"{candidate['density']:.2f}",
            f"{candidate['before_main_columns']} -> {candidate['after_main_columns']} ({candidate['main_columns_improvement_factor']:.1f}x)",
            f"{candidate['before_constraints']} -> {candidate['after_constraints']} ({candidate['constraint_improvement_factor']:.1f}x)",
            f"{candidate['before_bus_interactions']} -> {candidate['after_bus_interactions']} ({candidate['bus_interactions_improvement_factor']:.1f}x)"
        ]
        table_data.append(row)
    
    # Generate table using tabulate
    table_output = tabulate(table_data, headers=table_headers, tablefmt="grid")
    output_lines.append(table_output)
    
    # Write output to file or stdout
    try:
        if output_file:
            with open(output_file, 'w') as f:
                for line in output_lines:
                    f.write(line + '\n')
            print(f"Output written to: {output_file}")
        else:
            # Write to stdout
            for line in output_lines:
                print(line)
    except Exception as e:
        print(f"Error writing to output file: {e}")
        # Fallback to console output
        for line in output_lines:
            print(line)


if __name__ == "__main__":
    main() 

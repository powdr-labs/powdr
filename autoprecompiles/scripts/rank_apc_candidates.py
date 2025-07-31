#!/usr/bin/env python3
"""
Simple APC Candidates JSON Parser

This script parses the apc_candidates.json file and extracts key information
in a concise format.
"""

import json
import sys
from pathlib import Path


def main():
    """Parse APC candidates and show key information."""
    if len(sys.argv) != 3:
        print("Usage: python rank_apc_candidates.py <path_to_apc_candidates.json> <output_file>")
        print("Example: python rank_apc_candidates.py apc_candidates.json output.txt")
        sys.exit(1)
    
    json_file = Path(sys.argv[1])
    output_file = Path(sys.argv[2])
    
    if not json_file.exists():
        print(f"Error: File {json_file} not found!")
        sys.exit(1)
    
    try:
        with open(json_file, 'r') as f:
            data = json.load(f)
    except Exception as e:
        print(f"Error reading file: {e}")
        sys.exit(1)
    
    # Capture output to write to file
    output_lines = []
    
    output_lines.append(f"Found {len(data)} APC candidates")
    output_lines.append("=" * 120)
    
    # Process and calculate densitys for each candidate
    candidates_with_densitys = []
    
    for i, candidate in enumerate(data):
        start_pc = candidate["start_pc"]
        freq = candidate["execution_frequency"]
        num_statements = len(candidate["original_block"]["statements"])
        
        # Get optimization stats
        before_constraints = candidate["stats"]["before"]["constraints"]
        after_constraints = candidate["stats"]["after"]["constraints"]
        before_total_columns = candidate["total_width_before"]
        after_total_columns = candidate["total_width_after"]
        before_bus_interactions = candidate["stats"]["before"]["bus_interactions"]
        after_bus_interactions = candidate["stats"]["after"]["bus_interactions"]
        value = candidate["value"]
        cost = candidate["cost"]
        
        # Calculate improvements as factors (before/after ratios)
        constraint_improvement_factor = before_constraints / after_constraints
        total_columns_improvement_factor = before_total_columns / after_total_columns
        bus_interactions_improvement_factor = before_bus_interactions / after_bus_interactions
        
        # Calculate cells saved: freq * (before_total_columns - after_total_columns)
        cells_saved = freq * (before_total_columns - after_total_columns)

        # Calculate density used for ranking candidates
        density = value / cost
        
        candidates_with_densitys.append({
            'index': i + 1,
            'start_pc': start_pc,
            'freq': freq,
            'num_statements': num_statements,
            'before_constraints': before_constraints,
            'after_constraints': after_constraints,
            'before_total_columns': before_total_columns,
            'after_total_columns': after_total_columns,
            'before_bus_interactions': before_bus_interactions,
            'after_bus_interactions': after_bus_interactions,
            'constraint_improvement_factor': constraint_improvement_factor,
            'total_columns_improvement_factor': total_columns_improvement_factor,
            'bus_interactions_improvement_factor': bus_interactions_improvement_factor,
            'value': value,
            'cost': cost,
            'density': density,
            'cells_saved': cells_saved
        })
    
    # Sort by descending density
    candidates_with_densitys.sort(key=lambda x: x['density'], reverse=True)
    
    # Summary statistics (moved to top)
    output_lines.append("")
    output_lines.append("=" * 120)
    output_lines.append("SUMMARY STATISTICS")
    output_lines.append("=" * 120)
    
    total_candidates = len(data)
    total_statements = sum(len(c["original_block"]["statements"]) for c in data)
    
    total_before_constraints = sum(c["stats"]["before"]["constraints"] for c in data)
    total_after_constraints = sum(c["stats"]["after"]["constraints"] for c in data)
    total_constraint_improvement_factor = total_before_constraints / total_after_constraints
    
    total_before_total_columns = sum(c["total_width_before"] for c in data)
    total_after_total_columns = sum(c["total_width_after"] for c in data)
    total_columns_improvement_factor = total_before_total_columns / total_after_total_columns
    
    total_before_bus_interactions = sum(c["stats"]["before"]["bus_interactions"] for c in data)
    total_after_bus_interactions = sum(c["stats"]["after"]["bus_interactions"] for c in data)
    total_bus_interactions_improvement_factor = total_before_bus_interactions / total_after_bus_interactions
    
    output_lines.append(f"Total candidates: {total_candidates}")
    output_lines.append(f"Total statements: {total_statements}")
    output_lines.append(f"Average statements per candidate: {total_statements / total_candidates:.1f}")
    output_lines.append("")
    output_lines.append(f"Total Constraints: {total_before_constraints} → {total_after_constraints} ({total_constraint_improvement_factor:.2f}x reduction)")
    output_lines.append(f"Total Columns: {total_before_total_columns} → {total_after_total_columns} ({total_columns_improvement_factor:.2f}x reduction)")
    output_lines.append(f"Total Bus Interactions: {total_before_bus_interactions} → {total_after_bus_interactions} ({total_bus_interactions_improvement_factor:.2f}x reduction)")
    
    # Statement count distribution
    stmt_dist = {}
    for c in data:
        stmt_count = len(c["original_block"]["statements"])
        stmt_dist[stmt_count] = stmt_dist.get(stmt_count, 0) + 1
    
    output_lines.append("")
    output_lines.append("Statement count distribution:")
    for stmt_count in sorted(stmt_dist.keys()):
        count = stmt_dist[stmt_count]
        percentage = (count / total_candidates) * 100
        output_lines.append(f"  {stmt_count:2d} statements: {count:3d} candidates ({percentage:5.1f}%)")
    
    # Frequency distribution
    freq_dist = {}
    for c in data:
        freq = c["execution_frequency"]
        freq_dist[freq] = freq_dist.get(freq, 0) + 1
    
    output_lines.append("")
    output_lines.append("Execution frequency distribution:")
    for freq in sorted(freq_dist.keys()):
        count = freq_dist[freq]
        percentage = (count / total_candidates) * 100
        output_lines.append(f"  {freq:2d}x: {count:3d} candidates ({percentage:5.1f}%)")
    
    # Show sorted candidates by density
    output_lines.append("")
    output_lines.append("=" * 120)
    output_lines.append("CANDIDATES RANKED BY DENSITY")
    output_lines.append("=" * 120)
    
    for i, candidate in enumerate(candidates_with_densitys):
        output_lines.append(f"{i+1:3d}. PC: {candidate['start_pc']:10d} | "
              f"Statements: {candidate['num_statements']:2d} | "
              f"Freq: {candidate['freq']:2d}x | "
              f"Total Cols: {candidate['before_total_columns']:3d} → {candidate['after_total_columns']:3d} "
              f"({candidate['total_columns_improvement_factor']:.1f}x reduction) | "
              f"Cells Saved: {candidate['cells_saved']:6d} | "
              f"Value: {candidate['value']:6d} | "
              f"Cost: {candidate['cost']:6d} | "
              f"Density: {candidate['density']:6.2f} | "
              f"Constraints: {candidate['before_constraints']:3d} → {candidate['after_constraints']:3d} "
              f"({candidate['constraint_improvement_factor']:.1f}x reduction) | "
              f"Bus Int: {candidate['before_bus_interactions']:3d} → {candidate['after_bus_interactions']:3d} "
              f"({candidate['bus_interactions_improvement_factor']:.1f}x reduction)")
    
    # Write output to file
    try:
        with open(output_file, 'w') as f:
            for line in output_lines:
                f.write(line + '\n')
        print(f"Output written to: {output_file}")
    except Exception as e:
        print(f"Error writing to output file: {e}")
        # Fallback to console output
        for line in output_lines:
            print(line)


if __name__ == "__main__":
    main() 

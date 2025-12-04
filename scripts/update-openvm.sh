#!/bin/bash

# Script to update openvm git revision hashes across the repository.
#
# Usage:
#   ./scripts/update-openvm.sh <new-openvm-rev>
#
# Example:
#   ./scripts/update-openvm.sh v1.5.0-powdr

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

OPENVM_REV="$1"

if [[ -z "$OPENVM_REV" ]]; then
    echo "Usage: $0 <new-openvm-rev>"
    echo ""
    echo "Example: $0 v1.5.0-powdr"
    echo ""
    echo "This script updates all git revision references for:"
    echo "  - openvm dependencies (from powdr-labs/openvm.git)"
    exit 1
fi

echo "Updating openvm dependencies to: $OPENVM_REV"
echo ""

# Find all Cargo.toml files with openvm git dependencies
# Store in an array to safely handle paths with spaces
CARGO_FILES=()
while IFS= read -r file; do
    [[ -n "$file" ]] && CARGO_FILES+=("$file")
done < <(find "$REPO_ROOT" -name "Cargo.toml" -exec grep -l 'powdr-labs/openvm.git' {} \; 2>/dev/null || true)

if [[ ${#CARGO_FILES[@]} -eq 0 ]]; then
    echo "No Cargo.toml files with openvm dependencies found."
    exit 0
fi

for file in "${CARGO_FILES[@]}"; do
    echo "Updating $file"
    
    # Update openvm revisions
    # Match: rev = "..." after powdr-labs/openvm.git
    sed -i -E 's|(git = "https://github.com/powdr-labs/openvm.git", rev = ")[^"]+(")|'"\1${OPENVM_REV}\2|g" "$file"
done

echo ""
echo "Done! Updated the following files:"
for file in "${CARGO_FILES[@]}"; do
    echo "  - ${file#"$REPO_ROOT"/}"
done

echo ""
echo "Please review the changes and run 'cargo check' to verify."

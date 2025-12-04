#!/bin/bash

# Script to update stark-backend git revision hashes across the repository.
#
# Usage:
#   ./scripts/update-stark-backend.sh <new-stark-backend-rev>
#
# Example:
#   ./scripts/update-stark-backend.sh v1.3.0-powdr

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

STARK_BACKEND_REV="$1"

if [[ -z "$STARK_BACKEND_REV" ]]; then
    echo "Usage: $0 <new-stark-backend-rev>"
    echo ""
    echo "Example: $0 v1.3.0-powdr"
    echo ""
    echo "This script updates all git revision references for:"
    echo "  - stark-backend dependencies (from powdr-labs/stark-backend.git)"
    exit 1
fi

echo "Updating stark-backend dependencies to: $STARK_BACKEND_REV"
echo ""

# Find all Cargo.toml files with stark-backend git dependencies
# Store in an array to safely handle paths with spaces
CARGO_FILES=()
while IFS= read -r file; do
    [[ -n "$file" ]] && CARGO_FILES+=("$file")
done < <(find "$REPO_ROOT" -name "Cargo.toml" -exec grep -l 'powdr-labs/stark-backend.git' {} \; 2>/dev/null || true)

if [[ ${#CARGO_FILES[@]} -eq 0 ]]; then
    echo "No Cargo.toml files with stark-backend dependencies found."
    exit 0
fi

for file in "${CARGO_FILES[@]}"; do
    echo "Updating $file"
    
    # Update stark-backend revisions
    # Match: rev = "..." after powdr-labs/stark-backend.git
    sed -i -E 's|(git = "https://github.com/powdr-labs/stark-backend.git", rev = ")[^"]+(")|'"\1${STARK_BACKEND_REV}\2|g" "$file"
done

echo ""
echo "Done! Updated the following files:"
for file in "${CARGO_FILES[@]}"; do
    echo "  - ${file#"$REPO_ROOT"/}"
done

echo ""
echo "Please review the changes and run 'cargo check' to verify."

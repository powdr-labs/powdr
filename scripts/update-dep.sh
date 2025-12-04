#!/bin/bash

# Script to update openvm or stark-backend git revision hashes across the repository.
#
# Usage:
#   ./scripts/update-dep.sh openvm <new-rev>
#   ./scripts/update-dep.sh stark-backend <new-rev>
#
# Examples:
#   ./scripts/update-dep.sh openvm v1.5.0-powdr
#   ./scripts/update-dep.sh stark-backend v1.3.0-powdr

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

DEP_TYPE="$1"
NEW_REV="$2"

usage() {
    echo "Usage: $0 <openvm|stark-backend> <new-rev>"
    echo ""
    echo "Examples:"
    echo "  $0 openvm v1.5.0-powdr"
    echo "  $0 stark-backend v1.3.0-powdr"
    echo ""
    echo "This script updates all git revision references for the specified dependency."
    exit 1
}

if [[ -z "$DEP_TYPE" ]] || [[ -z "$NEW_REV" ]]; then
    usage
fi

case "$DEP_TYPE" in
    openvm)
        GREP_PATTERN='powdr-labs/openvm.git'
        GIT_URL='https://github.com/powdr-labs/openvm.git'
        ;;
    stark-backend)
        GREP_PATTERN='powdr-labs/stark-backend.git'
        GIT_URL='https://github.com/powdr-labs/stark-backend.git'
        ;;
    *)
        echo "Error: Unknown dependency type '$DEP_TYPE'"
        echo ""
        usage
        ;;
esac

echo "Updating $DEP_TYPE dependencies to: $NEW_REV"
echo ""

# Find all Cargo.toml files with the specified git dependencies
# Store in an array to safely handle paths with spaces
CARGO_FILES=()
while IFS= read -r file; do
    [[ -n "$file" ]] && CARGO_FILES+=("$file")
done < <(find "$REPO_ROOT" -name "Cargo.toml" -exec grep -l "$GREP_PATTERN" {} \; 2>/dev/null || true)

if [[ ${#CARGO_FILES[@]} -eq 0 ]]; then
    echo "No Cargo.toml files with $DEP_TYPE dependencies found."
    exit 0
fi

for file in "${CARGO_FILES[@]}"; do
    echo "Updating $file"
    
    # Update revisions
    # Match: rev = "..." after the git URL
    sed -i -E 's|(git = "'"$GIT_URL"'", rev = ")[^"]+(")|'"\1${NEW_REV}\2|g" "$file"
done

echo ""
echo "Done! Updated the following files:"
for file in "${CARGO_FILES[@]}"; do
    echo "  - ${file#"$REPO_ROOT"/}"
done

echo ""
echo "Please review the changes and run 'cargo check' to verify."

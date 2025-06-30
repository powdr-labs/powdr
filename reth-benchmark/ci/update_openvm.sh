#!/bin/bash
set -euo pipefail

# Function to display usage
usage() {
    echo "Usage: $0 <new_openvm_rev>"
    echo "Updates dependencies in Cargo.toml files"
    exit 1
}

# Check arguments
if [ $# -ne 1 ]; then
    usage
fi

NEW_OPENVM_REV=$1

echo "Updating dependencies:"
echo "  - OpenVM revision: $NEW_OPENVM_REV"

# Step 1: Replace all openvm.git imports with new openvm rev
echo "Updating openvm.git dependencies to $NEW_OPENVM_REV..."
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    find . -name "*.toml" -type f -exec sed -i '' "s/\(openvm[^= ]*\) *= *{ *git *= *\"https:\/\/github.com\/openvm-org\/openvm.git\", *rev *= *\"[^\"]*\"/\1 = { git = \"https:\/\/github.com\/openvm-org\/openvm.git\", rev = \"$NEW_OPENVM_REV\"/g" {} \;
else
    # Linux and others
    find . -name "*.toml" -type f -exec sed -i "s/\(openvm[^= ]*\) *= *{ *git *= *\"https:\/\/github.com\/openvm-org\/openvm.git\", *rev *= *\"[^\"]*\"/\1 = { git = \"https:\/\/github.com\/openvm-org\/openvm.git\", rev = \"$NEW_OPENVM_REV\"/g" {} \;
fi

# Step 2: Clone openvm with new rev to get the stark-backend rev
echo "Cloning openvm at $NEW_OPENVM_REV to determine stark-backend revision..."
TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

git clone --quiet --depth 1 https://github.com/powdr-labs/openvm.git "$TMP_DIR" > /dev/null 2>&1
cd "$TMP_DIR"
git checkout --quiet "$NEW_OPENVM_REV" > /dev/null 2>&1

# Initialize the STARK_BACKEND_REV variable
STARK_BACKEND_REV=""

# If not found, check root Cargo.toml
if [ -z "$STARK_BACKEND_REV" ] && [ -f "Cargo.toml" ]; then
    STARK_BACKEND_REV=$(grep 'openvm-stark-backend.*rev' Cargo.toml | sed -E 's/.*rev *= *"([^"]+)".*/\1/' || echo "")
    
    # If still not found, try to look in workspace dependencies
    if [ -z "$STARK_BACKEND_REV" ]; then
        STARK_BACKEND_REV=$(grep 'openvm-stark-backend.*rev' Cargo.toml -A 5 | grep 'rev' | head -1 | sed -E 's/.*rev *= *"([^"]+)".*/\1/' || echo "")
    fi
fi

cd - > /dev/null

# Step 3: Replace all stark-backend.git imports with the determined rev
if [ -n "$STARK_BACKEND_REV" ]; then
    echo "Updating stark-backend.git dependencies to $STARK_BACKEND_REV..."
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        find . -name "*.toml" -type f -exec sed -i '' "s/\(openvm-stark-[^= ]*\) *= *{ *git *= *\"https:\/\/github.com\/openvm-org\/stark-backend.git\", *rev *= *\"[^\"]*\"/\1 = { git = \"https:\/\/github.com\/openvm-org\/stark-backend.git\", rev = \"$STARK_BACKEND_REV\"/g" {} \;
    else
        # Linux and others
        find . -name "*.toml" -type f -exec sed -i "s/\(openvm-stark-[^= ]*\) *= *{ *git *= *\"https:\/\/github.com\/openvm-org\/stark-backend.git\", *rev *= *\"[^\"]*\"/\1 = { git = \"https:\/\/github.com\/openvm-org\/stark-backend.git\", rev = \"$STARK_BACKEND_REV\"/g" {} \;
    fi
else
    echo "Warning: Could not determine stark-backend revision from openvm repo. Not updating stark-backend dependencies."
fi

echo "Dependencies updated successfully!"

exit 0 
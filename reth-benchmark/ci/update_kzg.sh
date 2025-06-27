#!/bin/bash
set -euo pipefail

# Function to display usage
usage() {
    echo "Usage: $0 <new_kzg_rev>"
    echo "Updates dependencies in Cargo.toml files"
    exit 1
}

# Check arguments
if [ $# -ne 1 ]; then
    usage
fi

NEW_KZG_REV=$1

echo "Updating dependencies:"
echo "  - KZG revision: $NEW_KZG_REV"

# Step 4: Update the KZG dependency
echo "Updating openvm-kzg dependencies to $NEW_KZG_REV..."
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    find . -name "*.toml" -type f -exec sed -i '' "s/openvm-kzg *= *{ *git *= *\"[^\"]*\", *rev *= *\"[^\"]*\"/openvm-kzg = { git = \"https:\/\/github.com\/axiom-crypto\/openvm-kzg.git\", rev = \"$NEW_KZG_REV\"/g" {} \;
    find . -name "*.lock" -type f -exec sed -i '' "s/openvm-kzg.git?rev=[^#]*#/openvm-kzg.git?rev=$NEW_KZG_REV#/g" {} \;
else
    # Linux and others
    find . -name "*.toml" -type f -exec sed -i "s/openvm-kzg *= *{ *git *= *\"[^\"]*\", *rev *= *\"[^\"]*\"/openvm-kzg = { git = \"https:\/\/github.com\/axiom-crypto\/openvm-kzg.git\", rev = \"$NEW_KZG_REV\"/g" {} \;
    find . -name "*.lock" -type f -exec sed -i "s/openvm-kzg.git?rev=[^#]*#/openvm-kzg.git?rev=$NEW_KZG_REV#/g" {} \;
fi

echo "Dependencies updated successfully!"

exit 0 
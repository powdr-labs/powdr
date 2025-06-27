#!/bin/bash

old_rev=$1
new_rev=$2

if [ -z "$old_rev" ] || [ -z "$new_rev" ]; then
    echo "Usage: $0 <old_rev> <new_rev>"
    exit 1
fi

# Check OS and use appropriate sed syntax
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    find . -name "*.toml" -type f -exec sed -i '' "s/rev = \"$old_rev\"/rev = \"$new_rev\"/g" {} +
else
    # Linux and others
    find . -name "*.toml" -type f -exec sed -i "s/rev = \"$old_rev\"/rev = \"$new_rev\"/g" {} +
fi

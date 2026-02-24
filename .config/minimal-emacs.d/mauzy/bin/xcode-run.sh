#!/usr/bin/env bash
set -e

SCHEME="$1"

# Build
xcodebuild -scheme "$SCHEME" build -destination 'platform=macOS'  2>&1 | xcbeautify

# Get build settings
SETTINGS=$(xcodebuild -scheme "$SCHEME" -showBuildSettings 2>/dev/null)
PRODUCTS_DIR=$(echo "$SETTINGS" | grep -m1 BUILT_PRODUCTS_DIR | awk '{print $3}')
FULL_PRODUCT_NAME=$(echo "$SETTINGS" | grep -m1 FULL_PRODUCT_NAME | awk '{print $3}')

# Run it
open "$PRODUCTS_DIR/$FULL_PRODUCT_NAME"

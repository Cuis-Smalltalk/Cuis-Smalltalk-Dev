#!/bin/bash

set -euo pipefail

IMAGE_FILE="$(ls | grep 'Cuis5.0-[0-9]\+.image')"
TEST_TEMPORAL_DIR=$(mktemp -d)
TEST_TEMPORAL_IMG="$TEST_TEMPORAL_DIR/cuis.image"

cp "$IMAGE_FILE" "$TEST_TEMPORAL_IMG"

sqcogspur64linuxht/bin/squeak -vm-display-null "$TEST_TEMPORAL_IMG" -s .ContinuousIntegrationScripts/runTests.st

rm -r "$TEST_TEMPORAL_DIR"

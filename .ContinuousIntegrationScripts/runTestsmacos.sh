#!/bin/bash

set -euo pipefail

IMAGE_FILE="$(ls | grep 'Cuis5.0-[0-9]\+.image')"
TEST_TEMPORAL_DIR=$(mktemp -d)
TEST_TEMPORAL_IMG="$TEST_TEMPORAL_DIR/cuis.image"

/Applications/Squeak.app/Contents/MacOS/Squeak -version
/Applications/Squeak.app/Contents/MacOS/Squeak -headless "$TEST_TEMPORAL_IMG" -s .ContinuousIntegrationScripts/runTests.st

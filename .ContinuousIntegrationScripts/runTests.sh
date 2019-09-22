#!/bin/bash

set -euo pipefail

IMAGE_FILE="$(ls | grep 'Cuis5.0-[0-9]\+.image')"
RUN_TESTS_SCRIPT_FILEPATH=".ContinuousIntegrationScripts/runTests.st"

runTestsOnLinux() {
  TEST_TEMPORAL_DIR=$(mktemp -d)
  TEST_TEMPORAL_IMG="$TEST_TEMPORAL_DIR/cuis.image"

  cp "$IMAGE_FILE" "$TEST_TEMPORAL_IMG"

  sqcogspur64linuxht/bin/squeak -vm-display-null "$TEST_TEMPORAL_IMG" -s "$RUN_TESTS_SCRIPT_FILEPATH"

  rm -r "$TEST_TEMPORAL_DIR"
}

runTestsOnMacOS() {
  /Applications/Squeak.app/Contents/MacOS/Squeak -headless "$IMAGE_FILE" -s "$RUN_TESTS_SCRIPT_FILEPATH"
}

case $TRAVIS_OS_NAME in
  "linux")
    runTestsOnLinux ;;
  "osx")
    runTestsOnMacOS ;;
esac

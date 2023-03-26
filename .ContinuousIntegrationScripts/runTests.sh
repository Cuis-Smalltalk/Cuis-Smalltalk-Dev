#!/bin/bash

set -euo pipefail

IMAGE_FILE="$(ls | grep 'Cuis6.0-[0-9]\+.image')"
RUN_TESTS_SCRIPT_FILEPATH=".ContinuousIntegrationScripts/runTests.st"

runTestsOnLinux() {
  /home/runner/work/Cuis-Smalltalk-Dev/Cuis-Smalltalk-Dev/sqcogspur64linux/squeak -vm-display-null "$IMAGE_FILE" -s "$RUN_TESTS_SCRIPT_FILEPATH"
}

runTestsOnMacOS() {
  /Applications/Squeak.app/Contents/MacOS/Squeak -headless "$IMAGE_FILE" -s "$RUN_TESTS_SCRIPT_FILEPATH"
}

case $RUNNER_OS in
  "Linux")
    runTestsOnLinux ;;
  "macOS")
    runTestsOnMacOS ;;
esac

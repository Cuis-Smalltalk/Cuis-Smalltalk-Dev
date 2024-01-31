#!/bin/bash

set -euo pipefail

IMAGE_FILE="$(ls CuisImage/ | grep 'Cuis6.3-[0-9]\+.image')"
RUN_TESTS_SCRIPT_FILEPATH=".ContinuousIntegrationScripts/runTests.st"

runTestsOnLinux() {
  /home/runner/work/Cuis-Smalltalk-Dev/Cuis-Smalltalk-Dev/sqcogspur64linux/squeak -vm-display-null CuisImage/"$IMAGE_FILE" -s "$RUN_TESTS_SCRIPT_FILEPATH"
}

runTestsOnMacOS() {
  /Applications/Squeak.app/Contents/MacOS/Squeak -headless CuisImage/"$IMAGE_FILE" -s "$RUN_TESTS_SCRIPT_FILEPATH"
}

case $RUNNER_OS in
  "Linux")
    runTestsOnLinux ;;
  "macOS")
    runTestsOnMacOS ;;
esac

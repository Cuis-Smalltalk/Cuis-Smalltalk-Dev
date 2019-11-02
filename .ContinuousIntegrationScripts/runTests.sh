#!/bin/bash

set -euo pipefail

IMAGE_FILE="$(ls | grep 'Cuis5.0-[0-9]\+.image')"
RUN_TESTS_SCRIPT_FILEPATH=".ContinuousIntegrationScripts/runTests.st"

runTestsOnLinux() {
  sqcogspur64linuxht/bin/squeak -vm-display-null "$IMAGE_FILE" -s "$RUN_TESTS_SCRIPT_FILEPATH"
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

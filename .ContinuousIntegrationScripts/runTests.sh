#!/bin/bash

set -euo pipefail

CURRENT_SCRIPT_FOLDER=$(dirname "$(realpath "$0")")

"$CURRENT_SCRIPT_FOLDER/runCuis.sh" -s .ContinuousIntegrationScripts/runTests.st
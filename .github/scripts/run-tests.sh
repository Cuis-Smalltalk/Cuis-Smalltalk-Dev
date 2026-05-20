#!/bin/bash

set -euo pipefail

CURRENT_SCRIPT_FOLDER=$(dirname "$(realpath "$0")")

"$CURRENT_SCRIPT_FOLDER/run-cuis.sh" -s .github/scripts/run-tests.st
#!/bin/bash
# exportTonel.sh — Export all loaded packages as Tonel .st files.
#
# Requires CUIS_VM_PATH and CUIS_VM_ARGUMENTS to be set (done by installVm.sh).
# Output lands in tonel-export/ relative to the repo root.

set -euo pipefail

CURRENT_SCRIPT_FOLDER=$(dirname "$(realpath "$0")")

"$CURRENT_SCRIPT_FOLDER/runCuis.sh" \
    -s .ContinuousIntegrationScripts/exportTonel.st

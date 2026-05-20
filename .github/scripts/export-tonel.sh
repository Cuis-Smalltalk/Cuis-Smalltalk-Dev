#!/bin/bash
# export-tonel.sh — Export all loaded packages as Tonel .st files.
#
# Requires CUIS_VM_PATH and CUIS_VM_ARGUMENTS to be set (done by install-vm.sh).
# Output lands in tonel-export/ relative to the repo root.

set -euo pipefail

CURRENT_SCRIPT_FOLDER=$(dirname "$(realpath "$0")")

"$CURRENT_SCRIPT_FOLDER/run-cuis.sh" \
    -s .github/scripts/export-tonel.st

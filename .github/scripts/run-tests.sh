#!/usr/bin/env bash
set -euo pipefail

IMAGE_DIR="${1:-CuisImage}"
TEST_SCRIPT="${2:-.github/scripts/run-tests.st}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

CUIS_VM_BIN="${CUIS_VM:-${CUIS_VM_PATH:-}}"
CUIS_VM_OPTIONS="${CUIS_VM_ARGS:-${CUIS_VM_ARGUMENTS:-}}"

if [[ -z "$CUIS_VM_BIN" ]]; then
  echo "ERROR: set CUIS_VM or CUIS_VM_PATH before running tests" >&2
  exit 1
fi

IMAGE_FILE="$(bash "$SCRIPT_DIR/find-cuis-image.sh" "$REPO_ROOT/$IMAGE_DIR")"

VM_ARGS_ARRAY=()
if [[ -n "$CUIS_VM_OPTIONS" ]]; then
  read -r -a VM_ARGS_ARRAY <<< "$CUIS_VM_OPTIONS"
  "$CUIS_VM_BIN" "${VM_ARGS_ARRAY[@]}" "$REPO_ROOT/$IMAGE_DIR/$IMAGE_FILE" -s "$REPO_ROOT/$TEST_SCRIPT"
else
  "$CUIS_VM_BIN" "$REPO_ROOT/$IMAGE_DIR/$IMAGE_FILE" -s "$REPO_ROOT/$TEST_SCRIPT"
fi

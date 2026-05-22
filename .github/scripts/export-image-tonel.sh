#!/usr/bin/env bash
set -euo pipefail

IMAGE_DIR="${1:-CuisImage}"
OUTPUT_DIR="${2:-Packages/TonelImage}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

CUIS_VM_BIN="${CUIS_VM:-${CUIS_VM_PATH:-}}"
CUIS_VM_OPTIONS="${CUIS_VM_ARGS:--headless}"

if [[ -z "$CUIS_VM_BIN" ]]; then
  echo "ERROR: set CUIS_VM or CUIS_VM_PATH before exporting Tonel source" >&2
  exit 1
fi

IMAGE_FILE="$(bash "$SCRIPT_DIR/find-cuis-image.sh" "$REPO_ROOT/$IMAGE_DIR")"
TONEL_SETUP="
Utilities classPool at: #AuthorName put: 'TonelExporter'.
Utilities classPool at: #AuthorInitials put: 'TE'.
CodePackageFile installPackage: '$REPO_ROOT/Packages/Features/Tonel.pck.st' asFileEntry.
Smalltalk at: #TonelExportRepoRootDir put: '$REPO_ROOT'.
Smalltalk at: #TonelExportOutputDir put: '$OUTPUT_DIR'.
"

VM_ARGS_ARRAY=()
if [[ -n "$CUIS_VM_OPTIONS" ]]; then
  read -r -a VM_ARGS_ARRAY <<< "$CUIS_VM_OPTIONS"
  "$CUIS_VM_BIN" "${VM_ARGS_ARRAY[@]}" "$REPO_ROOT/$IMAGE_DIR/$IMAGE_FILE" -d "$TONEL_SETUP" -s "$REPO_ROOT/.github/scripts/export-image-tonel.st"
else
  "$CUIS_VM_BIN" "$REPO_ROOT/$IMAGE_DIR/$IMAGE_FILE" -d "$TONEL_SETUP" -s "$REPO_ROOT/.github/scripts/export-image-tonel.st"
fi


#!/bin/bash
# eject-package.sh — Export a package as Tonel sources and (optionally) unload it from the image.
#
# Behavior is test-gated: if tests fail, nothing is exported and the image is not modified.
#
# Usage:
#   .github/scripts/eject-package.sh <PackageName> [--force]
#
# Output:
#   src/<PackageName>/... (Tonel format)

set -euo pipefail

CURRENT_SCRIPT_FOLDER=$(dirname "$(realpath "$0")")

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <PackageName> [--force]" >&2
  exit 2
fi

PACKAGE_NAME="$1"
MODE="safe"
if [[ ${2:-} == "--force" ]]; then
  MODE="force"
fi

# 1) Tests first. Abort early on failure.
"$CURRENT_SCRIPT_FOLDER/run-tests.sh"

# 2) Export (and maybe unload) only if tests passed.
"$CURRENT_SCRIPT_FOLDER/run-cuis.sh" \
  -s .github/scripts/eject-package.st \
  -- "${PACKAGE_NAME}" "${MODE}"

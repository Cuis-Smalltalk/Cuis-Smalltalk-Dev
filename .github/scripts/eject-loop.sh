#!/bin/bash

# eject-loop.sh — Repeatedly eject packages from the image.
#
# Contract:
# - For each package:
#   1) Run tests (handled by eject-package.sh)
#   2) Eject package to Tonel + unload + snapshot image
#   3) Smoke-boot the image
#   4) Commit: Tonel sources + updated image
# - Revert (r): if smoke-boot fails, restore the repo to the pre-eject state.
#
# Usage:
#   .github/scripts/eject-loop.sh <PackageName> [<PackageName> ...]
#   .github/scripts/eject-loop.sh --force <PackageName> [<PackageName> ...]

set -euo pipefail

CURRENT_SCRIPT_FOLDER=$(dirname "$(realpath "$0")")

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 [--force] <PackageName> [<PackageName> ...]" >&2
  exit 2
fi

MODE="safe"
if [[ ${1:-} == "--force" ]]; then
  MODE="force"
  shift
fi

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 [--force] <PackageName> [<PackageName> ...]" >&2
  exit 2
fi

if [[ -n "$(git status --porcelain=v1)" ]]; then
  echo "ERROR: worktree is dirty. Commit/stash before running eject-loop." >&2
  exit 3
fi

for PACKAGE_NAME in "$@"; do
  echo "==> EJECT ${PACKAGE_NAME} (${MODE})"

  PRE_SHA=$(git rev-parse HEAD)

  EJECT_ARGS=()
  if [[ "$MODE" == "force" ]]; then
    EJECT_ARGS+=("--force")
  fi

  # Eject is test-gated internally.
  if ! "$CURRENT_SCRIPT_FOLDER/eject-package.sh" "${PACKAGE_NAME}" "${EJECT_ARGS[@]}"; then
    echo "ERROR: eject-package failed for ${PACKAGE_NAME}. Stopping." >&2
    exit 4
  fi

  # Smoke-boot check: ensure the image can start and run a trivial script.
  if ! "$CURRENT_SCRIPT_FOLDER/run-cuis.sh" -s .github/scripts/smoke-boot.st; then
    echo "ERROR: smoke-boot failed after ejecting ${PACKAGE_NAME}. Reverting (r)." >&2
    git restore --source "$PRE_SHA" --staged --worktree .
    exit 5
  fi

  # Commit Tonel + image.
  git add "src/${PACKAGE_NAME}" CuisImage/
  git commit -m "eject: move ${PACKAGE_NAME} out of image"
done

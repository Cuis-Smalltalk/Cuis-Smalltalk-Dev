#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

ARCHES=("$@")
if [[ "${#ARCHES[@]}" -eq 0 ]]; then
  ARCHES=(amd64 arm64)
fi

for ARCH in "${ARCHES[@]}"; do
  case "$ARCH" in
    amd64|arm64)
      ;;
    *)
      echo "ERROR: unsupported arch '$ARCH'. Use amd64 or arm64." >&2
      exit 1
      ;;
  esac

  docker buildx build \
    --platform "linux/${ARCH}" \
    --file "$REPO_ROOT/docker/Dockerfile" \
    --tag "cuis-headless:${ARCH}" \
    --load \
    "$REPO_ROOT"
done

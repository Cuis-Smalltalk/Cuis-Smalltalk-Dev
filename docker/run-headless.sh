#!/usr/bin/env bash
set -euo pipefail

CUIS_VM_BIN="${CUIS_VM:-}"
IMAGE_DIR="${CUIS_IMAGE_DIR:-/workspace/CuisImage}"
CUIS_IMAGE_PATH="${CUIS_IMAGE:-}"

if [[ -z "$CUIS_VM_BIN" ]]; then
  echo "ERROR: CUIS_VM is not set" >&2
  exit 1
fi

if [[ -n "$CUIS_IMAGE_PATH" ]]; then
  IMAGE_FILE="$CUIS_IMAGE_PATH"
else
  if [[ ! -d "$IMAGE_DIR" ]]; then
    echo "ERROR: image directory not found: $IMAGE_DIR" >&2
    exit 1
  fi

  IFS=$'\n' read -r -d '' -a IMAGE_FILES < <(find "$IMAGE_DIR" -maxdepth 1 -type f -name 'Cuis*.*-*.image' -print | sort && printf '\0')

  if [[ "${#IMAGE_FILES[@]}" -eq 0 ]]; then
    echo "ERROR: no .image found in $IMAGE_DIR/" >&2
    exit 1
  fi

  if [[ "${#IMAGE_FILES[@]}" -gt 1 ]]; then
    echo "ERROR: multiple .image files found in $IMAGE_DIR/:" >&2
    printf '  %s\n' "${IMAGE_FILES[@]}" >&2
    exit 1
  fi

  IMAGE_FILE="${IMAGE_FILES[0]}"
fi

VM_ARGS=(-headless)
if [[ -n "${CUIS_VM_ARGS:-}" ]]; then
  read -r -a EXTRA_VM_ARGS <<< "${CUIS_VM_ARGS}"
  VM_ARGS+=("${EXTRA_VM_ARGS[@]}")
fi

exec "$CUIS_VM_BIN" "${VM_ARGS[@]}" "$IMAGE_FILE" "$@"

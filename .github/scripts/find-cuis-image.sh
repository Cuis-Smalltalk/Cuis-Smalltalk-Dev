#!/usr/bin/env bash
set -euo pipefail

IMAGE_DIR="${1:-CuisImage}"

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

basename "${IMAGE_FILES[0]}"

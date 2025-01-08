#!/bin/bash
set -euo pipefail

IMAGE_FILE="$(ls CuisImage/ | grep 'Cuis7.3-[0-9]\+.image')"

"$CUIS_VM_PATH" "$CUIS_VM_ARGUMENTS" CuisImage/"$IMAGE_FILE" "$@"
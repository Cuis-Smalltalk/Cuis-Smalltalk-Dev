#!/bin/bash
set -euo pipefail

# Support two naming conventions:
#   - CUIS_VM_PATH / CUIS_VM_ARGUMENTS  (set by install-vm.sh on bare-metal runners)
#   - CUIS_VM / CUIS_VM_ARGS            (set by ENV in Dockerfile.vm inside Docker)
VM="${CUIS_VM_PATH:-${CUIS_VM:-}}"
VM_ARGS="${CUIS_VM_ARGUMENTS:-${CUIS_VM_ARGS:-}}"

if [[ -z "$VM" ]]; then
  echo "ERROR: neither CUIS_VM_PATH nor CUIS_VM is set" >&2
  exit 1
fi

IMAGE_FILE="$(ls CuisImage/ | grep -E 'Cuis[0-9]+\.[0-9]+-[0-9]+\.image' | head -1)"

# $VM_ARGS must not be quoted so each flag is passed as a separate argument
"$VM" $VM_ARGS CuisImage/"$IMAGE_FILE" "$@"
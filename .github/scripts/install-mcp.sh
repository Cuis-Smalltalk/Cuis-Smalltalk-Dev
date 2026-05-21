#!/usr/bin/env bash
set -euo pipefail

IMAGE_DIR="${1:-CuisImage}"
MCP_SOURCE="${2:-/Users/gaston/Code/cuis-mcp/McpServer.pck.st}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

CUIS_VM_BIN="${CUIS_VM:-${CUIS_VM_PATH:-}}"
CUIS_VM_OPTIONS="${CUIS_VM_ARGS:-${CUIS_VM_ARGUMENTS:-}}"
if [[ "$MCP_SOURCE" = /* ]]; then
  MCP_INPUT="$MCP_SOURCE"
else
  MCP_INPUT="$REPO_ROOT/$MCP_SOURCE"
fi

if [[ -d "$MCP_INPUT" ]]; then
  MCP_PACKAGE="$MCP_INPUT/McpServer.pck.st"
else
  MCP_PACKAGE="$MCP_INPUT"
fi

if [[ -z "$CUIS_VM_BIN" ]]; then
  echo "ERROR: set CUIS_VM or CUIS_VM_PATH before installing MCP" >&2
  exit 1
fi

if [[ ! -f "$MCP_PACKAGE" ]]; then
  echo "ERROR: MCP package not found: $MCP_PACKAGE" >&2
  exit 1
fi

IMAGE_FILE="$(bash "$SCRIPT_DIR/find-cuis-image.sh" "$REPO_ROOT/$IMAGE_DIR")"

INSTALL_MCP_SCRIPT="
[
  Utilities classPool at: #AuthorName put: 'GitHubActions'.
  Utilities classPool at: #AuthorInitials put: 'GHA'.

  StdIOWriteStream stdout nextPutAll: 'loading MCP dependencies'; newLine; flush.
  Feature require: 'Network-Kernel'.
  Feature require: 'JSON'.

  StdIOWriteStream stdout nextPutAll: 'installing MCP package from $MCP_PACKAGE'; newLine; flush.
  CodePackageFile installPackage: '$MCP_PACKAGE' asFileEntry.

  StdIOWriteStream stdout nextPutAll: 'saving image with MCP installed'; newLine; flush.
  Smalltalk snapshot: true andQuit: true embedded: false clearAllClassState: false.
] on: Error do: [ :ex |
  StdIOWriteStream stdout nextPutAll: 'ERROR: '; nextPutAll: ex printString; newLine; flush.
  StdIOWriteStream stdout nextPutAll: 'SIGNALER: '; nextPutAll: ex signalerContext printString; newLine; flush.
  Smalltalk quitPrimitive: 1
].
"

VM_ARGS_ARRAY=()
if [[ -n "$CUIS_VM_OPTIONS" ]]; then
  read -r -a VM_ARGS_ARRAY <<< "$CUIS_VM_OPTIONS"
  "$CUIS_VM_BIN" "${VM_ARGS_ARRAY[@]}" "$REPO_ROOT/$IMAGE_DIR/$IMAGE_FILE" -d "$INSTALL_MCP_SCRIPT"
else
  "$CUIS_VM_BIN" "$REPO_ROOT/$IMAGE_DIR/$IMAGE_FILE" -d "$INSTALL_MCP_SCRIPT"
fi

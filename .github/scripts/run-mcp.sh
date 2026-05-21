#!/usr/bin/env bash
set -euo pipefail

IMAGE_DIR="${1:-CuisImage}"
PORT="${2:-${CUIS_MCP_PORT:-1470}}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

CUIS_VM_BIN="${CUIS_VM:-${CUIS_VM_PATH:-}}"
CUIS_VM_OPTIONS="${CUIS_VM_ARGS:--headless}"

if [[ -z "$CUIS_VM_BIN" ]]; then
  echo "ERROR: set CUIS_VM or CUIS_VM_PATH before starting MCP" >&2
  exit 1
fi

IMAGE_FILE="$(bash "$SCRIPT_DIR/find-cuis-image.sh" "$REPO_ROOT/$IMAGE_DIR")"

START_MCP_SCRIPT="
[ | serverClass |
  StdIOWriteStream stdout nextPutAll: 'starting cuis-mcp'; newLine; flush.
  serverClass := Smalltalk classNamed: #McpServer.
  serverClass ifNil: [ Error signal: 'McpServer is not installed in the image' ].

  (serverClass class includesSelector: #startOn:) ifFalse: [
    serverClass class compile: 'startOn: aPort
  socket ifNotNil: [ socket stop ].
  current := self new.
  self refreshTools.
  socket := McpSocket newForServer: current.
  socket startOn: aPort.
  ^ current' classified: 'instance creation'.
  ].

  serverClass stop.
  serverClass startOn: $PORT.
  StdIOWriteStream stdout nextPutAll: 'cuis-mcp listening on localhost:$PORT'; newLine; flush.
] on: Error do: [ :ex |
  StdIOWriteStream stdout nextPutAll: 'ERROR: '; nextPutAll: ex printString; newLine; flush.
  StdIOWriteStream stdout nextPutAll: 'SIGNALER: '; nextPutAll: ex signalerContext printString; newLine; flush.
  Smalltalk quitPrimitive: 1
].
"

VM_ARGS_ARRAY=()
if [[ -n "$CUIS_VM_OPTIONS" ]]; then
  read -r -a VM_ARGS_ARRAY <<< "$CUIS_VM_OPTIONS"
  exec "$CUIS_VM_BIN" "${VM_ARGS_ARRAY[@]}" "$REPO_ROOT/$IMAGE_DIR/$IMAGE_FILE" -d "$START_MCP_SCRIPT"
else
  exec "$CUIS_VM_BIN" "$REPO_ROOT/$IMAGE_DIR/$IMAGE_FILE" -d "$START_MCP_SCRIPT"
fi

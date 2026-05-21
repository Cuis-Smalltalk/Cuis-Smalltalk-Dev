#!/usr/bin/env bash
set -euo pipefail

HOST="${CUIS_MCP_HOST:-127.0.0.1}"
PORT="${CUIS_MCP_PORT:-1470}"

python3 - "$HOST" "$PORT" <<'PY'
import select
import socket
import sys

host = sys.argv[1]
port = int(sys.argv[2])

try:
    sock = socket.create_connection((host, port))
except OSError as error:
    sys.stderr.write(
        f"ERROR: could not connect to cuis-mcp at {host}:{port}: {error}\n"
    )
    sys.stderr.write(
        "Start the Smalltalk side first with .github/scripts/install-mcp.sh and .github/scripts/run-mcp.sh.\n"
    )
    sys.exit(1)

sock.setblocking(False)
stdin = sys.stdin.buffer
stdout = sys.stdout.buffer

while True:
    readable, _, _ = select.select([sock, stdin], [], [])

    if sock in readable:
        data = sock.recv(65536)
        if not data:
            break
        stdout.write(data)
        stdout.flush()

    if stdin in readable:
        data = stdin.readline()
        if not data:
            try:
                sock.shutdown(socket.SHUT_WR)
            except OSError:
                pass
            continue
        sock.sendall(data)

sock.close()
PY

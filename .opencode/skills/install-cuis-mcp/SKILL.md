---
name: install-cuis-mcp
description: Use when the user says install/start MCP, McpServer.pck.st, `McpServer startDefault`, or wants opencode connected to a local Cuis MCP server, including alternate ports.
---

# Install Cuis MCP

Use this skill to leave a Cuis image ready with `McpServer` installed and a local opencode MCP entry pointing at it.

## Trigger

Use when the task mentions any of these:

- `McpServer.pck.st`
- `McpServer startDefault`
- install MCP in Cuis
- start Cuis MCP
- run several MCP servers on different ports

## Default package path

Prefer this package unless the user gives another one:

`/Users/gaston/Code/cuis-mcp/McpServer.pck.st`

## Standard install/start sequence

1. Install the package into the target image:

```sh
CUIS_VM="./CuisVM.app/Contents/MacOS/Squeak" \
  bash .github/scripts/install-mcp.sh CuisImage /Users/gaston/Code/cuis-mcp/McpServer.pck.st
```

2. Start the Smalltalk MCP server:

```sh
CUIS_VM="./CuisVM.app/Contents/MacOS/Squeak" \
  CUIS_VM_ARGS="-headless" \
  CUIS_MCP_PORT=1470 \
  bash .github/scripts/run-mcp.sh CuisImage
```

This starts `McpServer` in the image. The shell script will use `McpServer startOn:` when available and will add that class-side method on the fly if the loaded package only has `startDefault`.

## Multiple servers

To run several MCP servers at once, change `CUIS_MCP_PORT` per process:

```sh
CUIS_MCP_PORT=1471 bash .github/scripts/run-mcp.sh CuisImage
CUIS_MCP_PORT=1472 bash .github/scripts/run-mcp.sh CuisImage
```

The project config already registers an opencode MCP server named `cuis-mcp` through `.opencode/scripts/cuis-mcp-bridge.sh`, with default port `1470`.

If another port is needed for opencode, update `.opencode/opencode.json` or export `CUIS_MCP_PORT` for the bridge process before restarting opencode.

## Notes

- The opencode MCP entry is only a stdio-to-TCP bridge. The Smalltalk side must be running first.
- `install-mcp.sh` accepts either a directory containing `McpServer.pck.st` or the full `.pck.st` path.
- After changing `.opencode/opencode.json` or any skill, restart opencode so it reloads config.

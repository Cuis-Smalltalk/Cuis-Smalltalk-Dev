---
name: cuis-core-mcp-rewrite
description: Use when the user wants to improve or simplify Cuis core through MCP, fix BaseImageTests, keep the good result versioned as text, export fileOuts, and commit only after tests pass.
---

# Cuis Core Rewrite Through MCP

Use this skill when the user wants an agent to drive live Cuis changes through MCP instead of editing only repository text files.

## Goal

Improve, simplify, or repair core Cuis behavior in the live image while keeping the final result preserved as text artifacts in this repo.

## Required workflow

1. Ensure the local Cuis MCP server is installed and running.
   - Use the `install-cuis-mcp` skill first when needed.
2. Do a Tidy First check in the target area.
   - If structural cleanup is needed, do it first and keep it separate from behavior changes.
3. For behavior changes, work in TDD order.
   - Prefer `Packages/BaseImageTests.pck.st` as the first test catalog.
   - Make the next failing test fail for the right reason before changing behavior.
4. Use MCP to inspect and change live methods.
   - Read existing source before compiling replacements.
   - Keep each step small enough that a failing test identifies one thing.
5. Re-run the relevant tests after each behavioral step.
6. Export the result as text.
   - Core/image changes: export a new `CoreUpdates/*.cs.st`.
   - Package-scoped changes: save/export the corresponding `.pck.st`.
   - If the user wants the broader image state preserved, also serialize installed packages into `Packages/SerializedImage/InstalledPackages/`.
7. Commit only after:
   - tests are green
   - the text artifact exists in the repo
   - the artifact is the intended reload/replay source

## Main test target

Primary path:

`/Users/gaston/Code/Cuis-Smalltalk-Dev/Packages/BaseImageTests.pck.st`

Use narrower test scopes when possible, but treat `BaseImageTests` as the default safety net for core work.

## Export policy

Do not leave the result only in:

- the live image memory
- `CuisImage/*.changes`
- `UserChanges/*.user.changes`

The finished result must be recoverable from versioned text in the repo.

## Expected tools and files

- MCP server tools for reading and compiling methods
- `.github/scripts/run-tests.sh` or the repo test runner workflow
- `CoreUpdates/*.cs.st`
- `Packages/*.pck.st`
- `Packages/SerializedImage/InstalledPackages/*.pck.st` when preserving a richer image state

## Reporting back

Always report:

1. what area of core Cuis changed
2. what tests were run
3. what text artifact was exported
4. whether the image state was additionally serialized
5. the final commit hash when a commit was created

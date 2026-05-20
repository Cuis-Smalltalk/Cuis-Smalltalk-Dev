# Contributing

## Before you start

Read [AGENTS.md](AGENTS.md) if you plan to use an AI assistant. The methodology here is **Tidy First + TDD**: structural cleanup and behavioral changes go in separate commits, always.

## License

All contributions must be under the [MIT License](LICENSE). By contributing, you agree to the [Developer Certificate of Origin (DCO)](DCO) — you have the legal right to make the contribution.

## Workflow

1. Fork the repo and create a branch from `master`
2. Write a test first if adding or fixing behavior
3. Keep commits small and focused — one concern per commit
4. Never mix cleanup with behavior changes in the same commit
5. Open a pull request with a clear description of what and why

## Changesets vs packages

- **Core image changes**: use the ChangeSorter and ChangeList tools inside Cuis to prepare a `.cs.st` changeset. Attach it to a message on the [mailing list](https://lists.cuis.st/mailman/listinfo/cuis-dev) for review.
- **Package changes**: pull requests are fine for changes to existing packages or new packages in `Packages/`.

## Code style

Follow the conventions already in the codebase. Cuis Smalltalk values clarity and minimalism — if it doesn't need to be there, remove it.

## Upstream

This repo is a fork of [Cuis-Smalltalk/Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev). Changes that belong upstream should be contributed there directly.

## Questions

Open an issue or read [Getting Help with Cuis](Documentation/GettingHelpWithCuis.md).

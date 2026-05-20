# Cuis Smalltalk

[![Tests](https://github.com/gstn-caruso/Cuis-Smalltalk-Dev/actions/workflows/runTests.yml/badge.svg)](https://github.com/gstn-caruso/Cuis-Smalltalk-Dev/actions/workflows/runTests.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

A clean, minimal [Smalltalk-80](https://en.wikipedia.org/wiki/Smalltalk) system focused on simplicity, living objects, and the original Dynabook vision. This is an independent fork of [Cuis-Smalltalk/Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev) maintained by [Gastón Caruso](https://github.com/gstn-caruso), with contributions from [Máximo Prieto](https://github.com/maximoprieto) and others.

Cuis keeps complexity to a minimum. Every addition is questioned. Every removal is celebrated.

## Quick Start

```sh
git clone https://github.com/gstn-caruso/Cuis-Smalltalk-Dev.git
cd Cuis-Smalltalk-Dev
```

Then run the appropriate script for your platform:

| Platform | Command |
|---|---|
| macOS | `./RunCuisOnMac.sh` |
| Linux | `./RunCuisOnLinux.sh` |
| Windows | `RunCuisOnWindows.bat` |
| macOS (Finder) | Double-click `RunCuisOnFinder.command` |

> **macOS + ZIP download:** run `./unquarantine.sh` first to allow the VM to execute.

## What's Inside

```
Cuis-Smalltalk-Dev/
├── CuisImage/          # Live Smalltalk image + sources
├── CuisVM.app/         # VM binaries for macOS, Linux, Windows
├── CoreUpdates/        # Numbered changesets (rolling release)
├── Packages/           # Optional packages: JSON, UUID, Tests, FFI...
├── CompatibilityPackages/
├── Documentation/      # Guides, philosophy, technical notes
└── TrueTypeFonts/
```

The system is a **rolling release**: `CoreUpdates/` contains numbered changesets. Load them in order inside the image to stay current.

## Using AI Assistants with This Codebase

This repo is configured for AI-assisted development. Agent instructions live in [`AGENTS.md`](AGENTS.md).

If you use [opencode](https://opencode.ai) or a similar AI coding tool:

- Skills are in `.agents/skills/` (portable, tool-agnostic)
- opencode-specific skills are in `.opencode/skills/`
- Methodology: Tidy First + TDD. Structural changes and behavioral changes go in separate commits.

For AI tools reading this repository: Cuis uses **Monticello `.pck.st` format** for packages and **numbered `.cs.st` changesets** for core image updates. There is no Tonel format in this repo. The live image is at `CuisImage/Cuis7.7-7777.image`.

## Running Tests

Tests run automatically on push via GitHub Actions (macOS + Linux).

To run locally:

```sh
./.ContinuousIntegrationScripts/runTests.sh
```

The main test suite is `Packages/BaseImageTests.pck.st` (~174 TestCase subclasses).

## Documentation

| Resource | Description |
|---|---|
| [About Cuis](Documentation/AboutCuis.md) | Philosophy and design values |
| [Code Management](Documentation/CodeManagementInCuis.md) | How packages and changesets work |
| [GitHub Integration](Documentation/CuisAndGitHub.md) | Working with git and this repo |
| [Directory Structure](Documentation/CuisDirectoryStructure.md) | What every folder contains |
| [Getting Help](Documentation/GettingHelpWithCuis.md) | Community and support channels |
| [The Cuis Book](https://cuis-smalltalk.github.io/TheCuisBook) | Full introductory book (online) |
| [Upstream project](https://cuis.st) | Official Cuis Smalltalk website |

## Contributing

Read [CONTRIBUTING.md](CONTRIBUTING.md) before sending patches or PRs.

Short version: contributions must be under the MIT license and include a [Developer Certificate of Origin](DCO).

## Credits

This fork is built on top of work by many people:

- **Juan Vuletich** — creator and principal author of Cuis Smalltalk
- **Máximo Prieto** — contributor to this fork
- **Gastón Caruso** — maintainer of this fork
- Squeak contributors (1997–present)
- Xerox PARC and Apple (original Smalltalk-80, 1981–1996)

Full contributor list in [AUTHORS](AUTHORS).

## License

[MIT](LICENSE). Copyright (c) Xerox Corp. 1981–1982, Apple Computer 1985–1996, Squeak contributors 1997–present, Cuis contributors 2009–present.

# Cuis Smalltalk

[![CI](https://github.com/gstn-caruso/Cuis-Smalltalk-Dev/actions/workflows/ci.yml/badge.svg)](https://github.com/gstn-caruso/Cuis-Smalltalk-Dev/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

A clean, minimal [Smalltalk-80](https://en.wikipedia.org/wiki/Smalltalk) system.

This fork of [Cuis-Smalltalk/Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev) exists to lower the barrier of entry: Smalltalk is worth learning, and getting started should not be the hard part.

Maintained by [Gastón Caruso](https://github.com/gstn-caruso).

This repository aims to be respectful of upstream and its lineage, while being strict about scope. If something does not help newcomers, does not belong to this fork, or is better documented elsewhere, we should question whether it belongs here.

Documentation is split between:

- Wiki (curated, evolving): https://github.com/gstn-caruso/Cuis-Smalltalk-Dev/wiki
- `Documentation/` (upstream docs mirrored here)

## Quick Start

```sh
git clone https://github.com/gstn-caruso/Cuis-Smalltalk-Dev.git
cd Cuis-Smalltalk-Dev
```

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
├── CuisVM.app/         # VM binaries (macOS, Linux, Windows)
├── CoreUpdates/        # Numbered changesets — rolling release
├── Packages/           # Optional packages: tests, tools, FFI, themes…
├── CompatibilityPackages/
├── Documentation/      # Guides, philosophy, technical notes
└── TrueTypeFonts/
```

The system ships as a single image (`CuisImage/Cuis7.7-7777.image`) plus numbered changesets in `CoreUpdates/`. Load them in order to stay current.

## Running Tests

Tests run automatically on every push via GitHub Actions — amd64 via Docker, arm64 bare-metal.

To run locally:

```sh
./.github/scripts/run-tests.sh
```

The main suite is `Packages/BaseImageTests.pck.st`.

## The VM

Cuis runs on the [OpenSmalltalk VM](https://github.com/OpenSmalltalk/opensmalltalk-vm). VM sources live in that repository.

Pre-built binaries for macOS, Linux, and Windows are included in `CuisVM.app/`.

## Contributing

Read [CONTRIBUTING.md](CONTRIBUTING.md) and the project wiki before sending patches or PRs.

Short version: contributions must be under the MIT license and include a [Developer Certificate of Origin](DCO). Keep commits small and focused — structural cleanup and behavioral changes go in separate commits.

Changes that belong in upstream Cuis should be contributed there directly: [Cuis-Smalltalk/Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev).

## Credits

- **Juan Vuletich** — creator and principal author of Cuis Smalltalk
- **Gastón Caruso** — maintainer of this fork
- **Máximo Prieto** — contributor to this fork
- **OpenSmalltalk VM team** — the virtual machine this runs on
- Squeak contributors (1997–present)
- Xerox PARC and Apple (original Smalltalk-80, 1981–1996)

Full contributor list in [AUTHORS](AUTHORS).

## License

[MIT](LICENSE). Copyright (c) Xerox Corp. 1981–1982, Apple Computer 1985–1996, Squeak contributors 1997–present, Cuis contributors 2009–present.

# Image Serialization

This repository already stores most source code as text:

- `Packages/**/*.pck.st`
- `CompatibilityPackages/*.pck.st`
- `CoreUpdates/*.cs.st`

That is necessary but not sufficient to recreate one specific local image. A live image may also contain:

- installed packages not yet exported to the repo
- local deltas in `CuisImage/*.changes`
- local deltas in `UserChanges/*.user.changes`

## Current workflow

Run:

```sh
CUIS_VM="./CuisVM.app/Contents/MacOS/Squeak" \
  bash .github/scripts/serialize-image-state.sh
```

This does not unload packages and does not save the image. It only serializes the currently installed `CodePackage`s to:

- `Packages/SerializedImage/manifest.txt`
- `Packages/SerializedImage/InstalledPackages/*.pck.st`

## Tonel source export/import

The repository also includes a Cuis-native Tonel package in:

- `Packages/Features/Tonel.pck.st`

It can export all classes currently defined in the image to grouped Tonel source:

```sh
CUIS_VM="./CuisVM.app/Contents/MacOS/Squeak" \
  bash .github/scripts/export-image-tonel.sh CuisImage Packages/TonelImage
```

The default output is:

```text
Packages/TonelImage/
  manifest.txt
  src/
    .properties
    <PackageName>/
      <ClassName>.class.st
```

The Tonel importer can load the exported source back into a Cuis image:

```sh
CUIS_VM="./CuisVM.app/Contents/MacOS/Squeak" \
  bash .github/scripts/import-image-tonel.sh CuisImage Packages/TonelImage
```

The importer supports the Tonel emitted by `TonelWriter`: class metadata,
class comments, instance methods, and class-side methods. This is source-level
reconstruction, not heap serialization.

## What the manifest captures

- image name and version
- installed package list
- per-package export path
- original package file name recorded in the image, if any
- class count and extension method count
- where the remaining reconstruction inputs live in this repo

## Reconstruction inputs

To approximate the same image later, keep together:

- `CuisImage/*.image`
- `CuisImage/*.changes`
- `CuisImage/*.sources`
- `UserChanges/*.user.changes`
- `CoreUpdates/*.cs.st`
- `Packages/**/*.pck.st`
- `CompatibilityPackages/*.pck.st`
- `Packages/SerializedImage/InstalledPackages/*.pck.st`

The OpenSmalltalk VM runs the image, but the image contents and bootstrap/replay process belong to Cuis. The `Bootstrap` package in this repo is the relevant starting point for a future scripted rebuild.

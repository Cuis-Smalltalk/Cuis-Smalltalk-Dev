# Tonel Image Exporter Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a Cuis-native Tonel package that exports and imports Tonel for the code definitions in the current image, then serialize the whole image into a grouped Tonel folder.

**Architecture:** Add a `Tonel` package based on the Pharo Tonel v3 text format, adapted to Cuis APIs and stored as `.pck.st`. The package includes a writer, reader/importer, image exporter, SUnit tests, and headless export scripts.

**Tech Stack:** Cuis Smalltalk `.pck.st` packages, SUnit, OpenSmalltalk VM headless scripts, Tonel v3 text format.

---

### Task 1: Tonel Writer And Reader Core

**Files:**
- Create: `Packages/Features/Tonel.pck.st`
- Create: `Packages/Features/Tests-Tonel.pck.st`
- Create: `.github/scripts/run-tonel-tests.st`

- [ ] **Step 1: Write failing tests**

Create `TonelWriterTest` with tests for:
- class definition metadata contains `Class {`, `#name`, `#superclass`, `#instVars`, `#classVars`, `#classInstVars`, `#category`, and `#package`
- instance method output includes `{ #category : 'accessing' }`, `ClassName >> selector [`, and source body
- class method output uses `ClassName class >> selector [`
- comments are quoted before the class definition
- reading the writer output returns class metadata and method definitions

- [ ] **Step 2: Verify red**

Run:

```bash
./CuisVM.app/Contents/MacOS/Squeak -headless CuisImage/Cuis7.7-7777.image -s .github/scripts/run-tonel-tests.st
```

Expected: failure because `TonelWriter` and `TonelReader` are not implemented.

- [ ] **Step 3: Implement minimal writer and reader**

Implement `TonelWriter` with public API:

```smalltalk
TonelWriter class>>sourceForClass: aClass
TonelWriter class>>writeClass: aClass on: aStream
```

Implement `TonelReader` with public API:

```smalltalk
TonelReader class>>readStream: aStream
TonelClassDefinition>>className
TonelClassDefinition>>superclassName
TonelClassDefinition>>category
TonelClassDefinition>>methods
```

The writer emits Tonel v3:

```smalltalk
"
comment
"
Class {
	#name : 'Example',
	#superclass : 'Object',
	#instVars : [
		'ivar'
	],
	#category : 'Example',
	#package : 'Example'
}
```

- [ ] **Step 4: Verify green**

Run the same test command. Expected: all `TonelWriterTest` tests pass.

### Task 2: Importer

**Files:**
- Modify: `Packages/Features/Tonel.pck.st`
- Modify: `Packages/Features/Tests-Tonel.pck.st`

- [ ] **Step 1: Write failing tests**

Create `TonelImporterTest` with tests for:
- importing a Tonel class creates the class in Cuis
- importing instance methods compiles their source
- importing class methods compiles on the metaclass
- reimport updates an existing method

- [ ] **Step 2: Verify red**

Run `run-tonel-tests.st`. Expected: failure because `TonelImporter` is missing.

- [ ] **Step 3: Implement importer**

Implement `TonelImporter` with public API:

```smalltalk
TonelImporter class>>importStream: aStream
TonelImporter class>>importFile: aFileEntry
TonelImporter class>>importDirectory: aDirectoryEntry
```

The importer creates or updates classes and compiles method definitions. The first version supports the Tonel emitted by `TonelWriter`.

- [ ] **Step 4: Verify green**

Run `run-tonel-tests.st`. Expected: all writer, reader, and importer tests pass.

### Task 3: Image Exporter

**Files:**
- Modify: `Packages/Features/Tonel.pck.st`
- Modify: `Packages/Features/Tests-Tonel.pck.st`

- [ ] **Step 1: Write failing tests**

Create `TonelImageExporterTest` with tests for:
- exporting a selected class creates `<root>/<package>/<ClassName>.class.st`
- `packageNameForCategory:` groups `Kernel-Objects` under `Kernel`
- `fileNameForClass:` produces stable ASCII-safe filenames

- [ ] **Step 2: Verify red**

Run `run-tonel-tests.st`. Expected: failure because `TonelImageExporter` is missing.

- [ ] **Step 3: Implement exporter**

Implement `TonelImageExporter` with public API:

```smalltalk
TonelImageExporter class>>exportAllTo: aDirectoryEntry
TonelImageExporter class>>exportClasses: classes to: aDirectoryEntry
TonelImageExporter class>>packageNameForCategory: aCategoryName
TonelImageExporter class>>fileNameForClass: aClass
```

`exportAllTo:` enumerates `Smalltalk allClasses`, sorts by class name, creates package directories, and writes one `.class.st` file per class.

- [ ] **Step 4: Verify green**

Run `run-tonel-tests.st`. Expected: all Tonel tests pass.

### Task 4: Whole-Image Export Script

**Files:**
- Create: `.github/scripts/export-image-tonel.st`
- Create: `.github/scripts/export-image-tonel.sh`
- Modify: `Documentation/ImageSerialization.md`

- [ ] **Step 1: Add script**

The shell script runs the image headless, installs `Packages/Features/Tonel.pck.st`, and executes the Smalltalk script.

- [ ] **Step 2: Export the image**

Run:

```bash
./.github/scripts/export-image-tonel.sh CuisImage Packages/TonelImage
```

Expected: `Packages/TonelImage/src/.properties`, grouped package directories, `.class.st` files, and `manifest.txt`.

- [ ] **Step 3: Verify coverage**

The manifest records:
- exported class count
- method count
- source package directories
- image name and version

Compare exported class count to `Smalltalk allClasses size`.

### Task 5: MCP Touchpoint

**Files:**
- No source changes required unless the existing bridge needs a small fix.

- [ ] **Step 1: Try MCP bridge**

Start or connect to the configured `cuis-mcp` bridge on `127.0.0.1:1470`.

- [ ] **Step 2: Record result**

If available, use MCP to evaluate a read-only image query. If unavailable from this session, record that the export path was verified headlessly instead.

### Task 6: Commit And Push

**Files:**
- Commit only files created or changed for this task.

- [ ] **Step 1: Run verification**

Run:

```bash
./.github/scripts/export-image-tonel.sh CuisImage Packages/TonelImage
./CuisVM.app/Contents/MacOS/Squeak -headless CuisImage/Cuis7.7-7777.image -s .github/scripts/run-tonel-tests.st
git status --short
```

- [ ] **Step 2: Commit**

Commit with a concise message, no AI attribution.

- [ ] **Step 3: Push**

Push the current branch.

# Contributing

Thanks for your interest in contributing to this fork of Cuis Smalltalk.

## Getting started

1. Fork the repo and create a branch from `master`.
2. Make your changes following the methodology below.
3. Open a pull request with a description in Spanish (see below).

## Methodology

We follow **Tidy First + TDD**:

- **Tidy First**: before touching behavior, check if the code needs structural cleanup. If it does, do it in a separate commit. If it doesn't, say so explicitly.
- **TDD**: Red → Green → Refactor. Write the test first, make it fail for the right reason, then implement the minimum to make it pass.
- Keep structural changes separate from behavioral changes. Never mix them in the same commit.

## Core changes vs packages

- Changes to **core Cuis** (kernel, morphic, tools): if the change belongs upstream, submit it to [Cuis-Smalltalk/Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev) as a changeset.
- Changes specific to this fork: open a PR here.
- **Packages**: contribute via PR with the updated `.pck.st` file (Monticello) and, if applicable, the corresponding Tonel `.st` files committed to git.

## Pull request convention

PR descriptions must be written in **Spanish**. This is public open source documentation — write as if the reader has no context from prior conversations.

Required structure:

```
## ¿Qué hace este PR?
## ¿Por qué lo queremos mergear?
## Cambios incluidos
## Qué NO incluye (si aplica)
## Verificación
```

PR titles: in Spanish, max 72 characters.

## Documentation

Extended documentation lives in the [project wiki](https://github.com/gstn-caruso/Cuis-Smalltalk-Dev/wiki).

## The VM

The VM used by Cuis is the OpenSmalltalk VM: https://github.com/OpenSmalltalk/opensmalltalk-vm

## DCO and license

By contributing, you certify that you have the right to submit your contribution under the project's [MIT License](LICENSE). This project uses the [Developer Certificate of Origin (DCO)](https://developercertificate.org/). Add a `Signed-off-by` line to your commits:

```
git commit -s -m "your message"
```

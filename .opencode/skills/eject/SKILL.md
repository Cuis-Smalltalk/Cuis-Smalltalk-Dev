---
name: eject
description: Use when the user says "eject" to extract a Cuis package from the image into versioned Tonel sources under src/, then unload it from the image (safe/force).
---

# Eject Package From Image

Goal: take one loaded Cuis package, export its source to the repo (Tonel format under `src/<PackageName>/`), then remove it from the image and save the image.

Hard rules:
- Run tests first. If tests fail: stop, do not export, do not unload, do not save the image.
- Default to safe mode: if there are live instances of any class in the package, do nothing and stop.
- Only use force mode after explicit user confirmation (it can break the image).

## Commands

Safe eject:

```bash
.github/scripts/eject-package.sh <PackageName>
```

Force eject (requires explicit confirmation):

```bash
.github/scripts/eject-package.sh <PackageName> --force
```

Notes:
- This runs `.github/scripts/run-tests.sh` first.
- If it proceeds, it exports with `TonelWriter` to `src/<PackageName>/` and then unloads the package and snapshots the image.

## After A Successful Eject

1. Inspect changes: `git status` and `git diff`.
2. Commit exported sources + updated image (if the repo policy is to version images):

```bash
git add src/<PackageName> CuisImage/
git commit -m "eject: move <PackageName> out of image"
git push
```

## Safety Prompt (Force Mode)

Before running `--force`, warn clearly:
- Live instances exist; unloading can corrupt behavior and crash tools.
- Other packages may still reference the ejected classes.
- Proceeding is expected to "explode" if the image still needs that package.

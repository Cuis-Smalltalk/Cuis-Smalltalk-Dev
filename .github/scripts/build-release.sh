#!/bin/bash
# build-release.sh — Package Cuis into minimal, platform-specific release ZIPs.
#
# Each ZIP contains ONLY what is needed to run Cuis on that platform:
#   CuisImage/<image>.image + .changes + .sources + UnicodeData.txt
#   CuisVM.app/Contents/<platform>/  (VM binary + plugins for that platform only)
#   TrueTypeFonts/
#   Run script(s) for that platform
#   LICENSE + README.md
#
# CoreUpdates/, Packages/, Documentation/ are NOT included.
# The image must already have all updates applied before calling this script.
#
# Usage:
#   build-release.sh [output-dir] [arch-filter]
#
#   output-dir   defaults to release-artifacts/
#   arch-filter  optional: "amd64" or "arm64"
#                when set, only packages for that Linux arch are produced
#                (macOS and Windows are included in amd64 builds)

set -euo pipefail

OUTPUT_DIR="$(realpath "${1:-release-artifacts}")"
ARCH_FILTER="${2:-all}"
mkdir -p "$OUTPUT_DIR"

# ── Resolve image files ───────────────────────────────────────────────────────
IMAGE_FILE=$(ls CuisImage/ | grep -E '^Cuis[0-9]+\.[0-9]+-[0-9]+\.image$' | head -1)
[[ -z "$IMAGE_FILE" ]] && { echo "ERROR: no .image found in CuisImage/" >&2; exit 1; }

IMAGE_BASE="${IMAGE_FILE%.image}"
CHANGES_FILE="${IMAGE_BASE}.changes"
SOURCES_FILE=$(ls CuisImage/ | grep -E '^Cuis[0-9]+\.[0-9]+\.sources$' | head -1)
[[ -z "$SOURCES_FILE" ]] && { echo "ERROR: no .sources found in CuisImage/" >&2; exit 1; }

echo "Image   : CuisImage/$IMAGE_FILE"
echo "Sources : CuisImage/$SOURCES_FILE"
echo "Arch    : $ARCH_FILTER"
echo ""

# ── Package one platform ──────────────────────────────────────────────────────
# Args:
#   $1  platform slug         e.g. linux-x86_64
#   $2  VM content dir        e.g. CuisVM.app/Contents/Linux-x86_64
#   $3+ run scripts to copy
package_platform() {
    local PLATFORM="$1"; shift
    local VM_DIR="$1";   shift
    local RUN_SCRIPTS=("$@")

    local ZIP_NAME="${IMAGE_BASE}-${PLATFORM}.zip"
    local STAGING ZIP_PATH
    STAGING=$(mktemp -d)
    ZIP_PATH="$OUTPUT_DIR/$ZIP_NAME"
    local DEST="$STAGING/Cuis-${PLATFORM}"
    mkdir -p "$DEST/CuisImage" "$DEST/CuisVM.app/Contents" "$DEST/TrueTypeFonts"

    echo "Packaging $PLATFORM ..."

    # Minimal image files
    cp "CuisImage/$IMAGE_FILE"     "$DEST/CuisImage/"
    cp "CuisImage/$CHANGES_FILE"   "$DEST/CuisImage/" 2>/dev/null || true
    cp "CuisImage/$SOURCES_FILE"   "$DEST/CuisImage/"
    cp "CuisImage/UnicodeData.txt" "$DEST/CuisImage/"

    # VM for this platform only (no other arch binaries)
    cp -r "$VM_DIR" "$DEST/CuisVM.app/Contents/"
    cp -r "CuisVM.app/Contents/Resources" "$DEST/CuisVM.app/Contents/"

    # Fonts
    cp -r TrueTypeFonts/. "$DEST/TrueTypeFonts/"

    # Run scripts
    for s in "${RUN_SCRIPTS[@]}"; do
        [[ -f "$s" ]] && cp "$s" "$DEST/"
    done

    cp LICENSE   "$DEST/"
    cp README.md "$DEST/"

    find "$DEST" \( -name "*.sh" -o -name "*.command" \) -exec chmod +x {} +

    (cd "$STAGING" && zip -qr --symlinks "$ZIP_PATH" "Cuis-${PLATFORM}")
    rm -rf "$STAGING"

    echo "  -> $ZIP_NAME  ($(du -sh "$ZIP_PATH" | cut -f1))"
}

# ── Build platforms filtered by arch ─────────────────────────────────────────
case "$ARCH_FILTER" in
  arm64)
    package_platform "linux-arm64" \
        "CuisVM.app/Contents/Linux-arm64" \
        "RunCuisOnLinux.sh"
    ;;
  amd64|all)
    package_platform "linux-x86_64" \
        "CuisVM.app/Contents/Linux-x86_64" \
        "RunCuisOnLinux.sh"

    package_platform "macos" \
        "CuisVM.app/Contents/MacOS" \
        "RunCuisOnMac.sh" "RunCuisOnFinder.command" "unquarantine.sh"

    package_platform "windows-x86_64" \
        "CuisVM.app/Contents/Windows-x86_64" \
        "RunCuisOnWindows.bat"

    package_platform "windows-arm64" \
        "CuisVM.app/Contents/Windows-arm64" \
        "RunCuisOnWindows.bat"

    if [[ "$ARCH_FILTER" == "all" ]]; then
      package_platform "linux-arm64" \
          "CuisVM.app/Contents/Linux-arm64" \
          "RunCuisOnLinux.sh"
    fi
    ;;
  *)
    echo "ERROR: unknown arch filter '$ARCH_FILTER'. Use: amd64, arm64, all" >&2
    exit 1
    ;;
esac

echo ""
echo "Artifacts:"
ls -lh "$OUTPUT_DIR/"

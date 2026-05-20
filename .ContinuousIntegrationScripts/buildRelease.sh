#!/bin/bash
# buildRelease.sh — Package Cuis into platform-specific release ZIPs.
#
# Produces one ZIP per platform containing only the files needed to run Cuis:
#   - The updated image + sources + UnicodeData
#   - The platform VM (binary + plugins)
#   - Run scripts
#   - TrueTypeFonts
#
# Usage: buildRelease.sh <output-dir>
#   output-dir: directory where ZIPs will be written (created if missing)

set -euo pipefail

OUTPUT_DIR="${1:-release-artifacts}"
mkdir -p "$OUTPUT_DIR"

# Resolve image name dynamically (e.g. Cuis7.7-7953.image)
IMAGE_FILE=$(ls CuisImage/ | grep -E 'Cuis[0-9]+\.[0-9]+-[0-9]+\.image' | head -1)
if [[ -z "$IMAGE_FILE" ]]; then
  echo "ERROR: no .image file found in CuisImage/" >&2
  exit 1
fi

IMAGE_BASE="${IMAGE_FILE%.image}"       # e.g. Cuis7.7-7953
SOURCES_FILE=$(ls CuisImage/ | grep -E 'Cuis[0-9]+\.[0-9]+\.sources' | head -1)

echo "Image: $IMAGE_FILE"
echo "Sources: $SOURCES_FILE"

# Files common to all platforms
COMMON_FILES=(
  "CuisImage/$IMAGE_FILE"
  "CuisImage/${IMAGE_BASE}.changes"
  "CuisImage/$SOURCES_FILE"
  "CuisImage/UnicodeData.txt"
  "TrueTypeFonts"
  "LICENSE"
  "README.md"
)

package_platform() {
  local PLATFORM="$1"       # e.g. linux-x86_64
  local VM_DIR="$2"         # e.g. CuisVM.app/Contents/Linux-x86_64
  local RUN_SCRIPT="$3"     # e.g. RunCuisOnLinux.sh
  local ZIP_NAME="${IMAGE_BASE}-${PLATFORM}.zip"

  echo "--- Packaging $PLATFORM ---"

  local STAGING
  STAGING=$(mktemp -d)
  local DEST="$STAGING/Cuis-${PLATFORM}"
  mkdir -p "$DEST"

  # Copy common files
  for f in "${COMMON_FILES[@]}"; do
    if [[ -e "$f" ]]; then
      cp -r "$f" "$DEST/"
    fi
  done

  # Copy VM for this platform (preserve structure)
  mkdir -p "$DEST/CuisVM.app/Contents"
  cp -r "$VM_DIR" "$DEST/CuisVM.app/Contents/"
  cp -r "CuisVM.app/Contents/Resources" "$DEST/CuisVM.app/Contents/"

  # Copy run script(s)
  for s in $RUN_SCRIPT; do
    [[ -f "$s" ]] && cp "$s" "$DEST/"
  done

  # Make scripts executable
  find "$DEST" -name "*.sh" -o -name "*.command" | xargs -r chmod +x

  # Zip
  (cd "$STAGING" && zip -qr "$OLDPWD/$OUTPUT_DIR/$ZIP_NAME" "Cuis-${PLATFORM}")
  rm -rf "$STAGING"

  echo "Created: $OUTPUT_DIR/$ZIP_NAME ($(du -sh "$OUTPUT_DIR/$ZIP_NAME" | cut -f1))"
}

package_platform "linux-x86_64"   "CuisVM.app/Contents/Linux-x86_64"   "RunCuisOnLinux.sh"
package_platform "linux-arm64"    "CuisVM.app/Contents/Linux-arm64"    "RunCuisOnLinux.sh"
package_platform "macos"          "CuisVM.app/Contents/MacOS"          "RunCuisOnMac.sh RunCuisOnFinder.command unquarantine.sh"
package_platform "windows-x86_64" "CuisVM.app/Contents/Windows-x86_64" "RunCuisOnWindows.bat"
package_platform "windows-arm64"  "CuisVM.app/Contents/Windows-arm64"  "RunCuisOnWindows.bat"

echo ""
echo "Release artifacts:"
ls -lh "$OUTPUT_DIR/"

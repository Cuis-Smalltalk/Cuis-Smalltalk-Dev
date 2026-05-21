#!/usr/bin/env bash
set -euo pipefail

ARCH_FILTER="${1:?usage: package-release-zips.sh <amd64|arm64|all> [image-dir] [output-dir] [mcp-dir]}"
IMAGE_DIR="${2:-CuisImage}"
OUTPUT_DIR_ARG="${3:-release-artifacts}"
MCP_DIR="${4:-cuis-mcp}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$REPO_ROOT"
mkdir -p "$OUTPUT_DIR_ARG"
OUTPUT_DIR="$(cd "$OUTPUT_DIR_ARG" && pwd -P)"

IMAGE_FILE="$(bash "$SCRIPT_DIR/find-cuis-image.sh" "$IMAGE_DIR")"
IMAGE_BASE="${IMAGE_FILE%.image}"
CHANGES_FILE="${IMAGE_BASE}.changes"

IFS=$'\n' read -r -d '' -a SOURCES_FILES < <(find "$IMAGE_DIR" -maxdepth 1 -type f -name 'Cuis*.*.sources' -print | sort && printf '\0')
if [[ "${#SOURCES_FILES[@]}" -eq 0 ]]; then
  echo "ERROR: no .sources found in $IMAGE_DIR/" >&2
  exit 1
fi
if [[ "${#SOURCES_FILES[@]}" -gt 1 ]]; then
  echo "ERROR: multiple .sources files found in $IMAGE_DIR/:" >&2
  printf '  %s\n' "${SOURCES_FILES[@]}" >&2
  exit 1
fi
SOURCES_FILE="$(basename "${SOURCES_FILES[0]}")"

HAS_MCP=false
if [[ -d "$MCP_DIR/skills" ]]; then
  HAS_MCP=true
fi

echo "Image   : $IMAGE_DIR/$IMAGE_FILE"
echo "Sources : $IMAGE_DIR/$SOURCES_FILE"
echo "Arch    : $ARCH_FILTER"
if [[ "$HAS_MCP" == true ]]; then
  echo "MCP     : $MCP_DIR"
else
  echo "MCP     : not bundled"
fi
echo ""

package_platform() {
  local PLATFORM="$1"; shift
  local VM_DIR="$1"; shift
  local RUN_SCRIPTS=("$@")

  local ZIP_NAME="${IMAGE_BASE}-${PLATFORM}.zip"
  local STAGING ZIP_PATH DEST
  STAGING="$(mktemp -d)"
  ZIP_PATH="$OUTPUT_DIR/$ZIP_NAME"
  DEST="$STAGING/Cuis-${PLATFORM}"
  mkdir -p "$DEST/CuisImage" "$DEST/CuisVM.app/Contents" "$DEST/TrueTypeFonts"

  echo "Packaging $PLATFORM ..."

  cp "$IMAGE_DIR/$IMAGE_FILE" "$DEST/CuisImage/"
  cp "$IMAGE_DIR/$CHANGES_FILE" "$DEST/CuisImage/" 2>/dev/null || true
  cp "$IMAGE_DIR/$SOURCES_FILE" "$DEST/CuisImage/"
  cp "$IMAGE_DIR/UnicodeData.txt" "$DEST/CuisImage/"

  cp -r "$VM_DIR" "$DEST/CuisVM.app/Contents/"
  cp -r "CuisVM.app/Contents/Resources" "$DEST/CuisVM.app/Contents/"

  cp -r TrueTypeFonts/. "$DEST/TrueTypeFonts/"
  if [[ "$HAS_MCP" == true ]]; then
    mkdir -p "$DEST/cuis-mcp"
    cp -r "$MCP_DIR/skills" "$DEST/cuis-mcp/"
    cp "$MCP_DIR/README.md" "$DEST/cuis-mcp/" 2>/dev/null || true
    cp "$MCP_DIR/shim.sh" "$DEST/cuis-mcp/" 2>/dev/null || true
    cp "$MCP_DIR/start-headless.sh" "$DEST/cuis-mcp/" 2>/dev/null || true
  fi

  for s in "${RUN_SCRIPTS[@]}"; do
    [[ -f "$s" ]] && cp "$s" "$DEST/"
  done

  cp LICENSE "$DEST/"
  cp README.md "$DEST/"

  find "$DEST" \( -name "*.sh" -o -name "*.command" \) -exec chmod +x {} +

  (cd "$STAGING" && zip -qr --symlinks "$ZIP_PATH" "Cuis-${PLATFORM}")
  rm -rf "$STAGING"

  echo "  -> $ZIP_NAME  ($(du -sh "$ZIP_PATH" | cut -f1))"
}

case "$ARCH_FILTER" in
  arm64)
    echo "Skipping ZIP packaging for arm64. This release only ships macOS and linux-x86_64 ZIPs." 
    ;;
  amd64|all)
    package_platform "linux-x86_64" \
      "CuisVM.app/Contents/Linux-x86_64" \
      "RunCuisOnLinux.sh"

    package_platform "macos" \
      "CuisVM.app/Contents/MacOS" \
      "RunCuisOnMac.sh" "RunCuisOnFinder.command" "unquarantine.sh"
    ;;
  *)
    echo "ERROR: unknown arch filter '$ARCH_FILTER'. Use: amd64, arm64, all" >&2
    exit 1
    ;;
esac

echo ""
echo "Artifacts:"
ls -lh "$OUTPUT_DIR/"

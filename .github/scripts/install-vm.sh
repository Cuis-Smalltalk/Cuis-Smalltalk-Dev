#!/bin/bash

set -euo pipefail

echo "Setting up VM for $RUNNER_OS ($(uname -m))"

installVmLinux() {
  local ARCH
  ARCH=$(uname -m)

  case "$ARCH" in
    x86_64)
      CUIS_VM_PATH=CuisVM.app/Contents/Linux-x86_64/squeak
      ;;
    aarch64|arm64)
      CUIS_VM_PATH=CuisVM.app/Contents/Linux-arm64/squeak
      # The squeak wrapper script uses ldd to detect the libc path.
      # On Ubuntu 24.04 ARM64 the ldd output format differs from what
      # the script expects. We export PLATFORMLIBDIR to skip ldd detection.
      export PLATFORMLIBDIR=/lib/aarch64-linux-gnu
      ;;
    *)
      echo "Unsupported Linux architecture: $ARCH"
      exit 1
      ;;
  esac

  CUIS_VM_ARGUMENTS="-vm-sound-null -vm-display-null"
  chmod +x "$CUIS_VM_PATH"
  "$CUIS_VM_PATH" --version
}

installVmMacOS() {
  CUIS_VM_PATH=CuisVM.app/Contents/MacOS/Squeak
  CUIS_VM_ARGUMENTS="-headless"
  chmod +x "$CUIS_VM_PATH"
  "$CUIS_VM_PATH" -version
}

installVmWindows() {
  CUIS_VM_PATH=CuisVM.app/Contents/Windows-x86_64/SqueakConsole.exe
  CUIS_VM_ARGUMENTS="-headless"
  "$CUIS_VM_PATH" -version
}

case $RUNNER_OS in
  "Linux")   installVmLinux   ;;
  "macOS")   installVmMacOS   ;;
  "Windows") installVmWindows ;;
  *)
    echo "Unsupported OS: $RUNNER_OS"
    exit 1
    ;;
esac

if [[ -n "${GITHUB_ENV:-}" ]]; then
  echo "CUIS_VM_PATH=$CUIS_VM_PATH"           >> "$GITHUB_ENV"
  echo "CUIS_VM_ARGUMENTS=$CUIS_VM_ARGUMENTS" >> "$GITHUB_ENV"
else
  # Allow local usage outside GitHub Actions.
  export CUIS_VM_PATH
  export CUIS_VM_ARGUMENTS
  echo "CUIS_VM_PATH=$CUIS_VM_PATH"
  echo "CUIS_VM_ARGUMENTS=$CUIS_VM_ARGUMENTS"
fi

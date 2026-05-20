#!/bin/bash

set -euo pipefail

echo "Setting up VM for $RUNNER_OS"

installVmLinux() {
  CUIS_VM_PATH=CuisVM.app/Contents/Linux-x86_64/squeak
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

# Make the environment variables available to subsequent steps
echo "CUIS_VM_PATH=$CUIS_VM_PATH"       >> "$GITHUB_ENV"
echo "CUIS_VM_ARGUMENTS=$CUIS_VM_ARGUMENTS" >> "$GITHUB_ENV"

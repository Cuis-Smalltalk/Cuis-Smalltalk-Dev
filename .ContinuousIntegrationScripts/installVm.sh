#!/bin/bash

set -euo pipefail

echo "Setting up VM for $RUNNER_OS"

installVmLinux() {
  sudo apt-get update
  sudo apt-get install pulseaudio

  #Use VM included with Cuis in this repo
  CUIS_VM_PATH=CuisVM.app/Contents/Linux-x86_64/squeak
  CUIS_VM_ARGUMENTS="-vm-display-null"
  pwd
  "$CUIS_VM_PATH" --version
}

installVmMacOS() {

  #Use VM included with Cuis in this repo
  CUIS_VM_PATH=CuisVM.app/Contents/MacOS/Squeak
  CUIS_VM_ARGUMENTS="-headless"
  pwd
  "$CUIS_VM_PATH" -version
}

case $RUNNER_OS in
  "Linux")
    installVmLinux ;;
  "macOS")
    installVmMacOS ;;
esac

# Make the environment variables available to other scripts
echo "CUIS_VM_PATH=$CUIS_VM_PATH" >> "$GITHUB_ENV"
echo "CUIS_VM_ARGUMENTS=$CUIS_VM_ARGUMENTS" >> "$GITHUB_ENV"
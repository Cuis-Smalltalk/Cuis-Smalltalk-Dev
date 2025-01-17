#!/bin/bash

set -euo pipefail
VM_VERSION="202501132308"
BASE_VM_DOWNLOAD_PATH="https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/$VM_VERSION"

echo "Installing VM $VM_VERSION for $RUNNER_OS"

installVmLinux() {
  sudo apt-get update
  sudo apt-get install pulseaudio
  VM_FILENAME="squeak.cog.spur_linux64x64_itimer"

  #Use an unreleased, recent build
  #wget "$BASE_VM_DOWNLOAD_PATH/$VM_FILENAME.tar.gz"
  wget https://github.com/OpenSmalltalk/opensmalltalk-vm/actions/runs/12757798924/artifacts/2425377956
  tar -xvzf "$VM_FILENAME.tar.gz"

  CUIS_VM_PATH="$GITHUB_WORKSPACE"/sqcogspur64linux/squeak
  CUIS_VM_ARGUMENTS="-vm-display-null"
  "$CUIS_VM_PATH" --version
}

installVmMacOS() {
  VM_FILENAME="squeak.cog.spur_macos64x64"

  #Use an unreleased, recent build
  #wget "$BASE_VM_DOWNLOAD_PATH/$VM_FILENAME.dmg"
  wget https://github.com/OpenSmalltalk/opensmalltalk-vm/actions/runs/12757798952/artifacts/2425374280
  sudo hdiutil attach "$VM_FILENAME.dmg"
  cd "/Volumes/$VM_FILENAME"
  sudo cp -rf Squeak.app /Applications

  CUIS_VM_PATH=/Applications/Squeak.app/Contents/MacOS/Squeak
  CUIS_VM_ARGUMENTS="-headless"
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

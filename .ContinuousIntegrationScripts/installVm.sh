#!/bin/bash

set -euo pipefail
VM_VERSION="202206021410"
BASE_VM_DOWNLOAD_PATH="https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/$VM_VERSION"

echo "Installing VM $VM_VERSION for $RUNNER_OS"

installVmLinux() {
  sudo apt-get update
  sudo apt-get install pulseaudio
  VM_FILENAME="squeak.cog.spur_linux64x64_itimer"

  wget "$BASE_VM_DOWNLOAD_PATH/$VM_FILENAME.tar.gz"
  tar -xvzf "$VM_FILENAME.tar.gz"
  sqcogspur64linux/squeak --version
}

installVmMacOS() {
  VM_FILENAME="squeak.cog.spur_macos64x64"

  wget "$BASE_VM_DOWNLOAD_PATH/$VM_FILENAME.dmg"
  sudo hdiutil attach "$VM_FILENAME.dmg"
  cd "/Volumes/$VM_FILENAME"
  sudo cp -rf Squeak.app /Applications
}

case $RUNNER_OS in
  "Linux")
    installVmLinux ;;
  "macOS")
    installVmMacOS ;;
esac

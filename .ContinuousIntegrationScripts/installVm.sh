#!/bin/bash

set -euo pipefail
VM_VERSION="201911012148"
BASE_VM_DOWNLOAD_PATH="https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/$VM_VERSION"

echo "Installing VM $VM_VERSION for $TRAVIS_OS_NAME"

installVmLinux() {
  VM_FILENAME="squeak.cog.spur_linux64x64_$VM_VERSION"

  wget "$BASE_VM_DOWNLOAD_PATH/$VM_FILENAME.tar.gz"
  tar -xvzf "$VM_FILENAME.tar.gz"
  sqcogspur64linuxht/bin/squeak --version
}

installVmMacOS() {
  VM_FILENAME="squeak.cog.spur_macos64x64_$VM_VERSION"

  wget "$BASE_VM_DOWNLOAD_PATH/$VM_FILENAME.dmg"
  sudo hdiutil attach "$VM_FILENAME.dmg"
  cd "/Volumes/$VM_FILENAME"
  sudo cp -rf Squeak.app /Applications
}

case $TRAVIS_OS_NAME in
  "linux")
    installVmLinux ;;
  "osx")
    installVmMacOS ;;
esac

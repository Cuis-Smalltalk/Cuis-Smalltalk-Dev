#!/bin/bash

set -euo pipefail

echo "Setting up VM for macos"

wget https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201901172323/squeak.cog.spur_macos64x64_201901172323.dmg
sudo hdiutil attach squeak.cog.spur_macos64x64_201901172323.dmg
ls -a /Volumes
cd /Volumes/squeak.cog.spur_macos64x64_201901172323
sudo cp -rf Squeak.app /Applications

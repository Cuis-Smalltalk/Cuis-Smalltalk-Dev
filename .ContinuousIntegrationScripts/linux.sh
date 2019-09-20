#!/bin/bash

set -euo pipefail

echo "Setting up VM for linux"

wget https://github.com/OpenSmalltalk/opensmalltalk-vm/releases/download/201901172323/squeak.cog.spur_linux64x64_201901172323.tar.gz
tar -xvzf squeak.cog.spur_linux64x64_201901172323.tar.gz
sqcogspur64linuxht/bin/squeak --version

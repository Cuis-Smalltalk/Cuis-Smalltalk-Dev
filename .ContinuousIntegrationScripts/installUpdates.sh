#!/bin/bash

set -euo pipefail

IMAGE_FILE="$(ls | grep 'Cuis5.0-[0-9]\+.image')"

./sqcogspur64linuxht/bin/squeak -vm-display-null "$IMAGE_FILE" -d "\
    ChangeSet installNewUpdates.\
    Smalltalk snapshot: true andQuit: true clearAllClassState: false.\
    "

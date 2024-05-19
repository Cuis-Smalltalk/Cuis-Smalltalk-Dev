#!/bin/bash

set -euo pipefail

IMAGE_FILE="$(ls CuisImage/ | grep 'Cuis7.1-[0-9]\+.image')"

INSTALL_UPDATES_SCRIPT="\
  Utilities classPool at: #AuthorName put: 'TravisCI'.
  Utilities classPool at: #AuthorInitials put: 'TCI'.
  ChangeSet installNewUpdates.\
  Smalltalk saveAndQuit.\
"

installUpdatesLinux() {
  /home/runner/work/Cuis-Smalltalk-Dev/Cuis-Smalltalk-Dev/sqcogspur64linux/squeak -vm-display-null CuisImage/"$IMAGE_FILE" -d "$INSTALL_UPDATES_SCRIPT"
}

installUpdatesMacOS() {
  /Applications/Squeak.app/Contents/MacOS/Squeak -headless CuisImage/"$IMAGE_FILE" -d "$INSTALL_UPDATES_SCRIPT"
}

case $RUNNER_OS in
  "Linux")
    installUpdatesLinux ;;
  "macOS")
    installUpdatesMacOS ;;
esac

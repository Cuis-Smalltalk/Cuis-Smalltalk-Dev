#!/bin/bash

set -euo pipefail

CURRENT_SCRIPT_FOLDER=$(dirname "$(realpath "$0")")

INSTALL_UPDATES_SCRIPT="\
  Utilities classPool at: #AuthorName put: 'GitHubActions'.
  Utilities classPool at: #AuthorInitials put: 'GHA'.
  ChangeSet installNewUpdates.\
  Smalltalk saveAndQuit.\
"

"$CURRENT_SCRIPT_FOLDER/run-cuis.sh" -d "$INSTALL_UPDATES_SCRIPT"
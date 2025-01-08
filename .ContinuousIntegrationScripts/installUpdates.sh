#!/bin/bash

set -euo pipefail

CURRENT_SCRIPT_FOLDER=$(dirname "$(realpath "$0")")

INSTALL_UPDATES_SCRIPT="\
  Utilities classPool at: #AuthorName put: 'TravisCI'.
  Utilities classPool at: #AuthorInitials put: 'TCI'.
  ChangeSet installNewUpdates.\
  Smalltalk saveAndQuit.\
"

"$CURRENT_SCRIPT_FOLDER/runCuis.sh" -d "$INSTALL_UPDATES_SCRIPT"
"smoke-boot.st

Purpose: minimal "image boots" smoke check.
If the image can start and evaluate this script, we consider it bootable.
It exits without snapshotting.
"

Utilities classPool at: #AuthorName put: 'OpenCode'.
Utilities classPool at: #AuthorInitials put: 'OC'.

Transcript showCr: 'SMOKE: image boot OK'.

Smalltalk snapshot: false andQuit: true.

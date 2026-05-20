| outputDir |

Utilities classPool at: #AuthorName put: 'GitHubActions'.
Utilities classPool at: #AuthorInitials put: 'GHA'.

"Load the Tonel package writer if not already present"
(Smalltalk includesKey: #TonelWriter) ifFalse: [
    CodePackageFile installPackage:
        DirectoryEntry currentDirectory // 'Packages/System/Tonel.pck.st'.
].

outputDir := DirectoryEntry currentDirectory // 'tonel-export'.
outputDir exists ifFalse: [ outputDir createDirectory ].

"Export every loaded package that has a file-backed definition"
Smalltalk packageOrganizer packages do: [ :pkg |
    | writer |
    writer := TonelWriter on: outputDir.
    writer writePackage: pkg.
].

Transcript showCr: 'Tonel export complete: ', outputDir pathString.

Smalltalk quitPrimitive: 0

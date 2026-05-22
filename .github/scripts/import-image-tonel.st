"Import all Tonel source files from a grouped Tonel source directory."

[
	| repoRoot inputRelativePath inputRoot sourceRoot |
	repoRoot := ((Smalltalk at: #TonelImportRepoRootDir ifAbsent: [ DirectoryEntry currentDirectory pathName ]) asString) asDirectoryEntry.
	inputRelativePath := Smalltalk at: #TonelImportInputDir ifAbsent: [ 'Packages/TonelImage' ].
	inputRoot := (repoRoot pathName, '/', inputRelativePath) asDirectoryEntry.
	sourceRoot := inputRoot / 'src'.

	TonelImporter importDirectory: sourceRoot.

	StdIOWriteStream stdout
		nextPutAll: 'Imported Tonel source from '; nextPutAll: sourceRoot pathName; newLine;
		flush.

	Smalltalk quitPrimitive: 0
] on: Error do: [ :ex |
	StdIOWriteStream stdout nextPutAll: 'ERROR: '; nextPutAll: ex printString; newLine; flush.
	StdIOWriteStream stdout nextPutAll: 'SIGNALER: '; nextPutAll: ex signalerContext printString; newLine; flush.
	Smalltalk quitPrimitive: 1
].


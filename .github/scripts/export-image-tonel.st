"Export all classes currently defined in the image as grouped Tonel source."

[
	| repoRoot outputRelativePath outputRoot |
	repoRoot := ((Smalltalk at: #TonelExportRepoRootDir ifAbsent: [ DirectoryEntry currentDirectory pathName ]) asString) asDirectoryEntry.
	outputRelativePath := Smalltalk at: #TonelExportOutputDir ifAbsent: [ 'Packages/TonelImage' ].
	outputRoot := (repoRoot pathName, '/', outputRelativePath) asDirectoryEntry assureExistence.

	TonelImageExporter exportAllTo: outputRoot.

	StdIOWriteStream stdout
		nextPutAll: 'Exported Tonel image source to '; nextPutAll: outputRoot pathName; newLine;
		nextPutAll: 'Exported class count: '; nextPutAll: Smalltalk allClasses size printString; newLine;
		flush.

	Smalltalk quitPrimitive: 0
] on: Error do: [ :ex |
	StdIOWriteStream stdout nextPutAll: 'ERROR: '; nextPutAll: ex printString; newLine; flush.
	StdIOWriteStream stdout nextPutAll: 'SIGNALER: '; nextPutAll: ex signalerContext printString; newLine; flush.
	Smalltalk quitPrimitive: 1
].


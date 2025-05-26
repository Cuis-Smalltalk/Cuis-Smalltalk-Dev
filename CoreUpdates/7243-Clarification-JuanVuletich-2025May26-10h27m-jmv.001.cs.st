'From Cuis7.3 [latest update: #7242] on 26 May 2025 at 10:28:13 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 5/26/2025 10:27:58'!
putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		remoteString := RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Utilities logsUserChanges ifTrue: [
		DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName appendStreamDo: [ :stream |
			preambleBlock value: stream.  "Write the preamble"
			stream nextChunkPut: sourceStr.
			stream nextChunkPut: ' ' ]]! !


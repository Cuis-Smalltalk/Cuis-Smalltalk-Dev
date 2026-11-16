'From Cuis7.9 [latest update: #8342] on 12 November 2026 at 4:25:31 pm'!

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 7:17:54 pm (UTC)'!
fileOutCommentOn: aFileStream toFile: fileIndex
	"Copy the class comment to aFileStream."

	self hasClassComment ifTrue: [
		aFileStream newLine.
		fileIndex = 0 ifFalse: [
			aFileStream padSpacesTo: SourceFiles pointerScaleForWriting ].
		aFileStream nextChunkPut: self classComment asPlainString.
		Smalltalk userChangesAppendDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]].! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 12/Nov/2026, 7:22:56 pm (UTC)'!
getSourceFromFile
	"Read the source code from file, determining source file index and
	file position from the last 3 bytes of this method."
	| position theFile answer |
	(position := self filePosition) = 0 ifTrue: [^ nil].
	theFile := SourceFiles at: self fileIndex.
	theFile position: position.
	answer := theFile nextChunk.
	^answer isEmpty ifTrue: [nil] ifFalse: [answer]! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 12/Nov/2026, 7:18:58 pm (UTC)'!
putSource: sourceStr inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file position  |
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		fileIndex = 0 ifFalse: [
			file padSpacesTo: SourceFiles pointerScaleForWriting ].
		position := file position.
		file nextChunkPut: sourceStr asPlainString.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self setSourcePosition: position inFile: fileIndex.
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].! !

!classRemoval: #RemoteString stamp: 'jmv 12/Nov/2026, 7:25:23 pm (UTC)'!
Smalltalk removeClassNamed: #RemoteString!


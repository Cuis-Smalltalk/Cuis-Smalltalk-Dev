'From Cuis7.9 [latest update: #8350] on 13 November 2026 at 12:04:42 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 13/Nov/2026, 3:03:57 pm (UTC)'!
fileOutCategory: aSymbol onNewSources: aFileStream
	"File a description of the receiver's category, aString, onto a new .sources file.
	Set the trailing bytes of each method to the position of aFileStream
	and to fileIndex in order to indicate where to find the source code.
	Note:
		- This effectively kills method history
		- Leaves orphan entries in CodeHistory (that point to now obsolete positions in .sources and .changes
		- Used to create a new .sources file. Senders should eventually do `CodeHistory initialize`
		- Breaks consistency with loaded SmalltalkLibraries. Use with care."

	| selectors newPos |

	aFileStream newLine.
	selectors := (aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
				ifTrue: [ self organization allMethodSelectors ]
				ifFalse: [ self organization listAtCategoryNamed: aSymbol ].

	selectors do: [ :sel |
		self methodDict at: sel
			ifPresent: [ :compiledMethod |
				newPos := self
					printMethodChunkFor: compiledMethod
					withPreamble: true
					on: aFileStream
					padForPointerScale: true.
				newPos > 0 ifTrue: [
					"Set the new method source pointer"
					compiledMethod historyPointer:
						(CodeHistory historyPointerFromFileIndex: 1 andPosition: newPos). ]]].! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 13/Nov/2026, 3:03:11 pm (UTC)'!
writeToChangesFile: sourceStr withPreamble: preambleBlock
	"Store the source code for the receiver on the external .changes file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	Store a 4-byte source code pointer at the method end."

	| fileIndex file position  |
	fileIndex := 2.
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		file padSpacesTo: SourceFiles pointerScaleForWriting.
		position := file position.
		file nextChunkPut: sourceStr asPlainString.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self historyPointer: (CodeHistory historyPointerFromFileIndex: fileIndex andPosition: position).
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].! !

!methodRemoval: CompiledMethod #setSourcePosition:inFile: stamp: 'jmv 13/Nov/2026, 3:04:17 pm (UTC)'!
CompiledMethod removeSelector: #setSourcePosition:inFile:!

'From Cuis7.9 [latest update: #8315] on 4 November 2026 at 5:41:54 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 8:34:53 pm (UTC)'!
classComment: aString
	"Store the comment associated with the class we are orgainzing.  Empty string gets stored only if had a non-empty one before."
	self classComment: aString stamp: '<historical>'! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 8:35:01 pm (UTC)'!
classComment: aString stamp: aStamp
	"Store the comment, associated with the class we are organizing."

	| header |
	self organization classComment: aString stamp: aStamp.

	Smalltalk userChangesAppendDo: [ :stream |
		stream newLine; nextPut: $!!.	"directly"
		header := String streamContents: [:strm | strm nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm ].
		stream nextChunkPut: header.
		stream newLine; nextChunkPut: aString ].

	SystemChangeNotifier uniqueInstance classCommented: self.! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/Nov/2026, 8:38:29 pm (UTC)'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.
	If moveSource true, then also set the source code pointer of the method."

	| compiledMethod oldPos newPos sourceFile endPos methodCode methodCodeText codeStyler |
	compiledMethod := self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	doPreamble ifTrue: [
		self
			printCategoryChunk: (self organization categoryOfElement: selector)
			on: outStream
			withStamp: compiledMethod authorAndStampUTC
			priorMethod: nil.
		compiledMethod
			withPropertiesDo: [ :properties |
				properties hasAnyProperty ifTrue: [
					self printPropertiesChunk: properties on: outStream ]]
			ifSelector: [ :sel | nil ].
		outStream newLine ].

	((compiledMethod fileIndex = 0
		or: [(SourceFiles at: compiledMethod fileIndex) == nil])
		or: [(oldPos := compiledMethod filePosition) = 0])
			ifTrue: [
				"The source code is not accessible.  We must decompile..."
				outStream nextChunkPut: compiledMethod decompileString]
			ifFalse: [
				sourceFile := SourceFiles at: compiledMethod fileIndex.
				sourceFile position: oldPos.
				"Copy the method chunk"
				fileIndex = 0 ifFalse: [
					outStream padSpacesTo: SourceFiles pointerScaleForWriting ].
				newPos := outStream position.
				"Copy method chunk"
				methodCode := sourceFile nextChunk.
				(methodCode isEmpty or: [ methodCode isSeparators ]) ifTrue: [
					"This is very unlikely, but possible source file is corrupted."
					methodCode := compiledMethod decompileString ].
				"Convert method code to use ANSI assignments"
				(Preferences at: #fileOutANSIassignment) ifTrue: [
					codeStyler := SHTextStylerST80 new.
					codeStyler classOrMetaClass: compiledMethod methodClass.
					methodCodeText := methodCode asText.
					codeStyler instVarNamed: 'formattedText' put: methodCodeText.
					codeStyler getReady.
					codeStyler parseText.
					codeStyler replaceStringForRangesWithType: #assignment with: ':=' offset: 0.
					methodCode := methodCodeText string ].
				outStream nextChunkPut: methodCode.
				moveSource ifTrue: [    "Set the new method source pointer"
					endPos := outStream position.
					compiledMethod setSourcePosition: newPos inFile: fileIndex.
					SourceFiles checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex ]].
	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].
	outStream newLine.
! !


!BasicClassOrganizer methodsFor: 'private' stamp: 'jmv 4/Nov/2026, 8:16:24 pm (UTC)'!
hasClassComment

	^classComment notNil! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 4/Nov/2026, 8:37:53 pm (UTC)'!
putSource: sourceStr inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file position  |
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		position := (RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file) position.
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

!methodRemoval: RemoteString #last stamp: 'jmv 4/Nov/2026, 8:32:21 pm (UTC)'!
RemoteString removeSelector: #last!
!methodRemoval: RemoteString #compilerClass stamp: 'jmv 4/Nov/2026, 8:28:32 pm (UTC)'!
RemoteString removeSelector: #compilerClass!
!methodRemoval: RemoteString #historyPointer stamp: 'jmv 4/Nov/2026, 8:31:27 pm (UTC)'!
RemoteString removeSelector: #historyPointer!
!methodRemoval: RemoteString class #newString:onFileNumber: stamp: 'jmv 4/Nov/2026, 9:03:55 pm (UTC)'!
RemoteString class removeSelector: #newString:onFileNumber:!
!methodRemoval: RemoteString #string:onFileNumber: stamp: 'jmv 4/Nov/2026, 9:03:55 pm (UTC)'!
RemoteString removeSelector: #string:onFileNumber:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
BasicClassOrganizer allSubInstancesDo: [ :bco | bco classCommentOrNil ifNotNil: [ bco classComment: bco classComment]].
!


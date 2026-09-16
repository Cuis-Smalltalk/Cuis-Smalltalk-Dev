'From Cuis7.9 [latest update: #8190] on 16 September 2026 at 4:31:28 pm'!

!PositionableStream methodsFor: 'positioning' stamp: 'jmv 14/Sep/2026, 3:27:26 pm (UTC)'!
padSpacesTo: nBytes
	"Pad using the argument, aCharacter, to the next boundary of nBytes characters."
	| rem |
	rem := nBytes - (self position \\ nBytes).
	rem = nBytes ifTrue: [^ 0].
	rem timesRepeat: [self nextPut: $ ].! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 16/Sep/2026, 7:01:20 pm (UTC)'!
padSpacesTo: nBytes
	"Pad using the argument, aCharacter, to the next boundary of nBytes characters."
	| rem |
	rem := nBytes - (self position \\ nBytes).
	rem = nBytes ifTrue: [^ 0].
	[ rem > 0 ] whileTrue: [
		self nextPutByte: 32. "Space"
		rem := rem - 1 ].
	lastWrittenElement := $ .! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 14/Sep/2026, 3:34:03 pm (UTC)'!
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
					compiledMethod checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex.
					compiledMethod setSourcePosition: newPos inFile: fileIndex]].
	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].
	outStream newLine.
! !


!RemoteString methodsFor: 'private' stamp: 'jmv 14/Sep/2026, 3:34:11 pm (UTC)'!
string: aStringOrText onFileNumber: fileNumber toFile: aFileStream
	"Store this as the receiver's text if source files exist."

	| position |
	fileNumber = 0 ifFalse: [
		aFileStream padSpacesTo: SourceFiles pointerScaleForWriting ].
	position := aFileStream position.
	self fileNumber: fileNumber position: position.
	aFileStream nextChunkPut: aStringOrText asPlainString! !


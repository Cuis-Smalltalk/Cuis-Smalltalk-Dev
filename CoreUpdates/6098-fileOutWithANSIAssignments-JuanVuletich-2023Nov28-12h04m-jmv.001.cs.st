'From Cuis 6.0 [latest update: #6096] on 28 November 2023 at 2:29:03 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 11/28/2023 14:28:51'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
	| preamble compiledMethod oldPos newPos sourceFile endPos methodCode codeStyler |
	doPreamble 
		ifTrue: [preamble := self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asPlainString printString]
		ifFalse: [preamble := ''].
	compiledMethod := self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	((compiledMethod fileIndex = 0
		or: [(SourceFiles at: compiledMethod fileIndex) == nil])
		or: [(oldPos := compiledMethod filePosition) = 0])
	ifTrue: [
		"The source code is not accessible.  We must decompile..."
		preamble size > 0 ifTrue: [ outStream newLine; nextPut: $!!; nextChunkPut: preamble; newLine].
		outStream nextChunkPut: compiledMethod decompileString]
	ifFalse: [
		sourceFile := SourceFiles at: compiledMethod fileIndex.
		preamble size > 0
			ifTrue: [   "Copy the preamble"
				outStream copyPreamble: preamble oldPreamble: compiledMethod getPreamble ].
		sourceFile position: oldPos.
		"Copy the method chunk"
		fileIndex = 0 ifFalse: [
			outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
		newPos := outStream position.
		"Copy method chunk"
		methodCode := sourceFile nextChunk.
"====="
		"Convert method chunk to use ANSI assignments"
		codeStyler := SHTextStylerST80 new.
		codeStyler classOrMetaClass: compiledMethod methodClass.
		codeStyler instVarNamed: 'formattedText' put: methodCode asText.
		codeStyler getReady.
		codeStyler parseText.
		codeStyler replaceStringForRangesWithType: #assignment with: ':=' offset: 0.
		methodCode := codeStyler convertedAndStyledText string.
"====="
		outStream nextChunkPut: methodCode.
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos := outStream position.
			compiledMethod checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex.
			compiledMethod setSourcePosition: newPos inFile: fileIndex]].
	preamble notEmpty ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/15/2023 16:22:59'!
privateConvertAssignmentsOffset: delta
	"See also CodeProvider class >> #normalizeCodeForCodeFiles:	
	See also Clipboard >> #storeObject:"
	"Turn $_ into a left arrow"

	self
		replaceStringForRangesWithType: #assignment
		with: `Character smalltalkLeftArrow asString`
		offset: delta.
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow. Include a #zeroWidthSpace so as not to modify ranges, etc."
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: `Character smalltalkLeftArrow asString, UnicodeCodePoint zeroWidthSpace asString`
				offset: delta.
			"Turn $^into up arrow"
			self
				replaceStringForRangesWithType: #return
				with: `Character smalltalkUpArrow asString`
				offset: delta ].! !

!methodRemoval: PositionableStream #copyMethodChunkFrom: stamp: 'jmv 11/28/2023 13:16:36'!
PositionableStream removeSelector: #copyMethodChunkFrom:!

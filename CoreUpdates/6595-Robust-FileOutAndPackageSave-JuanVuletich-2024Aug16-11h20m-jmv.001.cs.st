'From Cuis7.1 [latest update: #6594] on 16 August 2024 at 11:20:57 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/16/2024 11:20:05'!
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
		(methodCode isEmpty or: [ methodCode isSeparators ]) ifTrue: [
			"This is very unlikely, but possible source file is corrupted."
			methodCode := compiledMethod decompileString ].

		"Convert method code to use ANSI assignments"
		(Preferences at: #fileOutANSIassignment) ifTrue: [
			codeStyler := SHTextStylerST80 new.
			codeStyler classOrMetaClass: compiledMethod methodClass.
			codeStyler instVarNamed: 'formattedText' put: methodCode asText.
			codeStyler getReady.
			codeStyler parseText.
			codeStyler replaceStringForRangesWithType: #assignment with: ':=' offset: 0.
			methodCode := codeStyler convertedAndStyledText string ].

		outStream nextChunkPut: methodCode.
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos := outStream position.
			compiledMethod checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex.
			compiledMethod setSourcePosition: newPos inFile: fileIndex]].
	preamble notEmpty ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !


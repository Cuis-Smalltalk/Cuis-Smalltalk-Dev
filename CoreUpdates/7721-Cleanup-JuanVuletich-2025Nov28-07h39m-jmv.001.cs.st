'From Cuis7.5 [latest update: #7720] on 28 November 2025 at 7:40:20 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 27/Nov/2025 17:37:18'!
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
			withStamp: compiledMethod timeStamp
			libraryName: (compiledMethod propertyValueAt: #libraryName)
			priorMethod: nil.
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
	outStream newLine.! !


!TextModel methodsFor: 'user interface support' stamp: 'jmv 27/Nov/2025 17:33:41'!
convertAndStyleAllWith: anSHTextStyler
	"Convert and style all contents as a single method or script.
	Do it in a background process if too big."

	anSHTextStyler convertAndStyle: self actualContents allowBackgroundStyleProcess: true.! !

!methodRemoval: SHTextStyler #convertedAndStyledText stamp: 'jmv 28/Nov/2025 07:40:05'!
SHTextStyler removeSelector: #convertedAndStyledText!

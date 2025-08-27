'From Cuis7.5 [latest update: #7442] on 27 August 2025 at 10:15:01 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/27/2025 09:58:55'!printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.
	If moveSource true, then also set the source code pointer of the method."

	| compiledMethod oldPos newPos sourceFile endPos methodCode codeStyler |
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
	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].
	outStream newLine.! !

!methodRemoval: PositionableStream #copyPreamble:oldPreamble: stamp: 'jmv 8/27/2025 10:13:04'!
PositionableStream removeSelector: #copyPreamble:oldPreamble:!

!PositionableStream reorganize!
('[mk] accessing' next: next:into:startingAt: nextInto: originalContents)
('[mk] positioning' position position: reset)
('[mk] private' on:)
('[mk] mode' useBytes)
('reading' back contents contentsOfEntireFile crLfNextLine next:into: nextAvailable: nextInto:startingAt: nextKeyword nextLine nextWordsInto: peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny:)
('writing' next:putAll: next:putAll:startingAt:)
('testing' atEnd atStart isBinary isEmpty isFileStream isText notEmpty)
('positioning' match: matchStart: padTo:put: padToNextLongPut: resetContents setToEnd skip skip: skipBack skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies positionError setFrom:to:)
('gui' untilEnd:displayingProgress:)
!


!MethodChangeRecord reorganize!
('method' methodInfoFromRemoval noteMethodInfoFromRemoval:)
('printing' printOn:)
('initialization' priorMethod:)
('change type' changeType noteChangeType:)
('stamp' stamp)
('perform' performOn:)
!


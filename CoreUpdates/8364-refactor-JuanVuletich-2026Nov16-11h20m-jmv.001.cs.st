'From Cuis7.9 [latest update: #8358] on 16 November 2026 at 11:42:41 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 16/Nov/2026, 2:40:56 pm (UTC)'!
printMethodChunkFor: compiledMethod withPreamble: doPreamble on: outStream padForPointerScale: doPad
	"Copy the source code for the method associated with selector onto the fileStream."

	| newPos methodCode methodCodeText codeStyler |

	methodCode := compiledMethod getSourceFromFile ifNil: [ compiledMethod decompileString ].
	
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
		
	doPreamble ifTrue: [
		compiledMethod
			writeMethodPreambleTo: outStream class: self
			category: (self organization categoryOfElement: compiledMethod selector)
			withStamp: compiledMethod authorAndStampUTC ].
	
	doPad ifTrue: [
		outStream padSpacesTo: SourceFiles pointerScaleForWriting ].
	newPos := outStream position.
	outStream nextChunkPut: methodCode.

	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].

	outStream newLine.
	^newPos! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 16/Nov/2026, 2:36:34 pm (UTC)'!
getSourceFromFile
	"Read the source code from file, determining source file index and
	file position from the last 3 bytes of this method."
	| fileIndex position theFile answer |
	(fileIndex := self fileIndex) = 0 ifTrue: [^ nil].
	(position := self filePosition) = 0 ifTrue: [^ nil].
	theFile := SourceFiles at: self fileIndex.
	theFile position: position.
	answer := theFile nextChunk.
	^answer isEmpty ifTrue: [nil] ifFalse: [answer]! !


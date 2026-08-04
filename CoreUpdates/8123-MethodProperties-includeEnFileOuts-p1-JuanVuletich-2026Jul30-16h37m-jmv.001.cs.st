'From Cuis7.9 [latest update: #8117] on 30 July 2026 at 4:38:13 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 30/Jul/2026, 7:18:47 pm (UTC)'!
printPropertiesChunk: anAdditionalMethodState on: aFileStream
	"Print a method properties chunk."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm | 
		strm nextPutAll: 'methodProperties: '.
		anAdditionalMethodState storePropertiesOn: strm ]).
! !


!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 30/Jul/2026, 7:20:32 pm (UTC)'!
propertiesDo: aBlock
	"Enumerate only the properties. Skip Pragmas."

	| element "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(element := self basicAt: i) isPragma ifFalse: [
			aBlock value: element ]].
! !

!AdditionalMethodState methodsFor: 'printing' stamp: 'jmv 30/Jul/2026, 3:21:51 pm (UTC)'!
printOn: aStream
	| isFirst |
	aStream
		nextPutAll: self class name withArticle;
		nextPutAll: '[';
		nextPutAll: ' Method: '.
	method printClassAndSelectorOn: aStream.
	aStream
		nextPutAll: '. Selector: '; print: selector;
		nextPutAll: '. Elements: {'.
	isFirst := true.
	1 to: self basicSize do: [ :i |
		isFirst ifFalse: [ aStream nextPutAll: '. ' ].
		isFirst := false.
		(self basicAt: i) printOn: aStream ].
	aStream
		nextPutAll: '}';
		nextPutAll: ']'.! !

!AdditionalMethodState methodsFor: 'printing' stamp: 'jmv 30/Jul/2026, 7:19:46 pm (UTC)'!
storePropertiesOn: aStream
	"Print as an Array."

	| isFirst |
	aStream nextPut: ${.
	isFirst := true.
	self propertiesDo: [ :assoc |
		isFirst ifTrue: [isFirst := false] ifFalse: [ aStream nextPut: $.; newLine; tab ].
		assoc storeOn: aStream ].
	aStream nextPut: $}.
! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 30/Jul/2026, 7:21:36 pm (UTC)'!
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
			withStamp: compiledMethod authorAndStamp
			libraryName: (compiledMethod propertyValueAt: #libraryName)
			priorMethod: nil.
		compiledMethod
			withPropertiesDo: [ :properties |
				self printPropertiesChunk: properties on: outStream ]
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
	outStream newLine.
! !


'From Cuis7.9 [latest update: #8139] on 20 August 2026 at 2:38:24 pm'!

!AdditionalMethodState methodsFor: 'testing' stamp: 'jmv 20/Aug/2026, 5:31:05 pm (UTC)'!
hasAnyProperty
	"Enumerate only the properties. Skip Pragmas."

	| element "<Association|Pragma>" |
	1 to: self basicSize do: [:i |
		(element := self basicAt: i) isPragma ifFalse: [
			^true ]].
	^false! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 20/Aug/2026, 5:31:20 pm (UTC)'!
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

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 20/Aug/2026, 5:35:12 pm (UTC)'!
printPropertiesChunk: anAdditionalMethodStateOrArray on: aFileStream
	"Print a method properties chunk."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm | 
		strm nextPutAll: 'methodProperties: '.
		anAdditionalMethodStateOrArray storePropertiesOn: strm ]).
! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 20/Aug/2026, 5:31:26 pm (UTC)'!
putSource: sourceStr class: class category: catName
	withStamp: changeStamp inFile: fileIndex priorMethod: priorMethod

	self putSource: sourceStr inFile: fileIndex withPreamble: [ :file |
		class
			printCategoryChunk: catName
			on: file
			withStamp: changeStamp
			priorMethod: priorMethod.
		self
			withPropertiesDo: [ :properties |
				properties hasAnyProperty ifTrue: [
					class printPropertiesChunk: properties on: file ]]
			ifSelector: [ :sel | nil ].
		file newLine ].
! !


!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 20/Aug/2026, 5:37:42 pm (UTC)'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	self changeClass printCategoryChunk: category on: aFileStream
		withStamp: stamp priorMethod: nil.
	properties isEmptyOrNil ifFalse: [
		self changeClass printPropertiesChunk: properties on: aFileStream ].
	aFileStream newLine.
	aFileStream nextChunkPut: self string.
	aFileStream nextChunkPut: ' '; newLine.
	aFileStream newLine.! !


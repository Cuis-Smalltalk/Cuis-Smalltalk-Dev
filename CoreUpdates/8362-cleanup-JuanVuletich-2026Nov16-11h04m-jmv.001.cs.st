'From Cuis7.9 [latest update: #8358] on 16 November 2026 at 11:07:35 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 16/Nov/2026, 2:06:25 pm (UTC)'!
printMethodChunkFor: compiledMethod withPreamble: doPreamble on: outStream padForPointerScale: doPad
	"Copy the source code for the method associated with selector onto the fileStream."

	| oldPos newPos sourceFile methodCode methodCodeText codeStyler |

	doPreamble ifTrue: [
		ClassDescription
			printCategoryChunk: self name
			category: (self organization categoryOfElement: compiledMethod selector)
			on: outStream
			withStamp: compiledMethod authorAndStampUTC.
		compiledMethod
			withPropertiesDo: [ :properties |
				properties hasAnyProperty ifTrue: [
					ClassDescription printPropertiesChunk: properties on: outStream ]]
			ifSelector: [ :sel | nil ].
		outStream newLine ].

	((compiledMethod fileIndex = 0
		or: [(SourceFiles at: compiledMethod fileIndex) == nil])
		or: [(oldPos := compiledMethod filePosition) = 0])
			ifTrue: [
				"The source code is not accessible.  We must decompile..."
				outStream nextChunkPut: compiledMethod decompileString.
				newPos := 0 ]
			ifFalse: [
				sourceFile := SourceFiles at: compiledMethod fileIndex.
				sourceFile position: oldPos.
				"Copy the method chunk"
				doPad ifTrue: [
					outStream padSpacesTo: SourceFiles pointerScaleForWriting ].
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
				newPos := outStream position.
				outStream nextChunkPut: methodCode ].
	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].
	outStream newLine.
	^newPos! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 16/Nov/2026, 2:07:02 pm (UTC)'!
writeMethodPreambleToChangesFile: file class: class category: catName withStamp: changeStamp

	ClassDescription
		printCategoryChunk: class name
		category: catName
		on: file
		withStamp: changeStamp.
	self
		withPropertiesDo: [ :properties |
			properties hasAnyProperty ifTrue: [
				ClassDescription printPropertiesChunk: properties on: file ]]
		ifSelector: [ :sel | nil ].
	file newLine.! !

!methodRemoval: ClassDescription #printCategoryChunk:on:withStamp: stamp: 'jmv 16/Nov/2026, 2:07:07 pm (UTC)'!
ClassDescription removeSelector: #printCategoryChunk:on:withStamp:!

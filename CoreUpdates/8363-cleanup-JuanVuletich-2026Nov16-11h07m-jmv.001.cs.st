'From Cuis7.9 [latest update: #8358] on 16 November 2026 at 11:20:24 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 16/Nov/2026, 2:14:49 pm (UTC)'!
writeMethodPreambleTo: file class: class category: catName withStamp: changeStamp

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


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 16/Nov/2026, 2:19:41 pm (UTC)'!
printMethodChunkFor: compiledMethod withPreamble: doPreamble on: outStream padForPointerScale: doPad
	"Copy the source code for the method associated with selector onto the fileStream."

	| oldPos newPos sourceFile methodCode methodCodeText codeStyler |

	doPreamble ifTrue: [
		compiledMethod
			writeMethodPreambleTo: outStream class: self
			category: (self organization categoryOfElement: compiledMethod selector)
			withStamp: compiledMethod authorAndStampUTC ].
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


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 16/Nov/2026, 2:15:20 pm (UTC)'!
writeToChangesFile: sourceStr class: class category: catName withStamp: changeStamp oldMethodOrNil: existingMethod

	"Store the source code for the receiver on the external .changes file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	Store a 4-byte source code pointer at the method end."

	| fileIndex file position libIndex |

	Smalltalk userChangesAppendDo: [ :stream |
		self writeMethodPreambleTo: stream class: class category: catName withStamp: changeStamp.
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].

	fileIndex := 2.
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		self writeMethodPreambleTo: file class: class category: catName withStamp: changeStamp.
		file padSpacesTo: SourceFiles pointerScaleForWriting.
		position := file position.
		file nextChunkPut: sourceStr asPlainString.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		
		
		libIndex := 16rFFFFFFFE . "Changes"
		CodeHistory current addMethodVersion: self libraryIndex: libIndex positionIndex: position oldVersionOrNil: existingMethod.
		
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].! !

!methodRemoval: CompiledMethod #writeMethodPreambleToChangesFile:class:category:withStamp: stamp: 'jmv 16/Nov/2026, 2:15:25 pm (UTC)'!
CompiledMethod removeSelector: #writeMethodPreambleToChangesFile:class:category:withStamp:!

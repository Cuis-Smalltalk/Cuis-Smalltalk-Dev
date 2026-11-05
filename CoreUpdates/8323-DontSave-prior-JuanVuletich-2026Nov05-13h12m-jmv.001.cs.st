'From Cuis7.9 [latest update: #8322] on 5 November 2026 at 1:17:44 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 5/Nov/2026, 4:07:59 pm (UTC)'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp

	ClassDescription
		printCategoryChunk: self name
		category: category
		on: aFileStream
		withStamp: changeStamp.! !


!ClassDescription class methodsFor: 'utilities' stamp: 'jmv 5/Nov/2026, 4:07:39 pm (UTC)'!
printCategoryChunk: className category: category on: aFileStream withStamp: changeStamp
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp.
	Always include a stamp, even if it is empty."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: className; nextPutAll: ' methodsFor: '; print: category asString.
		strm nextPutAll: ' stamp: '; print: (changeStamp ifNil: ['']) ]).! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 5/Nov/2026, 4:08:34 pm (UTC)'!
putSource: sourceStr class: class category: catName withStamp: changeStamp inFile: fileIndex

	self putSource: sourceStr inFile: fileIndex withPreamble: [ :file |
		class
			printCategoryChunk: catName
			on: file
			withStamp: changeStamp.
		self
			withPropertiesDo: [ :properties |
				properties hasAnyProperty ifTrue: [
					class printPropertiesChunk: properties on: file ]]
			ifSelector: [ :sel | nil ].
		file newLine ].! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 5/Nov/2026, 4:08:45 pm (UTC)'!
compileProperties: aString for: method
	"Keep any Pragmas, but use only newly specified Properties"

	(Compiler evaluate: aString) ifNotNil: [ :props | 
		props isArray ifTrue: [ | priorMethod |
			priorMethod := method shallowCopy.
			"method will have a new fresh instance of AdditionalMethodState,
			not shared with priorMethod at all. making priorMethod a #shallowCopy is therefore OK, "
			method methodProperties: props.
			"Save to Changes and User Changes files."
			method
				putSource: method sourceCode
				class: method methodClass
				category: (method methodClass organization categoryOfElement: method selector)
				withStamp: method authorAndStampAsSaved
				inFile: 2.
			SystemChangeNotifier uniqueInstance
				methodChangedFrom: priorMethod
				to: method
				selector: method selector
				inClass: method methodClass.
			^ true ]].
	^false! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 5/Nov/2026, 4:08:13 pm (UTC)'!
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
			withStamp: compiledMethod authorAndStampUTC.
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
					compiledMethod setSourcePosition: newPos inFile: fileIndex.
					SourceFiles checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex ]].
	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].
	outStream newLine.! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 5/Nov/2026, 4:09:42 pm (UTC)'!
reformatMethodAt: selector
	| newCodeString method | 
	newCodeString := self compilerClass new
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method := self compiledMethodAt: selector.
	method
		putSource: newCodeString
		class: self
		category: (self organization categoryOfElement: selector)
		withStamp: Utilities changeStampOrUnknown
		inFile: 2.! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 4:09:23 pm (UTC)'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp

	aCompiledMethodWithNode method
		putSource: code
		class: self
		category: category
		withStamp: changeStamp
		inFile: 2.! !


!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 5/Nov/2026, 4:10:35 pm (UTC)'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	| cName |
	cName := self changeClassName.
	self isMetaClassChange ifTrue: [
		cName := cName, ' class' ].
	ClassDescription
		printCategoryChunk: cName category: category on: aFileStream withStamp: stamp.
	properties isEmptyOrNil ifFalse: [
		ClassDescription printPropertiesChunk: properties on: aFileStream ].
	aFileStream newLine.
	aFileStream nextChunkPut: self string.
	aFileStream nextChunkPut: ' '; newLine.
	aFileStream newLine.! !

!methodRemoval: CompiledMethod #putSource:class:category:withStamp:inFile:priorMethod: stamp: 'jmv 5/Nov/2026, 4:16:48 pm (UTC)'!
CompiledMethod removeSelector: #putSource:class:category:withStamp:inFile:priorMethod:!
!methodRemoval: ClassDescription class #printCategoryChunk:category:on:withStamp:priorMethod: stamp: 'jmv 5/Nov/2026, 4:17:17 pm (UTC)'!
ClassDescription class removeSelector: #printCategoryChunk:category:on:withStamp:priorMethod:!
!methodRemoval: ClassDescription #printCategoryChunk:on:withStamp:priorMethod: stamp: 'jmv 5/Nov/2026, 4:16:53 pm (UTC)'!
ClassDescription removeSelector: #printCategoryChunk:on:withStamp:priorMethod:!

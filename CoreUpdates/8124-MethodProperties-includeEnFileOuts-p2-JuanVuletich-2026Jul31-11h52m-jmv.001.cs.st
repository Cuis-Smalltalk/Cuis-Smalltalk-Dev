'From Cuis7.9 [latest update: #8120] on 31 July 2026 at 11:56:23 am'!

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 30/Jul/2026, 8:19:37 pm (UTC)'!
compile: text classified: category withStamp: changeStamp methodProperties: aCollectionOrNil notifying: requestor
	^ self compile: text classified: category withStamp: changeStamp methodProperties: aCollectionOrNil notifying: requestor logSource: self acceptsLoggingOfCompilation! !

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 31/Jul/2026, 2:38:05 pm (UTC)'!
compile: code classified: category withStamp: changeStamp methodProperties: aCollectionOrNil notifying: requestor logSource: logSource
	
	| methodAndNode selector newMethod isExistingMethod updatedCode |
	methodAndNode := self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector := methodAndNode selector.
	newMethod := methodAndNode method.
	isExistingMethod := self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].

	"Any Pragmas were just added when compiling code. Add method properties now."
	aCollectionOrNil
		ifNotNil: [ aCollectionOrNil do: [ :assoc | newMethod addPragmaOrProperty: assoc]]
		ifNil: [
			"If method properties are not provided, then keep those of the existing method, if any."
			isExistingMethod ifTrue: [
				(self compiledMethodAt: selector) withPropertiesDo: [ :additionalMethodState |
					additionalMethodState propertiesDo: [ :assoc | newMethod addPragmaOrProperty: assoc ]] ifSelector: [ :s | ]]].

	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode := CodeProvider normalizeCodeForSaving: (requestor ifNil: [code] ifNotNil: [requestor text]) in: self.
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: newMethod
		inCategory: category methodTimeStamp: changeStamp.
	^ selector! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 31/Jul/2026, 2:32:14 pm (UTC)'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	Always include a stamp, even if it is empty."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		strm nextPutAll: ' stamp: '; print: (changeStamp ifNil: ['']).
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		]).! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 31/Jul/2026, 2:37:08 pm (UTC)'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil |

	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.

	aCompiledMethodWithNode method
		putSource: code
		fromParseNode: aCompiledMethodWithNode node
		class: self
		category: category
		withStamp: changeStamp
		inFile: 2
		priorMethod: priorMethodOrNil.! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 31/Jul/2026, 2:36:36 pm (UTC)'!
putSource: sourceStr fromParseNode: methodNode class: class category: catName
	withStamp: changeStamp inFile: fileIndex priorMethod: priorMethod

	self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |
		class
			printCategoryChunk: catName
			on: file
			withStamp: changeStamp
			priorMethod: priorMethod.
		self
			withPropertiesDo: [ :properties |
				class printPropertiesChunk: properties on: file ]
			ifSelector: [ :sel | nil ].
		file newLine ].
! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 31/Jul/2026, 2:37:43 pm (UTC)'!
compile: code classified: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor logSource: logSource
	
	| methodAndNode selector newMethod isExistingMethod updatedCode libName |
	methodAndNode := self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector := methodAndNode selector.
	newMethod := methodAndNode method.
	libName := aStringOrNil.
	isExistingMethod := self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed'].
			"If libraryName is provided, it takes precedence. If not, assume it doesn't change."
			libName isNil ifTrue: [ libName := (self compiledMethodAt: selector) propertyValueAt: #libraryName ]]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	libName ifNotNil: [ newMethod propertyValueAt: #libraryName put: libName ].
	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode := CodeProvider normalizeCodeForSaving: (requestor ifNil: [code] ifNotNil: [requestor text]) in: self.
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: newMethod
		inCategory: category methodTimeStamp: changeStamp.
	^ selector! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 31/Jul/2026, 2:32:48 pm (UTC)'!
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

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 31/Jul/2026, 2:38:27 pm (UTC)'!
reformatMethodAt: selector
	| newCodeString method | 
	newCodeString := self compilerClass new
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method := self compiledMethodAt: selector.
	method
		putSource: newCodeString
		fromParseNode: nil
		class: self
		category: (self organization categoryOfElement: selector)
		withStamp: Utilities changeStampOrUnknown
		inFile: 2
		priorMethod: method.
! !

!methodRemoval: CompiledMethod #putSource:fromParseNode:class:category:withStamp:libraryName:inFile:priorMethod: stamp: 'jmv 31/Jul/2026, 2:54:54 pm (UTC)'!
CompiledMethod removeSelector: #putSource:fromParseNode:class:category:withStamp:libraryName:inFile:priorMethod:!
!methodRemoval: ClassDescription #logMethodSource:forMethodWithNode:inCategory:withStamp:libraryName:notifying: stamp: 'jmv 31/Jul/2026, 2:54:27 pm (UTC)'!
ClassDescription removeSelector: #logMethodSource:forMethodWithNode:inCategory:withStamp:libraryName:notifying:!

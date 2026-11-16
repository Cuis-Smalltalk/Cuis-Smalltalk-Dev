'From Cuis7.9 [latest update: #8345] on 12 November 2026 at 5:31:23 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 12/Nov/2026, 8:01:54 pm (UTC)'!
writeToChangesFile: sourceStr class: class category: catName withStamp: changeStamp

	self writeToChangesFile: sourceStr withPreamble: [ :file |
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

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 12/Nov/2026, 8:01:09 pm (UTC)'!
writeToChangesFile: sourceStr withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| fileIndex file position  |
	fileIndex := 2.
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
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
		self setSourcePosition: position inFile: fileIndex.
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].! !


!Behavior methodsFor: 'compiling' stamp: 'jmv 12/Nov/2026, 8:01:29 pm (UTC)'!
compile: code notifying: requestor 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary. The 
	second argument, requestor, is to be notified if an error occurs. The 
	argument code is either a string or an object that converts to a string or 
	a PositionableStream. This method also saves the source code."
	
	| methodAndNode sel compiled updatedCode |
	methodAndNode := self
		basicCompile: code
		notifying: requestor
		trailer: self defaultMethodTrailer
		ifFail: [^nil].
	sel := methodAndNode selector.
	compiled := methodAndNode method.
	(self includesSelector: sel) ifTrue: [
		((self compiledMethodAt: sel) propertyValueAt: #libraryName) ifNotNil: [ :libName |
			compiled propertyValueAt: #libraryName put: libName ]].
	"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
	updatedCode := CodeProvider normalizeCodeForSaving: (requestor ifNil: [code] ifNotNil: [requestor text]) in: self.
	compiled writeToChangesFile: updatedCode withPreamble: [ :f |
		f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: sel withMethod: compiled.
	^ methodAndNode selector! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 12/Nov/2026, 8:02:14 pm (UTC)'!
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
				writeToChangesFile: method sourceCode
				class: method methodClass
				category: (method methodClass organization categoryOfElement: method selector)
				withStamp: method authorAndStampAsSaved.
			SystemChangeNotifier uniqueInstance
				methodChangedFrom: priorMethod
				to: method
				selector: method selector
				inClass: method methodClass.
			^ true ]].
	^false! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 8:02:20 pm (UTC)'!
reformatMethodAt: selector
	| priorMethod newCodeString method |
	priorMethod := (self compiledMethodAt: selector) shallowCopy.
	newCodeString := self compilerClass new
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method := self compiledMethodAt: selector.
	method
		writeToChangesFile: newCodeString
		class: self
		category: (self organization categoryOfElement: selector)
		withStamp: Utilities changeStampOrUnknown.
	SystemChangeNotifier uniqueInstance
		methodChangedFrom: priorMethod
		to: method
		selector: method selector
		inClass: method methodClass.! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 12/Nov/2026, 8:02:17 pm (UTC)'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp

	aCompiledMethodWithNode method
		writeToChangesFile: code
		class: self
		category: category
		withStamp: changeStamp! !

!methodRemoval: CompiledMethod #putSource:inFile:withPreamble: stamp: 'jmv 12/Nov/2026, 8:31:12 pm (UTC)'!
CompiledMethod removeSelector: #putSource:inFile:withPreamble:!
!methodRemoval: CompiledMethod #putSource:class:category:withStamp:inFile: stamp: 'jmv 12/Nov/2026, 8:31:11 pm (UTC)'!
CompiledMethod removeSelector: #putSource:class:category:withStamp:inFile:!

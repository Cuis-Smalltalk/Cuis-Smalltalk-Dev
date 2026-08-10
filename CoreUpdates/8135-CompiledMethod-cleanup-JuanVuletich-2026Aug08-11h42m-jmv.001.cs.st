'From Cuis7.9 [latest update: #8134] on 8 August 2026 at 11:47:29 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 8/Aug/2026, 2:46:34 pm (UTC)'!
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
				class printPropertiesChunk: properties on: file ]
			ifSelector: [ :sel | nil ].
		file newLine ].
! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 8/Aug/2026, 2:44:52 pm (UTC)'!
putSource: sourceStr inFile: fileIndex withPreamble: preambleBlock
	"Store the source code for the receiver on an external file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	If the fileIndex is 1, print on *.sources;  if it is 2, print on *.changes,
	in each case, storing a 4-byte source code pointer at the method end."

	| file remoteString  |
	(SourceFiles notNil and: [(file := SourceFiles at: fileIndex) notNil]) ifTrue: [
		file setToEnd.
		preambleBlock value: file.  "Write the preamble"
		remoteString := RemoteString newString: sourceStr onFileNumber: fileIndex toFile: file.
		file nextChunkPut: ' '.
		"If we are doing a FileIn, don't flush for each method.
		This is a performance optimization, to make file in of large chunks of code faster.
		It means that the source code and the time stamp (and rest of preamble) won't
		be available for reading until the file in finishes.
		This is usually not a problem."
		InMidstOfFileinNotification signal ifFalse: [file flush].
		self checkOKToAdd: sourceStr size at: remoteString position in: fileIndex.
		self setSourcePosition: remoteString position inFile: fileIndex ].

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].! !


!Behavior methodsFor: 'compiling' stamp: 'jmv 8/Aug/2026, 2:45:23 pm (UTC)'!
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
	compiled putSource: updatedCode inFile: 2 withPreamble: [ :f |
		f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: sel withMethod: compiled.
	^ methodAndNode selector! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/Aug/2026, 2:47:11 pm (UTC)'!
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
		inFile: 2
		priorMethod: method.
! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 8/Aug/2026, 2:47:00 pm (UTC)'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil |

	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.

	aCompiledMethodWithNode method
		putSource: code
		class: self
		category: category
		withStamp: changeStamp
		inFile: 2
		priorMethod: priorMethodOrNil.
! !

!methodRemoval: CompiledMethod #putSource:fromParseNode:class:category:withStamp:inFile:priorMethod: stamp: 'jmv 8/Aug/2026, 2:47:21 pm (UTC)'!
CompiledMethod removeSelector: #putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:!
!methodRemoval: CompiledMethod #putSource:fromParseNode:inFile:withPreamble: stamp: 'jmv 8/Aug/2026, 2:46:11 pm (UTC)'!
CompiledMethod removeSelector: #putSource:fromParseNode:inFile:withPreamble:!

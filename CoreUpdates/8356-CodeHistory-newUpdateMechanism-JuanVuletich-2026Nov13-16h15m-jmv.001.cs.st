'From Cuis7.9 [latest update: #8355] on 13 November 2026 at 4:22:24 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 13/Nov/2026, 6:59:32 pm (UTC)'!
writeToChangesFile: sourceStr class: class category: catName withStamp: changeStamp oldMethodOrNil: existingMethod

	self
		writeToChangesFile: sourceStr
		withPreamble: [ :file |
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp.
			self
				withPropertiesDo: [ :properties |
					properties hasAnyProperty ifTrue: [
						class printPropertiesChunk: properties on: file ]]
				ifSelector: [ :sel | nil ].
			file newLine ]
		oldMethodOrNil: existingMethod.! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 13/Nov/2026, 7:20:26 pm (UTC)'!
writeToChangesFile: sourceStr withPreamble: preambleBlock oldMethodOrNil: existingMethod
	"Store the source code for the receiver on the external .changes file.
	If no sources are available, i.e., SourceFile is nil, do nothing.
	Store a 4-byte source code pointer at the method end."

	| fileIndex file position  |

	Smalltalk userChangesAppendDo: [ :stream |
		preambleBlock value: stream.  "Write the preamble"
		stream nextChunkPut: sourceStr.
		stream nextChunkPut: ' ' ].

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
		
		self historyPointer: (CodeHistory historyPointerFromFileIndex: fileIndex andPosition: position).
		"CodeHistory current addMethodVersion: self oldVersionOrNil: existingMethod."
		
		SourceFiles checkOKToAdd: sourceStr size at: position in: fileIndex ].! !


!CodeHistory methodsFor: 'add / update' stamp: 'jmv 13/Nov/2026, 6:45:02 pm (UTC)'!
addMethodRemoval: oldMethod
	"oldMethod is about to be removed. Add an entry to the CodeHistory."

	| newItem prevItem libIndex posIndex |
	
	"Add en entry in CodeHistory for the removal"
	libIndex := 0.
	posIndex := 0.
	prevItem := oldMethod historyPointer.
	newItem := self addLibraryIndex: libIndex positionIndex: posIndex prevItem: prevItem nextItem: 0.

	"If oldMethod was already in CodeHistory, update its previous entry"
	(prevItem > 0 and: [
	prevItem < 16rFC000000]) ifTrue: [
		self at: prevItem updateNextItem: newItem ].

	"Store the new item in removals.
	This will let us keep the chain of items in case the method is added again."
	removals at: oldMethod asMethodReference put: newItem.! !

!CodeHistory methodsFor: 'add / update' stamp: 'jmv 13/Nov/2026, 6:43:31 pm (UTC)'!
addMethodVersion: newMethod oldVersionOrNil: oldMethod
	"newMethod may be
	- entirely new (oldMethod is nil)
	- replacement for an existing method: oldMethod
	- re-addition of a previously existing and removed method (oldMethod is nil)
	Note: newMethod is still not installed, the old method, if existing is still installed"

	| newItem prevItem fileIndex posIndex libIndex |
	
	"If re-added, take the entry for the removal. We don't have the old method, but its entry
	in CodeHistory was stored in removals. Retrieve it and remove it from removals."
	prevItem := oldMethod
		ifNotNil: [ oldMethod historyPointer ]
		ifNil: [ removals removeKey: newMethod asMethodReference ifAbsent: [0] ].
	
	"Point to changes file / position"
	libIndex := 0.
	fileIndex := newMethod fileIndex.
	posIndex := newMethod filePosition.
	fileIndex = 1 ifTrue: [ libIndex := 16rFFFFFFFF ]. "Sources"
	fileIndex = 2 ifTrue: [ libIndex := 16rFFFFFFFE ]. "Changes"
	
	"Add new entry to CodeHistory"
	newItem := self addLibraryIndex: libIndex positionIndex: posIndex prevItem: prevItem nextItem: 0.

	"If method already in CodeHistory, update its previous entry to point to new entry"
	(prevItem > 0 and: [
	prevItem < 16rFC000000]) ifTrue: [
		self at: prevItem updateNextItem: newItem ].

	"Make method point to the entry just added"
	newMethod historyPointer: newItem.! !


!Behavior methodsFor: 'compiling' stamp: 'jmv 13/Nov/2026, 6:59:07 pm (UTC)'!
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
	compiled
		writeToChangesFile: updatedCode
		withPreamble: [ :f | f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine]
		oldMethodOrNil: nil.
	self addSelectorSilently: sel withMethod: compiled.
	^ methodAndNode selector! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 13/Nov/2026, 7:20:44 pm (UTC)'!
compile: code classified: category withStamp: changeStamp methodProperties: aCollectionOrNil notifying: requestor logSource: logSource
	
	| methodAndNode selector newMethod priorMethod updatedCode |
	methodAndNode := self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector := methodAndNode selector.
	newMethod := methodAndNode method.
	priorMethod := self compiledMethodAt: selector ifAbsent: [].
	priorMethod notNil
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
			priorMethod ifNotNil: [
				priorMethod withPropertiesDo: [ :additionalMethodState |
					additionalMethodState propertiesDo: [ :assoc | newMethod addPragmaOrProperty: assoc ]] ifSelector: [ :s | ]]].

	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode := requestor ifNil: [code] ifNotNil: [requestor text].
		updatedCode := CodeProvider normalizeCodeForSaving: updatedCode in: self.
		methodAndNode method
			writeToChangesFile: updatedCode class: self category: category withStamp: changeStamp oldMethodOrNil: priorMethod ].
	self addAndClassifySelector: selector withMethod: newMethod inCategory: category methodTimeStamp: changeStamp.
	^ selector! !

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 13/Nov/2026, 6:57:16 pm (UTC)'!
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
				withStamp: method authorAndStampAsSaved
				oldMethodOrNil: priorMethod.
			SystemChangeNotifier uniqueInstance
				methodChangedFrom: priorMethod
				to: method
				selector: method selector
				inClass: method methodClass.
			^ true ]].
	^false! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 13/Nov/2026, 6:56:50 pm (UTC)'!
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
		withStamp: Utilities changeStampOrUnknown
		oldMethodOrNil: priorMethod.
	SystemChangeNotifier uniqueInstance
		methodChangedFrom: priorMethod
		to: method
		selector: method selector
		inClass: method methodClass.! !

!methodRemoval: CompiledMethod #writeToChangesFile:withPreamble: stamp: 'jmv 13/Nov/2026, 7:21:03 pm (UTC)'!
CompiledMethod removeSelector: #writeToChangesFile:withPreamble:!
!methodRemoval: CompiledMethod #writeToChangesFile:class:category:withStamp: stamp: 'jmv 13/Nov/2026, 7:21:02 pm (UTC)'!
CompiledMethod removeSelector: #writeToChangesFile:class:category:withStamp:!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

SystemChangeNotifier uniqueInstance removeActionsWithReceiver: CodeHistory current.!


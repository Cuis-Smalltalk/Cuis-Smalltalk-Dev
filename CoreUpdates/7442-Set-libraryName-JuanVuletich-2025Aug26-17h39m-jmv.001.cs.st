'From Cuis7.5 [latest update: #7441] on 26 August 2025 at 5:47:06 pm'!

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 8/26/2025 17:40:50'!
addAndClassifySelector: selector withMethod: compiledMethod inCategory: category methodTimeStamp: aTimeStampString notifying: requestor
	| priorMethodOrNil priorCategoryOrNil |
	priorMethodOrNil := self compiledMethodAt: selector ifAbsent: nil.
	priorCategoryOrNil := self whichCategoryIncludesSelector: selector.
	self addSelectorSilently: selector withMethod: compiledMethod.
	SystemChangeNotifier uniqueInstance doSilently: [self organization classify: selector under: category].
	priorMethodOrNil
		ifNil: [
			SystemChangeNotifier uniqueInstance 
				methodAdded: compiledMethod
				selector: selector
				inProtocol: category
				class: self
				methodTimeStamp: aTimeStampString
				requestor: requestor ]
		ifNotNil: [
			SystemChangeNotifier uniqueInstance 
				methodChangedFrom: priorMethodOrNil 
				to: compiledMethod 
				selector: selector 
				inClass: self 
				requestor: requestor.
			category = priorCategoryOrNil ifFalse: [
				SystemChangeNotifier uniqueInstance
					selectorRecategorized: selector
					from: priorCategoryOrNil
					to: category
					inClass: self ]].! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/26/2025 17:31:03'!
classDefinitionRecordFrom: tokens
	| classDefinition isMeta itemPosition className record fullClassName category stamp |
	
	itemPosition := file position.
	fullClassName := tokens second.
	isMeta := fullClassName includesSubString: ' class'.
	className := isMeta ifTrue: [fullClassName substrings first] ifFalse: [fullClassName].
	category := CompiledMethod classCategoryFrom: tokens.
	stamp := CompiledMethod stampFrom: tokens.
	classDefinition := file nextChunk.
	
	record := ChangeRecord new
		file: file
		position: itemPosition
		type: #classDefinition
		class: className asSymbol
		category: category
		meta: isMeta
		stamp: stamp.
	
	^record
! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 8/26/2025 17:28:58'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position stamp priorMethodRef prevPos prevFileIndex libraryName preamble tokens sourceFilesCopy |
	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex := 0.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x ifNotNil: [ x name asFullFileEntry readStream ]].
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		"Skip back filler"
		file position: position.
		file backChunk.
		"Get preamble"
		preamble := file backChunk.
		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevFileIndex := nil.
		prevPos := nil.
		stamp := ''.
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				stamp := CompiledMethod stampFrom: tokens.
				priorMethodRef := CompiledMethod priorReferenceFrom: tokens.
				priorMethodRef ifNotNil: [
					prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: priorMethodRef.
					prevPos := sourceFilesCopy filePositionFromSourcePointer: priorMethodRef ].
				libraryName := CompiledMethod libraryNameFrom: tokens ].
 		self addItem:
				(ChangeRecord new
					file: file position: position type: #method
					class: class name category: category meta: meta stamp: stamp 
					prior: priorMethodRef libraryName: libraryName)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position := prevPos.
		prevPos notNil ifTrue: [
			file := sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!Behavior methodsFor: 'compiling' stamp: 'jmv 8/26/2025 16:41:14'!
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
	compiled putSource: updatedCode fromParseNode: methodAndNode node inFile: 2
			withPreamble: [ :f | f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine].
	self addSelectorSilently: sel withMethod: compiled.
	^ methodAndNode selector! !

!Behavior methodsFor: 'compiling' stamp: 'jmv 8/26/2025 16:36:58'!
recompile: selector from: oldClass
	"Compile the method associated with selector in the receiver's method dictionary."
	"ar 7/10/1999: Use oldClass compiledMethodAt: not self compiledMethodAt:"
	| method trailer methodNode compiled |
	method := oldClass compiledMethodAt: selector.
	trailer := method trailer.
	methodNode := self compilerClass new
				compile: (oldClass sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [^ self].   "Assume OK after proceed from SyntaxError"
	selector == methodNode selector ifFalse: [self error: 'selector changed!!'].
	compiled := methodNode generate: trailer.
	(method propertyValueAt: #libraryName) ifNotNil: [ :libName |
		compiled propertyValueAt: #libraryName put: libName ].
	self addSelectorSilently: selector withMethod: compiled.
! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 8/26/2025 17:32:13'!
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
			inCategory: category withStamp: changeStamp libraryName: libName
			notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: newMethod
		inCategory: category methodTimeStamp: changeStamp notifying: requestor.
	^ selector! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/26/2025 16:01:40'!
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
		withStamp: Utilities changeStamp
		libraryName: (method propertyValueAt: #libraryName)
		inFile: 2
		priorMethod: method.! !


!Decompiler class methodsFor: 'testing' stamp: 'jmv 8/26/2025 16:35:37'!
recompileAllTest
	"[Decompiler recompileAllTest]"
	"decompile every method and compile it back; if the decompiler is correct then the system should keep running.  :)"
	
	Smalltalk allBehaviorsDo: [ :behavior |
		Utilities informUser: (behavior printString) during: [
			behavior selectors do: [ :sel |
				| existingMethod decompiled ast compiled |
				decompiled := Decompiler new decompile: sel in: behavior.
				ast := Compiler new compile: decompiled in: behavior notifying: nil ifFail: [ self error: 'failed' ].
				existingMethod := behavior compiledMethodAt: sel.
				compiled := ast generate: existingMethod trailer.
				(existingMethod propertyValueAt: #libraryName) ifNotNil: [ :libName |
					compiled propertyValueAt: #libraryName put: libName ].
				behavior addSelectorSilently: sel withMethod: compiled. ] ] ]! !

!methodRemoval: ClassDescription #addAndClassifySelector:withMethod:inProtocol:methodTimeStamp:notifying: stamp: 'jmv 8/26/2025 17:41:20'!
ClassDescription removeSelector: #addAndClassifySelector:withMethod:inProtocol:methodTimeStamp:notifying:!

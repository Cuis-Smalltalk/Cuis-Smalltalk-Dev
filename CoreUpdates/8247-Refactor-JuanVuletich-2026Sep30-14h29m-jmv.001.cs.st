'From Cuis7.9 [latest update: #8247] on 30 September 2026 at 2:53:30 pm'!

!ChangeList class methodsFor: 'services' stamp: 'jmv 30/Sep/2026, 5:44:10 pm (UTC)'!
methodSourceAndMetadataFor: historyPointer into: aBlock
	"Evaluate a block with information for method stored at historyPointer (that points to source code)
		- method source code
		- method stamp string as saved (nil if not present)
		- method properties (nil if not present)
		- previousHistoryPointer (0 if not present)
	
	ChangeList methodSourceAndMetadataFor: (Object >> #=) historyPointer
		into: [ :methodSource :stamp :methodProperties :prev |
			'-----' print.
			methodSource print.
			stamp print.
			methodProperties print.
			prev print ]
	"

	| fileIndex position preamble tokens methodSource methodPropertiesString stampString prevHistoryPointer |
	
	methodPropertiesString := ''.
	stampString := nil.
	prevHistoryPointer := 0.
	fileIndex := SourceFiles fileIndexFromHistoryPointer: historyPointer.
	position := SourceFiles filePositionFromHistoryPointer: historyPointer.
	
	 (SourceFiles at: fileIndex) name asFullFileEntry readStreamDo: [ :sourceFile |
		"Read source"
		sourceFile position: position.
		methodSource := sourceFile nextChunk withCuisLineEndings.
		"Point to source. Find Preamble"
		sourceFile position: position.
		sourceFile backChunk. "Skip back blank space filler."
		"Get preamble (and Method Properties if present)"
		preamble := sourceFile backChunk.
		(preamble beginsWith: 'methodProperties:') ifTrue: [
			methodPropertiesString := (preamble copyFrom: 1 + 'methodProperties:' size to: preamble size).
			sourceFile backChunk. "Skip back blank space."
			preamble := sourceFile backChunk ]].

	"Preamble is likely a linked method preamble, if we're in
		a changes file (not the sources file).  Try to parse it
		for prior source position and file index"
	(preamble includesSubString: 'methodsFor:')
		ifTrue: [
			tokens := Scanner new scanTokens: preamble.
			stampString := CompiledMethod stampFrom: tokens.
			prevHistoryPointer := (CompiledMethod priorReferenceFrom: tokens) ifNil: [ 0 ].
			prevHistoryPointer = 0 ifFalse: [ prevHistoryPointer := prevHistoryPointer + 16rFC000000 - 16r1000000 ]].

	aBlock value: methodSource value: stampString value: methodPropertiesString value: prevHistoryPointer.! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 30/Sep/2026, 5:44:21 pm (UTC)'!
authorAndStampAsSaved
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStampLocalTZ
	(Object compiledMethodAt: #printString) authorAndStampUTC
	"

	ChangeList
		methodSourceAndMetadataFor: self historyPointer
		into: [ :methodSource :stampString :methodPropertiesString :prevHistoryPointer |
			^stampString ].! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Sep/2026, 5:31:43 pm (UTC)'!
scanCategory: category class: class meta: meta stamp: stamp
	"Read methods following a method category preamble in a chunk file.
	See #methods, #methodsFor:*, scanCategory:*"

	| methodProperties libNameOrNil itemPosition methodSource selector |
	[
		methodProperties := nil.
		libNameOrNil := nil.
		file skipSeparators.
		(file peekForString: '!!methodProperties:') ifTrue: [
			methodProperties := Compiler evaluate: file nextChunk.
				methodProperties do: [ :assoc |
					assoc key == #libraryName ifTrue: [ libNameOrNil := assoc value ]]].
		file skipSeparators.
		itemPosition := file position.
		methodSource := file nextChunk withCuisLineEndings.
		methodSource = '' ifTrue: [ ^self ]. "done when double terminators"
		selector := ((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: methodSource) ifNil: [''].
		self
			addItem: (MethodDefinitionChange new
				class: class category: category meta: meta stamp: stamp
				properties: methodProperties
				selector: selector
				string: methodSource)
			text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
				, selector
				, (stamp isEmpty ifTrue: [''] ifFalse: [' - ' , stamp])
				, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodProperties printString, '] '])
	] repeat.! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 30/Sep/2026, 5:53:10 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| methodProperties historyPointer |

	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.

	historyPointer := method historyPointer.
	[ historyPointer = 0 ] whileFalse: [
		ChangeList
			methodSourceAndMetadataFor: historyPointer
			into: [ :methodSource :stampString :methodPropertiesString :prevHistoryPointer |
				methodProperties := Compiler evaluate: methodPropertiesString.
				self addItem:
						(MethodDefinitionChange new
							class: class name category: category meta: meta stamp: stampString
							properties: methodProperties
							selector: selectorOfMethod
							string: methodSource)
					text: stampString , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
							, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodPropertiesString, '] ']).
				historyPointer := prevHistoryPointer ]].
! !

!methodRemoval: CompiledMethod #getPreamble stamp: 'jmv 30/Sep/2026, 5:39:37 pm (UTC)'!
CompiledMethod removeSelector: #getPreamble!

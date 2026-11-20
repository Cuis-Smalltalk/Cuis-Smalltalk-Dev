'From Cuis7.9 [latest update: #8394] on 20 November 2026 at 4:38:00 pm'!

!CodeHistory methodsFor: 'services' stamp: 'jmv 20/Nov/2026, 6:11:04 pm (UTC)'!
indexAndPositionFrom: methodHistoryPointer intoSourcesFiles: methodDefInSourcesFilesBlock
	methodDefInLibrary: methodDefInLibraryBlock methodRemoval: methodRemovalBlock

	| libraryIndex fileIndex position |
	methodHistoryPointer < 16rFC000000
		ifTrue: [ "historyPointer is an entry in CodeHistory"
			libraryIndex := self libraryIndexAt: methodHistoryPointer.
			position := self positionIndexAt: methodHistoryPointer.
			libraryIndex < 16rFFFFFFFE
				ifTrue: [
					position = 16rFFFFFFFF
						ifTrue: [ ^methodRemovalBlock value ]
						ifFalse: [ ^methodDefInLibraryBlock value: libraryIndex value: position ]]

				ifFalse: [ 	"CodeHistory entry points to .sources or .changes"
					fileIndex := 1. "16rFFFFFFFF : Sources"
					"Maybe at some point no CompiledMethod will point to .changes. Only CodeHistory and .sources"
					libraryIndex = 16rFFFFFFFE ifTrue: [ fileIndex := 2 ]. "Changes" ]]

		ifFalse: [ "historyPointer encodes (.sources or .changes) + position"
			fileIndex := CodeHistory fileIndexFromHistoryPointer: methodHistoryPointer.
			position := CodeHistory filePositionFromHistoryPointer: methodHistoryPointer ].

	^methodDefInSourcesFilesBlock value: fileIndex value: position! !

!CodeHistory methodsFor: 'services' stamp: 'jmv 20/Nov/2026, 6:55:33 pm (UTC)'!
methodSourceAndMetadataFor: methodHistoryPointer intoMethodDefinition: methodDefinitionBlock methodRemoval: methodRemovalBlock priorReferenceInto: priorRefBlock
	"Always evaluate just either methodDefinitionBlock or methodRemovalBlock. Never both, never none.
	Evaluate priorRefBlock only if there is a prior version reference in the preable. (Note: these will most likely be completely removed in Cuis 8.0)"

	| preamble methodPropertiesString methodSource tokens authorAndStampAsSaved priorHistoryPointer |
	methodSource := nil.
	methodPropertiesString := ''.
	authorAndStampAsSaved := ''.
	
	methodHistoryPointer = 0 ifTrue: [
		"historyPointer holds no information. It is still a method definition."
		^methodDefinitionBlock value: methodSource value: authorAndStampAsSaved value: methodPropertiesString ].
	
	preamble := nil.
	self
		indexAndPositionFrom: methodHistoryPointer
		intoSourcesFiles: [ :fileIndex :position |
			 (SourceFiles at: fileIndex) name asFullFileEntry readStreamDo: [ :sourceStream |
				CodeHistory methodPreamblePropertiesAndSourceFrom: sourceStream at: position into: [ :pr :ps :s |
					preamble := pr. methodPropertiesString := ps. methodSource := s ]]]
		methodDefInLibrary: [ :libraryIndex :position |
			 'code string from library index (en realidad byes utf8!!)' readStreamDo: [ :sourceStream | "stream string sobre utf8 bytes?"
				CodeHistory methodPreamblePropertiesAndSourceFrom: sourceStream at: position into: [ :pr :ps :s |
					preamble := pr. methodPropertiesString := ps. methodSource := s ]]]
		methodRemoval: [
			methodRemovalBlock value.
			^self ].

	tokens := nil.
	(preamble notNil and: [ preamble includesSubString: 'methodsFor:']) ifTrue: [
		tokens := Scanner new scanTokens: preamble.
		authorAndStampAsSaved := CompiledMethod stampFrom: tokens ].
	methodDefinitionBlock value: methodSource value: authorAndStampAsSaved value: methodPropertiesString.

	tokens ifNotNil: [
		priorHistoryPointer := (CompiledMethod priorReferenceFrom: tokens) ifNil: [ 0 ].
		priorHistoryPointer = 0 ifFalse: [
			priorHistoryPointer := priorHistoryPointer + 16rFC000000 - 16r1000000.
			priorRefBlock value: priorHistoryPointer ]].! !

!CodeHistory methodsFor: 'services' stamp: 'jmv 20/Nov/2026, 7:15:38 pm (UTC)'!
previousAndNextFrom: methodHistoryPointer into: aBlock
	"Previous and next are zero if not defined."

	| prevItem nextItem |
	prevItem := 0.
	nextItem := 0.
	(methodHistoryPointer < 16rFC000000 and: [ methodHistoryPointer ~= 0])
		ifTrue: [
			"historyPointer is an entry in CodeHistory"
			prevItem := self prevItemIndexAt: methodHistoryPointer.
			nextItem := self nextItemIndexAt: methodHistoryPointer ].
	aBlock value:  prevItem value: nextItem.! !


!CodeHistory class methodsFor: 'services' stamp: 'jmv 20/Nov/2026, 12:54:55 am (UTC)'!
methodPreamblePropertiesAndSourceFrom: sourceStream at: position into: aBlock

	| preamble methodPropertiesString methodSource |

	"Read source"
	sourceStream position: position.
	methodSource := sourceStream nextChunk withCuisLineEndings.
	
	"Point to source. Find Preamble"
	sourceStream position: position.
	sourceStream backChunk. "Skip back blank space filler."

	"Get preamble (and Method Properties if present)"
	methodPropertiesString := ''.
	preamble := sourceStream backChunk.
	(preamble beginsWith: 'methodProperties:') ifTrue: [
		methodPropertiesString := (preamble copyFrom: 1 + 'methodProperties:' size to: preamble size).
		sourceStream backChunk. "Skip back blank space."
		preamble := sourceStream backChunk ].
	
	aBlock value: preamble value: methodPropertiesString value: methodSource.! !


!Parser class methodsFor: 'accessing' stamp: 'SqR 25/Aug/2014, 1:06:00 pm (UTC)'!
chainExamplesDoubleColon

	1 bitShift: 3 :: + 5.  "13"

	3 + 4 :: negated + 2 :: negated - 2 bitShift: 2 :: bitShift: -1.  "6"

	1 = 2 and: [3 = 4] :: ifTrue: [5] ifFalse: [6].  "6"

	1 = 1
		:: and: [2 = 2]
		:: and: [3 = 3]
		:: and: [4 = 4]
		:: ifTrue: [#yes] ifFalse: [#bummer].  "#yes"

	1 = 1 :: = true.  "true"

	1 = 1 :: ifTrue: [2] ifFalse: [3] :: + 1 ; + 10.  "12"

	1 bitShift: 3 :: + 2 bitShift: -1.  "5"

	1 to: 10 by: 3 :: size.  "4"

	1 to: 10 by: 3 :: inject: 0 into: [:t :x | t + x].  "22"

	1 to: 10 by: 3 :: inject: 0 into: [:t :x | t + x] :: bitShift: -1 :: - 1.  "10"

	CompiledMethod allInstances
		:: collect: [:x | x sourceOrDecompiled -> x]
		:: collect: [:x | x key occurrencesOf: $( :: + (x key occurrencesOf: $)) :: -> x key -> x value]
		:: asSortedCollection: [:x :y | x > y]
		:: inspect.! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 20/Nov/2026, 6:52:05 pm (UTC)'!
sourceOrDecompiled
	"Answer source code. If not available, answer decompiled code.
	(Object compiledMethodAt: #printString) sourceOrDecompiled
	"

	| answer |
	answer := nil.
	CodeHistory current
		methodSourceAndMetadataFor: self historyPointer
		intoMethodDefinition: [ :methodSource :authorAndStampAsSaved :methodPropertiesString |
			answer := methodSource ]
		methodRemoval: []
		priorReferenceInto: [:p |].
		
	^answer ifNil: [ self decompileString ]! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 20/Nov/2026, 6:51:51 pm (UTC)'!
authorAndStampAsSaved
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStampLocalTZ
	(Object compiledMethodAt: #printString) authorAndStampUTC
	"

	CodeHistory current
		methodSourceAndMetadataFor: self historyPointer
		intoMethodDefinition: [ :methodSource :authorAndStampAsSaved :methodPropertiesString |
			^authorAndStampAsSaved ]
		methodRemoval: []
		priorReferenceInto: [:p |].! !


!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 20/Nov/2026, 7:32:18 pm (UTC)'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| historyPointer nextHistoryPointer methodProperties authorAndStampLocalTZ |

	selectorOfMethod := selector.
	currentCompiledMethod := method.
	classOfMethod := meta ifTrue: [class class] ifFalse: [class].
	changeList := OrderedCollection new.
	list := OrderedCollection new.
	listIndex := 0.

	historyPointer := method historyPointer.
	[ historyPointer = 0 ] whileFalse: [
		nextHistoryPointer := 0.
		CodeHistory current
			methodSourceAndMetadataFor: historyPointer
			intoMethodDefinition: [ :methodSource :authorAndStampAsSaved :methodPropertiesString |
				"If there is no source and the following are not available, they will be '' and nil."
				authorAndStampLocalTZ := CompiledMethod authorAndStampLocalTZ: authorAndStampAsSaved.
				methodProperties := Compiler evaluate: methodPropertiesString.
				self addItem:
						(MethodDefinitionChange new
							class: class name category: category meta: meta stamp: authorAndStampLocalTZ
							properties: (methodProperties ifNil: [#()])
							selector: selectorOfMethod
							string: methodSource)
					text:
						"Maybe add an indication of library from where this version was loaded."
						authorAndStampLocalTZ , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector
						, (methodProperties ifNil: [''] ifNotNil: [' [methodProperties: ', methodPropertiesString, '] '])]
			methodRemoval: [
						self addItem: (MethodDeletionChange new methodReference:
										(MethodReference class: classOfMethod selector: selectorOfMethod))
								text: '---------- method removal: ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector ]
			priorReferenceInto: [ :p |
				"If no previous version in CodeHistory, take it from method preablme in .changes if present.
				(these will most likely be completely removed in Cuis 8.0)"
				nextHistoryPointer := p ].
		CodeHistory current
			previousAndNextFrom: historyPointer
			into: [ :p :n | p = 0 ifFalse: [ nextHistoryPointer := p ]].
		historyPointer := nextHistoryPointer ].! !

!methodRemoval: CodeHistory #methodSourceAndMetadataFor:into: stamp: 'jmv 20/Nov/2026, 7:37:20 pm (UTC)'!
CodeHistory removeSelector: #methodSourceAndMetadataFor:into:!
!methodRemoval: CompiledMethod #filePosition stamp: 'jmv 20/Nov/2026, 7:37:27 pm (UTC)'!
CompiledMethod removeSelector: #filePosition!
!methodRemoval: CompiledMethod #getSourceFromFile stamp: 'jmv 20/Nov/2026, 7:37:25 pm (UTC)'!
CompiledMethod removeSelector: #getSourceFromFile!
!methodRemoval: CompiledMethod #fileIndex stamp: 'jmv 20/Nov/2026, 7:37:28 pm (UTC)'!
CompiledMethod removeSelector: #fileIndex!

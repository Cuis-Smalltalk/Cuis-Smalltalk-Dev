'From Cuis 5.0 of 7 November 2016 [latest update: #3168] on 9 September 2017 at 3:07:54 am'!

!BraceNode methodsFor: 'as yet unclassified' stamp: 'pb 9/9/2017 03:05:08'!
isComplex
	^ true.! !


!BraceNode methodsFor: 'printing' stamp: 'pb 9/9/2017 03:05:21'!
printOn: aStream indent: level
	| isComplex useLevel |
	useLevel := level.
	isComplex := elements anySatisfy: [ :ea |
		ea isComplex ].
	isComplex ifTrue: [ useLevel := useLevel + 1 ].
	aStream nextPut: ${.
	1
		to: elements size
		do: [ :i |
			isComplex ifTrue: [ aStream newLineTab: (1 max: useLevel) ].
			(elements at: i)
				printOn: aStream
				indent: useLevel.
			i < elements size ifTrue: [ aStream nextPutAll: '. ' ]].
	isComplex ifTrue: [ aStream newLineTab: (1 max: level) ].
	aStream nextPut: $}.! !


!LiteralNode methodsFor: 'printing' stamp: 'pb 9/9/2017 03:06:57'!
printOn: aStream indent: level
	key isVariableBinding
		ifTrue: [
			key key isNil
				ifTrue: [
					aStream
						nextPutAll: '###';
						nextPutAll: key value soleInstance name ]
				ifFalse: [
					aStream
						nextPutAll: '##';
						nextPutAll: key key ]]
		ifFalse: [
			key isLiteral
				ifTrue: [ | isComplex |
					isComplex := false.
					key isArray ifTrue: [
						isComplex := key anySatisfy: [ :ea |
							ea isArray ]].
					"Is it complex? (i.e. array of arrays)"
					isComplex
						ifTrue: [
							aStream
								nextPut: $#;
								nextPut: $(.
							key do: [ :ea |
								aStream newLineTab: (1 max: level + 1).
								ea storeOn: aStream ].
							aStream newLineTab: (1 max: level).
							aStream nextPut: $) ]
						ifFalse: [ key storeOn: aStream ]]
				ifFalse: [
					"Need to generate code for stuff that is in a CompiledMethod literal
					but is not understood as a literal by the Compiler.
					Well, then it is because it was generated using backticks!!"
					aStream nextPut: $`.
					key storeOn: aStream.
					aStream nextPut: $` ]].! !


!BraceNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools: elements maxElementsForConsArray)
('testing' blockAssociationCheck: numElements)
('enumerating' casesForwardDo: casesReverseDo:)
('code generation (new scheme)' emitCodeForValue:encoder: sizeCodeForValue:)
('code generation' selectorForShortForm:)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' elements: elements:sourceLocations: matchBraceStreamReceiver:messages: matchBraceWithReceiver:selector:arguments:)
('as yet unclassified' isComplex)
!


'From Cuis7.3 [latest update: #7158] on 15 May 2025 at 7:17:20 pm'!

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 5/15/2025 17:15:57'!
visitAssignmentNode: anAssignmentNode 
	
	self addToEquivalentsIfOneStatementEquivalentTo: anAssignmentNode.
	
	^super visitAssignmentNode: anAssignmentNode 
! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 5/15/2025 17:18:13'!
visitBacktickNode: aBacktickNode 
	
	self addToEquivalentsIfOneStatementEquivalentTo: aBacktickNode.
	
	^super visitBacktickNode: aBacktickNode! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 5/15/2025 19:08:30'!
skipSeparatorsFrom: aStartIndex by: aStep in: aSourceCode
	
	| char index |
	
	index := aStartIndex + aStep.
	[ char := aSourceCode at: index ifAbsent: [ Character null ].
	char isSeparator ] whileTrue: [ index := index + aStep ].
	
	^char -> index! !


!MessageNode methodsFor: 'source ranges' stamp: 'HAW 5/15/2025 14:08:46'!
expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

	| receiverExpandedRanges expandedRangeWithReceiver |
	
	receiverExpandedRanges := self isCascade
		ifTrue: [ aSourceRange ] "not expanded because expansion is handled in CascadeNode"
		ifFalse: [ originalReceiver expandRanges: (self receiverSourceRangesFrom: sourceRanges) basedOn: sourceRanges using: sourceCode ].
	expandedRangeWithReceiver := self
		expandRange: (aSourceRange isInterval ifTrue: [ aSourceRange ] ifFalse: [ aSourceRange first ])
		basedOn: receiverExpandedRanges.
	^ super
		expandRanges: expandedRangeWithReceiver
		basedOn: expandedRangeWithReceiver
		using: sourceCode! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 5/15/2025 19:15:42'!
expandToMatchExpressionOn: aSourceCode
	"takes a source code and if the source range references an expression
	that can be expanded and still is valid, it returns the source range
	'grown'. Examples: (*3 + 4*) to *(3 + 4)*; `*3 + 4*` to *`3 + 4`*"

	| firstCharAndIndex lastCharAndIndex |
	 
	firstCharAndIndex := self skipSeparatorsFrom: start by: -1 in: aSourceCode.
	lastCharAndIndex := self skipSeparatorsFrom: stop by: 1 in: aSourceCode.
	
	^ (self canBeExpandedStartingWith: firstCharAndIndex key endingWith: lastCharAndIndex key)
		ifTrue: [ self class from: firstCharAndIndex value to: lastCharAndIndex value ]
		ifFalse: [ self ]! !


!CompiledMethod methodsFor: 'method node' stamp: 'HAW 5/15/2025 18:24:41'!
createMethodNodeOptimizeSpecialSends: doOptimizeSpecialSends
	"Creates the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
		getSourceFor: (self selector ifNil: [ self defaultSelector ])
		in: aClass.
	"OMeta2 (and maybe others) could do source code transformations that mean #methodNodeFor: could fail.
	To avoid incopatibility with other parsers, we only send the message methodNodeFor:noPattern:optimizeSpecialSends: 
	when it is false that is the special case needed for the refactorings - Hernan"
	^ (doOptimizeSpecialSends
		ifTrue: [ 	(aClass methodNodeFor: source) ]
		ifFalse: [ (aClass methodNodeFor: source noPattern: false optimizeSpecialSends: false) ]) 
			ifNil: [ self decompile ].! !


!SourceCodeOfMethodToBeExtractedPrecondition methodsFor: 'private' stamp: 'HAW 5/15/2025 17:30:28'!
findSourceRangeOfCloserStatementIn: listOfAncestors ifNone: noneBlock

	^ (listOfAncestors
		detect: [ :assoc | assoc key isBlockNode or: [ assoc key isReturn ]]
		ifFound: [ :assoc | listOfAncestors before: assoc ifNone: noneBlock ]
		ifNone: noneBlock) value! !

!methodRemoval: SourceCodeInterval #expandBy: stamp: 'HAW 5/15/2025 19:04:44'!
SourceCodeInterval removeSelector: #expandBy:!

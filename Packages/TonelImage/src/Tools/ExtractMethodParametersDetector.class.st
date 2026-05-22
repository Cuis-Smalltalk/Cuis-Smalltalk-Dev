"
I am responsible for returning the parse nodes we need to parameterize before performing an extract method refactoring.
"
Class {
	#name : 'ExtractMethodParametersDetector',
	#superclass : 'Object',
	#instVars : [
		'intervalToExtract',
		'methodNodeToRefactor'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ExtractMethodParametersDetector class >> for: aMethodNodeToRefactor at: anIntervalToExtract [

	^ self new initializeFor: aMethodNodeToRefactor at: anIntervalToExtract
]

{ #category : 'evaluating' }
ExtractMethodParametersDetector class >> valueFor: aMethodNodeToRefactor at: anIntervalToExtract [

	^ (self for: aMethodNodeToRefactor at: anIntervalToExtract) value
]

{ #category : 'private' }
ExtractMethodParametersDetector >> blockNodesEnclosingIntervalToExtract [

	| nodes |
	nodes := Set new.
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isBlockNode and: [ self intervalToExtractIsCoveredByAnyOf: sourceRanges ])
			ifTrue: [ nodes add: parseNode ] ].
	^ nodes
]

{ #category : 'private' }
ExtractMethodParametersDetector >> definedInOuterScope: parseNode [

	^ self nodesThatAddVariablesToScope
		anySatisfy: [ :node | node hasLocallyArgumentOrTemporaryNamed: parseNode name ]
]

{ #category : 'private' }
ExtractMethodParametersDetector >> firstPositionInIntervalToExtractIn: sourceRanges  [

	^ (self intervalsToExtractIn: sourceRanges)
		min: [ :sourceRange | sourceRange first ]
]

{ #category : 'initialization' }
ExtractMethodParametersDetector >> initializeFor: aMethodNodeToRefactor at: anIntervalToExtract [

	methodNodeToRefactor := aMethodNodeToRefactor.
	intervalToExtract := anIntervalToExtract
]

{ #category : 'private' }
ExtractMethodParametersDetector >> intervalToExtractIncludesAnyOf: sourceRanges [

	^ sourceRanges anySatisfy: [ :sourceRange |
		intervalToExtract includes: sourceRange first ]
]

{ #category : 'private' }
ExtractMethodParametersDetector >> intervalToExtractIsCoveredByAnyOf: sourceRanges [

	^ sourceRanges anySatisfy: [ :sourceRange |
		sourceRange first < intervalToExtract first
			and: [ sourceRange last > intervalToExtract last ] ]
]

{ #category : 'private' }
ExtractMethodParametersDetector >> intervalsToExtractIn: sourceRanges [

	^ sourceRanges select: [ :sourceRange | intervalToExtract includesAllOf: sourceRange ]
]

{ #category : 'private' }
ExtractMethodParametersDetector >> isNotExtractedAlongWithItsDeclaration: parseNode [

	^ (methodNodeToRefactor
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNodeInInterval |
			parseNodeInInterval isTemporariesDeclaration
				and: [ parseNode isVariableNode ]
				and: [ parseNodeInInterval declaresVariable: parseNode ] ]) not
]

{ #category : 'private' }
ExtractMethodParametersDetector >> nodesThatAddVariablesToScope [

	^ (Set with: methodNodeToRefactor)
		addAll: self blockNodesEnclosingIntervalToExtract;
		yourself
]

{ #category : 'private' }
ExtractMethodParametersDetector >> shouldBeParameterized: parseNode appearingIn: sourceRanges [

	^ (self intervalToExtractIncludesAnyOf: sourceRanges)
		and: [ parseNode isTempOrArg ]
		and: [ self definedInOuterScope: parseNode ]
		and: [ self isNotExtractedAlongWithItsDeclaration: parseNode ]
]

{ #category : 'evaluating' }
ExtractMethodParametersDetector >> value [
	
	"Return the parse nodes of the parameters to use in the extracted method.
	
	Implementation details
	--------------
	The enumeration order of the parse nodes is non-deterministic.
	
	To fix this, I keep track of the position for each parameter node to sort
	results in the order they appear in the source code."
	
	| parameterNodePositions |
	
	parameterNodePositions := Dictionary new.
	
	methodNodeToRefactor completeSourceRangesDo: [ :parseNode :sourceRanges |
		(self shouldBeParameterized: parseNode appearingIn: sourceRanges)
			ifTrue: [ 
				parameterNodePositions
					at: parseNode
					put: (self firstPositionInIntervalToExtractIn: sourceRanges) ]].
		
	^ parameterNodePositions keys sort: [ :parameterNode :anotherParameterNode |
		(parameterNodePositions at: parameterNode) <= (parameterNodePositions at: anotherParameterNode) ]
]

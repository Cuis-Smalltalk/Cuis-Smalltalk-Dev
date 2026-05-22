Class {
	#name : 'NodesInRangeFinder',
	#superclass : 'Object',
	#instVars : [
		'range',
		'sourceMethod',
		'sourceMethodNode',
		'statements',
		'sourceRangeIndexByNode',
		'firstNodeFound',
		'lastNodeRange',
		'sourceCode'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
NodesInRangeFinder class >> in: aSourceMethod for: anIntervalToExtract  [

	^self new initializeIn: aSourceMethod for: anIntervalToExtract 
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> addToStatementsIf: aNode isAtStartingPositionWhenLocatedAt: nodeRange [

	(self isFirstInRange: aNode locatedAt: nodeRange)
		ifTrue: [
			firstNodeFound := true.
			statements add: aNode.
			range last <= nodeRange last ifTrue: [ ^true ].
			lastNodeRange := nodeRange ].

	^false
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> addToStatementsIf: aNode isInsideExtractionWhenLoacatedAt: nodeRange [

	(self notSubNodeOfLastNode: nodeRange) ifTrue: [
		statements add: aNode.
		(self isAtEndPosition: nodeRange) ifTrue: [ ^true ].
		lastNodeRange := nodeRange.].
	
	^false
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> completeSourceRangeOf: aNode [

	| nodeRange nodeRanges |
	
	"Some nodes do not have ranges at all like BlockNode - Hernan"
	nodeRanges := sourceMethodNode completeSourceRanges at: aNode ifAbsent: [ #() ].
	nodeRange := nodeRanges at: (sourceRangeIndexByNode occurrencesOf: aNode) ifAbsent: [ `0 to: 0` ].
	nodeRange := nodeRange asSourceCodeInterval trimToMatchExpressionOn: sourceCode.
	
	^nodeRange
		
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> ifInsideRangeAddToStatements: aNode locatedAt: nodeRange [

	^ firstNodeFound
		ifTrue: [ self addToStatementsIf: aNode isInsideExtractionWhenLoacatedAt: nodeRange]
		ifFalse: [ self addToStatementsIf: aNode isAtStartingPositionWhenLocatedAt: nodeRange]
]

{ #category : 'initialization' }
NodesInRangeFinder >> initializeIn: aSourceMethod for: anIntervalToExtract  [

	range := anIntervalToExtract.
	sourceMethod := aSourceMethod.
	
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> isAtEndPosition: nodeRange [

	^ range last = nodeRange last
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> isAtStartPosition: nodeRange [

	^ range first <= nodeRange first
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> isCascadeMessage: aNode insideRange: nodeRange [

	^ aNode isCascadeNode and: [ range last + 2 = nodeRange last ]
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> isFirstInRange: aNode locatedAt: nodeRange [

	^(self isAtStartPosition: nodeRange) and: [ self isInsideRange: aNode locatedAt: nodeRange ]
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> isInsideRange: aNode locatedAt: nodeRange [

	^ range last >= nodeRange last or: [self isCascadeMessage: aNode insideRange: nodeRange]
]

{ #category : 'evaluating - private' }
NodesInRangeFinder >> notSubNodeOfLastNode: nodeRange [
	
	^lastNodeRange last < nodeRange first
]

{ #category : 'evaluating' }
NodesInRangeFinder >> value [
	
	| nodeAtLastPositionFound |

	"Because some nodes have many ranges, like the literal nodes, we count the time we pass by a node that is equivalent
	to the index in the source ranges collection. sourceRangeIndexByNode is used to count per node - Hernan"
	sourceMethodNode := sourceMethod methodNode.
	sourceCode := sourceMethod sourceCode.
	statements := OrderedCollection new.
	sourceRangeIndexByNode := IdentityBag new.
	firstNodeFound := false.

	sourceMethodNode nodesDo: [ :aNode | | nodeRange |
		sourceRangeIndexByNode add: aNode.
		nodeRange := self completeSourceRangeOf: aNode.
		nodeAtLastPositionFound := self ifInsideRangeAddToStatements: aNode locatedAt: nodeRange.
		nodeAtLastPositionFound ifTrue: [ ^statements ]].
			
	self error: 'nodes not found'
	
]

Class {
	#name : 'EquivalentNodesFinder',
	#superclass : 'ParseNodeVisitor',
	#instVars : [
		'nodesToLookFor',
		'equivalentNodesRanges',
		'nodesToLookForSize',
		'uniqueNodeVisitedCounter',
		'methodNode',
		'selectionToKeep'
	],
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'instance creation' }
EquivalentNodesFinder class >> for: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep [

	^ self new initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep
]

{ #category : 'instance creation' }
EquivalentNodesFinder class >> noSelection [
	
	^`0 to: 0`
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> addEquivalentNodesOf: blockStatements  [

	| start stop |
	
	start := 1.
	
	[ stop := start + nodesToLookForSize - 1.
	stop <= blockStatements size ] whileTrue: [ 
		start := self 
			nextStartAfterAddingEquivalentNodesOf: blockStatements 
			from: start 
			to: stop ].
		
	
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> addToEquivalentsIfOneStatementEquivalentTo: aNode [

	(self hasOneNodeToLookForAndEquivalentTo: aNode)
		ifTrue: [ equivalentNodesRanges add: (self completeSourceRangeOf: aNode) ].
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> areNodesToLookForEquivalentTo: nodesToCompareWith [
 
	| equivalent |
	
	equivalent := nodesToLookFor 
		with: nodesToCompareWith 
		allSatisfy: [:leftNode :rightNode | leftNode equivalentTo: rightNode ].
		
	^equivalent and: [ self shouldKeepSelection not or: [ self selectionToKeepIsEqualOrHasNoIntersectionWith: nodesToCompareWith ]]	
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> completeSourceRangeOf: aNode [

	| sourceRangeIndex completeSourceRanges completeRange trimmedRange |
	
	sourceRangeIndex := uniqueNodeVisitedCounter occurrencesOf: aNode.
	sourceRangeIndex = 0 ifTrue: [ sourceRangeIndex := 1 ].
	
	"It should always find the completeSourceRanges of aNode and a range at the sourceRangeIndex - Hernan"
	completeSourceRanges := methodNode completeSourceRanges at: aNode.
	completeRange := completeSourceRanges at: sourceRangeIndex.
	
	"See ExtractMethodFinderTest>>test27FindsInsideParentesis - Hernan"
	trimmedRange := nodesToLookForSize = 1 
		ifTrue: [ completeRange trimToMatchExpressionOn: methodNode sourceText ]
		ifFalse: [ completeRange ].
	
	^trimmedRange
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> completeSourceRangesOfAll: aCollectionOfNodes [

	| firstRange lastRange |
	
	firstRange := self completeSourceRangeOf: aCollectionOfNodes first.
	lastRange := self completeSourceRangeOf: aCollectionOfNodes last.
	
	^SourceCodeInterval from: firstRange first to: lastRange last
]

{ #category : 'ranges' }
EquivalentNodesFinder >> equivalentNodesRanges [
	
	^equivalentNodesRanges
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> hasOneNodeToLookForAndEquivalentTo: aNode [

	^ nodesToLookForSize = 1 and: [ nodesToLookFor first equivalentTo: aNode ]
]

{ #category : 'initialization' }
EquivalentNodesFinder >> initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep [

	nodesToLookFor := aNodesToLookFor.
	methodNode := aMethodNode.
	selectionToKeep := aSelectionToKeep.
	
	nodesToLookForSize := nodesToLookFor size.
	uniqueNodeVisitedCounter := IdentityBag new.
	equivalentNodesRanges := OrderedCollection new.
	
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> nextStartAfterAddingEquivalentNodesOf: blockStatements from: start to: stop  [
	
	| nodesToCompareWith nextStart |
	
	nodesToCompareWith := blockStatements copyFrom: start to: stop.
	
	nextStart := (self areNodesToLookForEquivalentTo: nodesToCompareWith) 
		ifTrue: [ 
			equivalentNodesRanges add: (self completeSourceRangesOfAll: nodesToCompareWith).
			stop + 1 ]
		ifFalse: [ 
			start + 1].
		
	^nextStart
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> selectionToKeepIsEqualOrHasNoIntersectionWith: nodesToCompareWith  [

	 | equivalentNodesSourceRanges | 

	equivalentNodesSourceRanges := self completeSourceRangesOfAll: nodesToCompareWith.
		
	^(selectionToKeep = equivalentNodesSourceRanges or: [ (selectionToKeep intersectsRange: equivalentNodesSourceRanges) not ])
]

{ #category : 'private - visiting' }
EquivalentNodesFinder >> shouldKeepSelection [

	^selectionToKeep ~= self class noSelection 
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitAssignmentNode: anAssignmentNode  [
	
	self addToEquivalentsIfOneStatementEquivalentTo: anAssignmentNode.
	
	^super visitAssignmentNode: anAssignmentNode 

]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitBacktickNode: aBacktickNode  [
	
	self addToEquivalentsIfOneStatementEquivalentTo: aBacktickNode.
	
	^super visitBacktickNode: aBacktickNode
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitBlockNode: blockNode [

	| blockStatements |

	"See ExtractMethodFinderTest>>#test30DoesNotFailWhenLookingForABlockEquivalentToAMethod"
	blockNode = methodNode block ifFalse: [
		self addToEquivalentsIfOneStatementEquivalentTo: blockNode ].

	super visitBlockNode: blockNode.
	
	nodesToLookForSize > 1 ifTrue: [ 
		blockStatements := blockNode statements copy. 
		"I need to add temp declaration to statements becuase user could have selected the temp declaration
		See 
		- ExtractMethodFinderTest>>test22FindsTemporaryWhenSelectingDefinitionInInnerClosure
		- ExtractMethodFinderTest>>test22_1_FindsTemporaryWhenSelectingDefinitionInInnerClosure - Hernan"
		blockNode declaresAnyVariable ifTrue: [ blockStatements addFirst: blockNode temporariesDeclaration ].
		self addEquivalentNodesOf: blockStatements ]
		
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitBraceNode: aBraceNode  [
	
	uniqueNodeVisitedCounter add: aBraceNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aBraceNode.
	
	^super visitBraceNode: aBraceNode 
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitCascadeNode: aCascadeNode  [

	self addToEquivalentsIfOneStatementEquivalentTo: aCascadeNode.

	 ^super visitCascadeNode: aCascadeNode 
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitInstanceVariableNode: anInstanceVariableNode  [

	uniqueNodeVisitedCounter add: anInstanceVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: anInstanceVariableNode.

	^super visitInstanceVariableNode: anInstanceVariableNode 
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitLiteralNode: aLiteralNode [

	uniqueNodeVisitedCounter add: aLiteralNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aLiteralNode.

	^super visitLiteralNode: aLiteralNode
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitLiteralVariableNode: aLiteralVariableNode  [
	
	uniqueNodeVisitedCounter add: aLiteralVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aLiteralVariableNode.
	
	^super visitLiteralVariableNode: aLiteralVariableNode 
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitMessageNode: aMessageNode [

	self addToEquivalentsIfOneStatementEquivalentTo: aMessageNode.

	 ^super visitMessageNode: aMessageNode 
]

{ #category : 'visiting' }
EquivalentNodesFinder >> visitTempVariableNode: aTempVariableNode  [
	
	uniqueNodeVisitedCounter add: aTempVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aTempVariableNode.
	
	^super visitTempVariableNode: aTempVariableNode 
]

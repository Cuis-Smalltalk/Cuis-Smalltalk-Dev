'From Cuis7.5 [latest update: #7263] on 13 June 2025 at 10:12:24 am'!
!classDefinition: #EquivalentNodesFinder category: #'Compiler-Support'!
ParseNodeVisitor subclass: #EquivalentNodesFinder
	instanceVariableNames: 'nodesToLookFor equivalentNodesRanges nodesToLookForSize nodeVisitedCounter methodNode selectionToKeep uniqueNodeVisitedCounter '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 6/13/2025 09:58:24'!
completeSourceRangeOf: aNode

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
	
	^trimmedRange! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 6/13/2025 10:00:04'!
visitBlockNode: blockNode

	| blockStatements |

	self addToEquivalentsIfOneStatementEquivalentTo: blockNode.

	super visitBlockNode: blockNode.
	
	nodesToLookForSize > 1 ifTrue: [ 
		blockStatements := blockNode statements. 
		"I need to add temp declaration to statements becuase user could have selected the temp declaration
		See ExtractMethodFinderTest>>test22FindsTemporaryWhenSelectingDefinitionInInnerClosure - Hernan"
		blockNode declaresAnyVariable ifTrue: [ blockStatements addFirst: blockNode temporariesDeclaration ].
		self addEquivalentNodesOf: blockStatements ]
		! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 6/13/2025 09:58:24'!
visitBraceNode: aBraceNode 
	
	uniqueNodeVisitedCounter add: aBraceNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aBraceNode.
	
	^super visitBraceNode: aBraceNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 6/13/2025 09:58:24'!
visitInstanceVariableNode: anInstanceVariableNode 

	uniqueNodeVisitedCounter add: anInstanceVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: anInstanceVariableNode.

	^super visitInstanceVariableNode: anInstanceVariableNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 6/13/2025 09:58:24'!
visitLiteralNode: aLiteralNode

	uniqueNodeVisitedCounter add: aLiteralNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aLiteralNode.

	^super visitLiteralNode: aLiteralNode! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 6/13/2025 09:58:24'!
visitLiteralVariableNode: aLiteralVariableNode 
	
	uniqueNodeVisitedCounter add: aLiteralVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aLiteralVariableNode.
	
	^super visitLiteralVariableNode: aLiteralVariableNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 6/13/2025 09:58:24'!
visitTempVariableNode: aTempVariableNode 
	
	uniqueNodeVisitedCounter add: aTempVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aTempVariableNode.
	
	^super visitTempVariableNode: aTempVariableNode ! !

!EquivalentNodesFinder methodsFor: 'initialization' stamp: 'HAW 6/13/2025 09:58:24'!
initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep

	nodesToLookFor := aNodesToLookFor.
	methodNode := aMethodNode.
	selectionToKeep := aSelectionToKeep.
	
	nodesToLookForSize := nodesToLookFor size.
	uniqueNodeVisitedCounter := IdentityBag new.
	equivalentNodesRanges := OrderedCollection new.
	! !

!classDefinition: #EquivalentNodesFinder category: #'Compiler-Support'!
ParseNodeVisitor subclass: #EquivalentNodesFinder
	instanceVariableNames: 'nodesToLookFor equivalentNodesRanges nodesToLookForSize uniqueNodeVisitedCounter methodNode selectionToKeep'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!EquivalentNodesFinder reorganize!
('private - visiting' addEquivalentNodesOf: addToEquivalentsIfOneStatementEquivalentTo: areNodesToLookForEquivalentTo: completeSourceRangeOf: completeSourceRangesOfAll: hasOneNodeToLookForAndEquivalentTo: nextStartAfterAddingEquivalentNodesOf:from:to: selectionToKeepIsEqualOrHasNoIntersectionWith: shouldKeepSelection)
('visiting' visitAssignmentNode: visitBacktickNode: visitBlockNode: visitBraceNode: visitCascadeNode: visitInstanceVariableNode: visitLiteralNode: visitLiteralVariableNode: visitMessageNode: visitTempVariableNode:)
('initialization' initializeFor:in:keepingSelection:)
('ranges' equivalentNodesRanges)
!


'From Cuis7.9 [latest update: #8045] on 3 July 2026 at 3:33:57 pm'!

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 27/Jan/2026, 12:37:36 pm (UTC)'!
addAllToFoundNodes: nodesToAdd

	foundNodes add: (self completeSourceRangesOfAll: nodesToAdd). ! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:29:15 pm (UTC)'!
addSingleAssigmentNodeIfNecessary: anAssignmentNode

	self addToEquivalentsIfOneStatementEquivalentTo: anAssignmentNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:29:25 pm (UTC)'!
addSingleBacktickNodeIfNecessary: aBacktickNode

	self addToEquivalentsIfOneStatementEquivalentTo: aBacktickNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:31:59 pm (UTC)'!
addSingleBlockNodeIfNeccesary: blockNode

	self addToEquivalentsIfOneStatementEquivalentTo: blockNode! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:29:32 pm (UTC)'!
addSingleBraceNodeIfNecessary: aBraceNode
	
	self addToEquivalentsIfOneStatementEquivalentTo: aBraceNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 5/Feb/2026, 12:50:21 am (UTC)'!
addSingleCascadeNodeIfNecessary: aCascadeNode

	self addEquivalentNodesInCascadeNode: aCascadeNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:29:48 pm (UTC)'!
addSingleInstanceVariableNodeIfNecessary: anInstanceVariableNode

	self addToEquivalentsIfOneStatementEquivalentTo: anInstanceVariableNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:25:01 pm (UTC)'!
addSingleLiteralNodeIfNeccesary: aLiteralNode

	self addToEquivalentsIfOneStatementEquivalentTo: aLiteralNode! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:26:29 pm (UTC)'!
addSingleLiteralVariableNodeIfNecessary: aLiteralVariableNode

	self addToEquivalentsIfOneStatementEquivalentTo: aLiteralVariableNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:27:24 pm (UTC)'!
addSingleMessageNodeIfNeccesary: aMessageNode

	self addToEquivalentsIfOneStatementEquivalentTo: aMessageNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 5/Feb/2026, 12:48:16 am (UTC)'!
addSingleReturnNodeIfNeccesary: aReturnNode

	self addToEquivalentsIfOneStatementEquivalentTo: aReturnNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:28:24 pm (UTC)'!
addSingleTempVariableNodeIfNecessary: aTempVariableNode

	self addToEquivalentsIfOneStatementEquivalentTo: aTempVariableNode.! !

!EquivalentNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 11:18:54 pm (UTC)'!
nodesToLookForMatchingConditionWith: nodesToCompareWith

	| equivalent |
	
	equivalent := nodesToLookFor 
		with: nodesToCompareWith 
		allSatisfy: [:leftNode :rightNode | leftNode equivalentTo: rightNode ].
		
	^ equivalent 
		and: [ self shouldKeepSelection not
			or: [ self selectionToKeepIsEqualOrHasNoIntersectionWith: nodesToCompareWith ]]! !


!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 26/Jan/2026, 11:40:00 pm (UTC)'!
addToEquivalentsIfOneStatementEquivalentTo: aNode

	(self hasOneNodeToLookForAndEquivalentTo: aNode)
		ifTrue: [ foundNodes add: (self completeSourceRangeOf: aNode) ].! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 5/Feb/2026, 12:23:55 am (UTC)'!
addToEquivalentsNodesPartiallyOf: aCascadeNode 

	| intervalToAdd cascadeNodeSourceRange |
	
	cascadeNodeSourceRange := self completeSourceRangeOf: aCascadeNode.
	
	intervalToAdd := (cascadeNodeSourceRange includesAnyOf: selectionToKeep)
		ifTrue: [ selectionToKeep ]
		ifFalse: [
			(cascadeNodeSourceRange first 
				to: (cascadeNodeSourceRange first + selectionToKeep size - 1))
				asSourceCodeInterval ].

	foundNodes add: intervalToAdd! !

!EquivalentNodesFinder methodsFor: 'ranges' stamp: 'JEC 26/Jan/2026, 11:40:00 pm (UTC)'!
equivalentNodesRanges
	
	^foundNodes! !

!methodRemoval: EquivalentNodesFinder #visitMessageNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitMessageNode:!
!methodRemoval: EquivalentNodesFinder #completeSourceRangesOfAll: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #completeSourceRangesOfAll:!
!methodRemoval: EquivalentNodesFinder #visitBraceNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitBraceNode:!
!methodRemoval: EquivalentNodesFinder #visitCascadeNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitCascadeNode:!
!methodRemoval: EquivalentNodesFinder #initializeFor:in:keepingSelection: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #initializeFor:in:keepingSelection:!
!methodRemoval: EquivalentNodesFinder #completeSourceRangeOf: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #completeSourceRangeOf:!
!methodRemoval: EquivalentNodesFinder #visitLiteralVariableNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitLiteralVariableNode:!
!methodRemoval: EquivalentNodesFinder #nextStartAfterAddingEquivalentNodesOf:from:to: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #nextStartAfterAddingEquivalentNodesOf:from:to:!
!methodRemoval: EquivalentNodesFinder #visitBlockNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitBlockNode:!
!methodRemoval: EquivalentNodesFinder #visitLiteralNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitLiteralNode:!
!methodRemoval: EquivalentNodesFinder #visitInstanceVariableNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitInstanceVariableNode:!
!methodRemoval: EquivalentNodesFinder #visitBacktickNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitBacktickNode:!
!methodRemoval: EquivalentNodesFinder #visitAssignmentNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitAssignmentNode:!
!methodRemoval: EquivalentNodesFinder #areNodesToLookForEquivalentTo: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #areNodesToLookForEquivalentTo:!
!methodRemoval: EquivalentNodesFinder #visitTempVariableNode: stamp: 'jmv 3/Jul/2026, 7:33:52 pm (UTC)'!
EquivalentNodesFinder removeSelector: #visitTempVariableNode:!

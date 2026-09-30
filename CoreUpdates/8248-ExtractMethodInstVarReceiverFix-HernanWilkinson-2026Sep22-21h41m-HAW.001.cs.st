'From Cuis7.9 [latest update: #8184] on 26 September 2026 at 11:41:19 am'!

!ParseNode methodsFor: 'testing' stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
isParameterizableVariable

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
isTemporaryOrUndeclaredVariable

	^false! !


!VariableNode methodsFor: 'testing' stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
isParameterizableVariable

	^self isSuperPseudoVariable not! !

!VariableNode methodsFor: 'testing' stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
isTemporaryOrUndeclaredVariable

	^self isTemp or: [ self isUndeclared ]! !


!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
hasTemporaryOrUndeclaredReceiver: aNode or: aNodeToLookFor

	^(self receiverNodeOf: aNode) key isTemporaryOrUndeclaredVariable
		or: [ (self receiverOf: aNodeToLookFor) isTemporaryOrUndeclaredVariable ]! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
haveParameterizableReceivers: aNode and: aNodeToLookFor

	^(self haveSameReceivers: aNode and: aNodeToLookFor)
		or: [ (self hasTemporaryOrUndeclaredReceiver: aNode or: aNodeToLookFor)
		or: [ self haveVariableReceivers: aNode and: aNodeToLookFor ]]! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 12:21:32 pm (UTC)'!
haveSameReceivers: aNode and: aNodeToLookFor

	| receiverAndRange |

	receiverAndRange := self receiverNodeOf: aNode.

	^(self
		differencesBetween: receiverAndRange key
		at: receiverAndRange value
		and: (self receiverOf: aNodeToLookFor)) isEmpty! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
haveVariableReceivers: aNode and: aNodeToLookFor

	^(self receiverNodeOf: aNode) key isParameterizableVariable
		and: [ (self receiverOf: aNodeToLookFor) isParameterizableVariable ]! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 2:22:30 pm (UTC)'!
isReceiverTemporaryOfCodeToExtract: aNode

	| receiverNode |

	receiverNode := (self receiverNodeOf: aNode) key.

	^ receiverNode isTemp and: [ temporariesRenaming includesKey: receiverNode name ]! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
receiverOf: aNode

	^(self messageNodeOf: aNode) receiverOrCascadeReceiver! !


!ParameterizableNodesFinder methodsFor: 'testing' stamp: 'HAW 26/Sep/2026, 12:20:36 pm (UTC)'!
hasOneNodeToLookForAndParameterizableWith: aNode

	temporariesRenaming := ParameterizableDifferencesFinder renamingOf: { aNode } to: nodesToLookFor.

	^ nodesToLookForSize = 1 
		and: [ (self nodesAreFromRejectingIntervals: { aNode }) not
		and: [ self is: aNode parameterizableWith: nodesToLookFor first ] ] ! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 12:20:36 pm (UTC)'!
allParameterizableNodesBetween: aNode and: nodeToLookFor

	| parameterizableNodes |
	
	parameterizableNodes := OrderedCollection new.

	(self shouldAddReceiverOf: aNode asParameterizableComparingWith: nodeToLookFor)
		ifTrue: [ parameterizableNodes add: (self receiverNodeOf: aNode) ].
	
	(self argumentsFromNodeToLookFor: nodeToLookFor)
		with: (self allArgumentNodesOf: aNode)
		do: [ :argNodeToLookFor :argFromNode |
			parameterizableNodes addAll: (self differencesBetween: argFromNode key at: argFromNode value and: argNodeToLookFor) ].
	
	^ parameterizableNodes! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 12:25:58 pm (UTC)'!
is: aNode parameterizableWith: aNodeToLookFor

	^(aNode hasParameterizableDifferenceWith: aNodeToLookFor)
		and: [ self haveParameterizableReceivers: aNode and: aNodeToLookFor ]! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 26/Sep/2026, 2:22:30 pm (UTC)'!
shouldAddReceiverOf: aNode asParameterizableComparingWith: nodeToLookFor

	(self isReceiverTemporaryOfCodeToExtract: aNode) ifTrue: [ ^ false ].

	^ (self hasTemporaryOrUndeclaredReceiver: aNode or: nodeToLookFor)
		or: [ (self haveSameReceivers: aNode and: nodeToLookFor) not ]! !

!methodRemoval: ParameterizableNodesFinder #isTemporaryOrUndeclared: stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
ParameterizableNodesFinder removeSelector: #isTemporaryOrUndeclared:!
!methodRemoval: ParameterizableNodesFinder #isReceiverOfPieceTemporary: stamp: 'HAW 26/Sep/2026, 2:22:30 pm (UTC)'!
ParameterizableNodesFinder removeSelector: #isReceiverOfPieceTemporary:!
!methodRemoval: ParameterizableNodesFinder #isParameterizableVariable: stamp: 'HAW 26/Sep/2026, 12:47:49 pm (UTC)'!
ParameterizableNodesFinder removeSelector: #isParameterizableVariable:!

!ParameterizableNodesFinder reorganize!
('initialization' initializeFor:in:keepingSelection:rejectingIntervals:)
('testing' hasFoundParameterizableReplacements hasOneNodeToLookForAndParameterizableWith:)
('private - visiting' addAllToFoundNodes: addSingleFoundNode: allArgumentNodesOf: allParameterizableNodesBetween:and: areAllNodesInMethodNode: argumentsFromNodeToLookFor: differencesBetween:at:and: hasTemporaryOrUndeclaredReceiver:or: haveNodesToLookFor: haveParameterizableReceivers:and: haveSameReceivers:and: haveVariableReceivers:and: is:parameterizableWith: is:sameAs: isLookingInMethodNode isReceiverTemporaryOfCodeToExtract: messageNodeOf: nodesAreFromRejectingIntervals: parameterizableNodeWithRealRange:using: receiverNodeOf: receiverOf: shouldAddReceiverOf:asParameterizableComparingWith:)
('accessing' parameterizableNodes)
('visiting - finding nodes' addNodeIfFoundAsParameterizable: addSingleAssigmentNodeIfNecessary: addSingleMessageNodeIfNeccesary: nodesToLookForMatchingConditionWith:)
!


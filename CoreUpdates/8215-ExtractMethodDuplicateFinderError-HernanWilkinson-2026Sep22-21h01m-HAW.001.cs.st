'From Cuis7.9 [latest update: #8184] on 22 September 2026 at 9:41:23 pm'!

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 23/Sep/2026, 12:17:34 am (UTC)'!
is: aNode parameterizableWith: aNodeToLookFor

	"A keyword send whose arguments differ, to the same receiver once the temporaries of the piece are
	taken as the same"

	| receiverAndRange |

	(aNode hasParameterizableDifferenceWith: aNodeToLookFor) ifFalse: [ ^ false ].
	receiverAndRange := self receiverNodeOf: aNode.

	^ (self
		differencesBetween: receiverAndRange key
		at: receiverAndRange value
		and: (self receiverNodeOf: aNodeToLookFor) key) isEmpty! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 23/Sep/2026, 12:15:35 am (UTC)'!
isReceiverOfPieceTemporary: aNode

	"A temporary the piece assigns goes with it into the new method, so it is not a parameter"

	| receiverNode |

	receiverNode := (self receiverNodeOf: aNode) key.

	^ receiverNode isTemp and: [ temporariesRenaming includesKey: receiverNode name ]! !


!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 23/Sep/2026, 12:15:00 am (UTC)'!
readsRenamedTemporary: aNode

	"A node that differs and reads no temporary of the piece is the difference as a whole; one that reads
	some is looked into, since the temporary cannot be a parameter"

	aNode nodesDo: [ :eachNode |
		(eachNode isTemp and: [ renaming includesKey: eachNode name ]) ifTrue: [ ^true ]].

	^false! !


!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 23/Sep/2026, 12:15:35 am (UTC)'!
allParameterizableNodesBetween: aNode and: nodeToLookFor

	| parameterizableNodes |
	
	parameterizableNodes := OrderedCollection new.

	((self shouldAddReceiverOf: aNode asParameterizableComparingWith: nodeToLookFor)
		and: [ (self isReceiverOfPieceTemporary: aNode) not ])
			ifTrue: [ parameterizableNodes add: (self receiverNodeOf: aNode) ].
	
	(self argumentsFromNodeToLookFor: nodeToLookFor)
		with: (self allArgumentNodesOf: aNode)
		do: [ :argNodeToLookFor :argFromNode |
			parameterizableNodes addAll: (self differencesBetween: argFromNode key at: argFromNode value and: argNodeToLookFor) ].
	
	^ parameterizableNodes! !

!ParameterizableNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'HAW 23/Sep/2026, 12:17:34 am (UTC)'!
nodesToLookForMatchingConditionWith: nodesToCompareWith

	| anyIsParameterizable |
	
	((self haveNodesToLookFor: nodesToCompareWith) 
		or: [ self nodesAreFromRejectingIntervals: nodesToCompareWith ])
		ifTrue: [ ^ false ].

	temporariesRenaming := ParameterizableDifferencesFinder renamingOf: nodesToCompareWith to: nodesToLookFor.
	anyIsParameterizable := false.
	nodesToCompareWith 
		with: nodesToLookFor 
		do: [ :nodeToCompare :nodeToLookFor |
			(self is: nodeToCompare sameAs: nodeToLookFor) ifFalse: [
				(self is: nodeToCompare parameterizableWith: nodeToLookFor) ifFalse: [ ^ false ].
				anyIsParameterizable := true ]].

	^ anyIsParameterizable! !


!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 23/Sep/2026, 12:15:00 am (UTC)'!
compare: aNode at: aRange with: aNodeToLookFor

	(self is: aNode at: aRange sameAs: aNodeToLookFor) ifTrue: [ ^self ].
	(self readsRenamedTemporary: aNode) ifFalse: [ ^differences add: aNode -> aRange ].
	(self areSends: aNode and: aNodeToLookFor) ifTrue: [ ^self compareSend: aNode at: aRange with: aNodeToLookFor ].
	(self areCascades: aNode and: aNodeToLookFor) ifTrue: [ ^self compareCascade: aNode at: aRange with: aNodeToLookFor ].
	(self areAssignments: aNode and: aNodeToLookFor) ifTrue: [ ^self compareAssignment: aNode at: aRange with: aNodeToLookFor ].
	(aNode isReturn and: [ aNodeToLookFor isReturn ]) ifTrue: [ ^self compareNested: aNode expr within: aRange with: aNodeToLookFor expr ].
	(self areBraces: aNode and: aNodeToLookFor) ifTrue: [ ^self compareBrace: aNode at: aRange with: aNodeToLookFor ].

	differences add: aNode -> aRange! !


!ExtractMethodNewMethod class methodsFor: 'private - pre-conditions' stamp: 'HAW 23/Sep/2026, 12:16:40 am (UTC)'!
assert: newMessage hasValidParametersForExtracting: anIntervalToExtract from: aMethodNodeToRefactor with: parameterizableNodes sharedAs: aParameterSharing

	| parseNodesToParameterize outsideParameters parameterCount |
	
	parseNodesToParameterize := ExtractMethodParametersDetector
		valueFor: aMethodNodeToRefactor
		at: anIntervalToExtract
		with: parameterizableNodes.
	outsideParameters := parseNodesToParameterize reject: [ :parseNode |
		parameterizableNodes anySatisfy: [ :nodeWithRange | nodeWithRange key equivalentTo: parseNode ]].
	parameterCount := outsideParameters size + aParameterSharing asSet size.
	
	(newMessage hasRightNumberOfArguments and: [ newMessage arguments size = parameterCount ])
		ifFalse: [ self signalExtractMethodWithWrongNumberOfArgumentsError ]! !


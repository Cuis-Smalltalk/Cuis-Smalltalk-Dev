'From Cuis7.9 [latest update: #8049] on 3 July 2026 at 3:37:32 pm'!

!ParseNode methodsFor: 'testing' stamp: 'JEC 7/Apr/2026, 4:01:37 pm (UTC)'!
compareTo: aParseNode 
	ifEquivalentDo: anEquivalentActionBlock 
	ifParameterizableDo: aParameterizableActionBlock 
	ifNoneDo: aNoneActionBlock 

	(self equivalentTo: aParseNode)
		ifTrue: [ ^ anEquivalentActionBlock value: self ].
	
	((self hasParameterizableDifferenceWith: aParseNode) 
		and: [ self value hasEquivalentReceiverWith: aParseNode value ])
		ifTrue: [ ^ aParameterizableActionBlock value: self ].
	
	aNoneActionBlock value.! !

!ParseNode methodsFor: 'testing' stamp: 'JEC 7/Apr/2026, 3:55:17 pm (UTC)'!
hasParameterizableDifferenceWith: aParseNode

	^ false! !

!ParseNode methodsFor: 'testing' stamp: 'JEC 31/May/2026, 6:50:47 pm (UTC)'!
nodeIncludes: aParseNode 

	self nodesDo: [ :aNode | (aNode equivalentTo: aParseNode) ifTrue: [ ^true ] ].
	
	^ false.! !


!AssignmentNode methodsFor: 'testing' stamp: 'JEC 7/Apr/2026, 3:55:17 pm (UTC)'!
hasParameterizableDifferenceWith: aParseNode 

	^ aParseNode isAssignmentNode
		and: [ self value hasParameterizableDifferenceWith: aParseNode value ]! !


!MethodNode methodsFor: 'source mapping' stamp: 'JEC 30/Mar/2026, 2:01:57 am (UTC)'!
sourceRangesOfParameterizableNodesOf: nodesToLookFor keepingSelection: aSelectionToKeep rejectingIntervals: rejectingIntervals
	
	| finder |
	
	finder := ParameterizableNodesFinder 
		for: nodesToLookFor 
		in: self 
		keepingSelection: aSelectionToKeep
		rejectingIntervals: rejectingIntervals.
	self accept: finder.

	^finder parameterizableNodes! !


!MessageNode methodsFor: 'testing' stamp: 'JEC 15/Mar/2026, 5:45:14 pm (UTC)'!
areNodesKeywordMessagesSend: aParseNode

	^ aParseNode isMessageNode 
		and: [ aParseNode isKeywordMessageSend 
		and: [ self isKeywordMessageSend ] ]
! !

!MessageNode methodsFor: 'testing' stamp: 'JEC 7/Apr/2026, 2:31:17 pm (UTC)'!
hasParameterizableArgumentDifferenceWithKeywordMessage: aMessageNode 

	^ (self selector equivalentTo: aMessageNode selector)
		and: [ 
			self arguments 
				with: aMessageNode arguments 
				anySatisfy: [ :selfArg :aMessageNodeArg | (selfArg equivalentTo: aMessageNodeArg) not ] ]! !

!MessageNode methodsFor: 'testing' stamp: 'JEC 7/Apr/2026, 3:55:17 pm (UTC)'!
hasParameterizableDifferenceWith: aParseNode

	^ (self areNodesKeywordMessagesSend: aParseNode)
		and: [ self isReceiverVariableNotLiteral
		and: [ aParseNode isReceiverVariableNotLiteral
		and: [ self hasParameterizableArgumentDifferenceWithKeywordMessage: aParseNode ] ] ]! !

!MessageNode methodsFor: 'testing' stamp: 'JEC 7/Apr/2026, 2:43:00 pm (UTC)'!
isReceiverVariableNotLiteral

	^ self receiver isVariableNode and: [ self receiver isLiteralNode not ]! !

!MessageNode methodsFor: 'accessing' stamp: 'JEC 26/Mar/2026, 1:04:33 am (UTC)'!
argumentNodesSourceRangesIn: aMethodNode

	^ self keywordRanges 
		with: self arguments
		collect: [ :keywordRange : argumentNode |
			| sourceRanges |
		
			sourceRanges := (aMethodNode 
				completeSourceRangesOf: argumentNode 
				ifAbsent: [ self shouldNotHappen ]) 
				asSortedCollection: [ :leftRange :rightRange | leftRange first < rightRange first ].
			
			argumentNode -> (sourceRanges detect: [ :range | keywordRange last < range first ]) ]! !

!MessageNode methodsFor: 'accessing' stamp: 'JEC 6/Apr/2026, 4:51:49 pm (UTC)'!
receiverNodeSourceRangesIn: aMethodNode 

	| receiverSourceRanges |

	receiverSourceRanges := (aMethodNode 
		completeSourceRangesOf: receiver 
		ifAbsent: [ self shouldNotHappen ]) 
		asSortedCollection: [ :leftRange :rightRange | leftRange first < rightRange first ].
		
	receiverSourceRanges withNextDo: [ :range :nextRange |
		(nextRange isNil or: [ nextRange first > keywordRanges first first ])
			ifTrue: [ ^ receiver -> range ] ].
	
	self shouldNotHappen
	! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'JEC 12/Feb/2026, 2:09:47 am (UTC)'!
intervalOfSubCollection: aSubCollectionToFind startingAt: aPosition

	| startingIndex |

	startingIndex := self indexOfSubCollection: aSubCollectionToFind startingAt: aPosition.

	^startingIndex toSelfPlus: aSubCollectionToFind size! !


!CompiledMethod methodsFor: 'method node' stamp: 'JEC 30/Mar/2026, 2:02:53 am (UTC)'!
sourceRangesOfParameterizableNodesOf: nodesToLookFor keepingSelection: aSelectionToKeep rejectingIntervals: rejectingIntervals
	
	^self methodNodeForLookingEquivalentNodes 
		sourceRangesOfParameterizableNodesOf: nodesToLookFor 
		keepingSelection: aSelectionToKeep
		rejectingIntervals: rejectingIntervals! !

!CompiledMethod methodsFor: 'method node' stamp: 'JEC 30/Mar/2026, 2:06:38 am (UTC)'!
sourceRangesOfParameterizableNodesOf: nodesToLookFor rejectingIntervals: rejectingIntervals
	
	^ self 
		sourceRangesOfParameterizableNodesOf: nodesToLookFor 
		keepingSelection: EquivalentNodesFinder noSelection
		rejectingIntervals: rejectingIntervals! !


!Message methodsFor: 'testing' stamp: 'JEC 3/Mar/2026, 11:20:09 pm (UTC)'!
hasEqualArguments: aCollectionOfArguments

	^ args hasEqualElements: aCollectionOfArguments ! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'JEC 12/Feb/2026, 2:10:08 am (UTC)'!
intervalOfSubCollection: aSubCollectionToFind

	^ self intervalOfSubCollection: aSubCollectionToFind startingAt: 1! !

!methodRemoval: EquivalentNodesFinder class #for:in:keepingSelection: stamp: 'jmv 3/Jul/2026, 7:37:11 pm (UTC)'!
EquivalentNodesFinder class removeSelector: #for:in:keepingSelection:!
!methodRemoval: EquivalentNodesFinder class #noSelection stamp: 'jmv 3/Jul/2026, 7:37:11 pm (UTC)'!
EquivalentNodesFinder class removeSelector: #noSelection!

'From Cuis7.9 [latest update: #8045] on 3 July 2026 at 3:32:54 pm'!
!classDefinition: #NodesFinder category: #'Compiler-Support'!
ParseNodeVisitor subclass: #NodesFinder
	instanceVariableNames: 'nodesToLookFor foundNodes uniqueNodeVisitedCounter nodesToLookForSize methodNode selectionToKeep'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!classDefinition: #ParameterizableNodesFinder category: #'Compiler-Support'!
NodesFinder subclass: #ParameterizableNodesFinder
	instanceVariableNames: 'rejectingIntervals'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!ParseNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025, 11:06:35 pm (UTC)'!
isAssignmentToTemporaryAssigningTo: aTempVarNode

	^ false! !


!AssignmentNode methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 11:05:06 pm (UTC)'!
anySourceRangesOfAssigningValueIn: methodNode satisfy: aBlock 

	| sourceRanges |

	sourceRanges := methodNode completeSourceRangesOf: value ifAbsent: [ ^ false ].
	^ sourceRanges anySatisfy: aBlock. ! !

!AssignmentNode methodsFor: 'testing' stamp: 'JEC 19/Dec/2025, 11:57:24 pm (UTC)'!
assignsToVariable: aVariableNode 

	^ variable equivalentTo: aVariableNode! !

!AssignmentNode methodsFor: 'testing' stamp: 'JEC 21/Dec/2025, 4:24:24 pm (UTC)'!
assignsToVariableName: aName

	^ variable isNamed: aName! !

!AssignmentNode methodsFor: 'testing' stamp: 'JEC 8/Jan/2026, 11:00:08 pm (UTC)'!
isAssigningACascadeNode

	^ value isCascadeNode! !

!AssignmentNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025, 11:07:26 pm (UTC)'!
isAssignmentToTemporaryAssigningTo: aTempVarNode

	^ self isAssignmentToTemporary and: [ self assignsToVariable: aTempVarNode ]! !


!CascadeNode methodsFor: 'testing' stamp: 'JEC 11/Jan/2026, 1:06:06 am (UTC)'!
isEquivalentToAMessageNode: aNode 

	^ messages anySatisfy: [ :aMessageNode | aMessageNode equivalentTo: aNode ]! !

!CascadeNode methodsFor: 'testing' stamp: 'JEC 11/Jan/2026, 1:02:44 am (UTC)'!
isEquivalentToReceiverNode: aNode

	^ receiver equivalentTo: aNode! !


!BlockNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025, 7:57:13 pm (UTC)'!
temporariesDeclarationDeclares: tempVarNode 

	^ self temporariesDeclaration declaresVariable: tempVarNode! !


!MethodNode methodsFor: 'source mapping' stamp: 'JEC 31/Dec/2025, 1:31:18 pm (UTC)'!
completeSourceRangesWithin: anInterval do: aBinaryBlock

	^ self completeSourceRangesDo: [ :parseNode :sourceRanges |
		| sourceRangesInInterval |
		
		sourceRangesInInterval := sourceRanges select: [ :sourceRange | anInterval includes: sourceRange first ].
		
		sourceRangesInInterval 
			ifNotEmpty: [
				aBinaryBlock 
					value: parseNode
					value: sourceRangesInInterval ] ]! !

!MethodNode methodsFor: 'source mapping' stamp: 'JEC 28/Dec/2025, 7:49:06 pm (UTC)'!
nodeAndSourceRangesThatDeclares: tempVarNode

	self completeSourceRangesDo: [ :parseNode :sourceRanges |  
		(parseNode isBlockNode and: [ parseNode temporariesDeclarationDeclares: tempVarNode ])
		ifTrue: [ ^ parseNode -> sourceRanges ] ].

	"If there is not a block node that declares that variable, so the method node is the one"
	^ self -> { 1 to: self sourceText size }! !

!MethodNode methodsFor: 'source mapping' stamp: 'JEC 28/Dec/2025, 12:48:38 am (UTC)'!
sourceRangesAssignmentsTo: tempVarNode

	| assignmentsSourceRanges |
	
	assignmentsSourceRanges := OrderedCollection new.
	
	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isAssignmentNode and: [ parseNode assignsToVariableName: tempVarNode name ])
			ifTrue: [ assignmentsSourceRanges addAll: sourceRanges ] ].
	
	^ assignmentsSourceRanges! !

!MethodNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025, 11:08:13 pm (UTC)'!
anyAssignmentNodeWithin: intervalToExtract assignsTo: tempVarNode 

	^ self
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isAssignmentToTemporaryAssigningTo: tempVarNode ]! !

!MethodNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025, 12:22:51 am (UTC)'!
anyDeclarationNodeWithin: intervalToExtract declaresVariable: tempVarNode

	^ self
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode |
			parseNode isTemporariesDeclaration
				and: [ parseNode declaresVariable: tempVarNode ] ]! !

!MethodNode methodsFor: 'testing' stamp: 'JEC 29/Dec/2025, 12:31:29 am (UTC)'!
hasReturnExpressions

	self completeSourceRangesDo: [ :parseNode :sourceRanges | 
		parseNode isReturn ifTrue: [ ^ true ] ].
	
	^ false! !


!ReturnNode methodsFor: 'printing' stamp: 'JEC 4/Jan/2026, 1:46:12 am (UTC)'!
printExpressionOn: aStream indent: level

	| expressionString shouldAddParenthesis |

	expressionString := String 
		streamContents: [ :expressionStream | expr printOn: expressionStream indent: level ].
		
	shouldAddParenthesis := expr isLiteralNode not 
		and: [ expr isBlockNode not
		and: [ expr isBraceNode not
		and: [ expr isVariableNode not ] ] ].

	aStream 
		nextPut: $( when: shouldAddParenthesis;
		"This is because the MessageNode always print it with new lines."
		nextPutAll: expressionString withBlanksCondensed;
		nextPut: $) when: shouldAddParenthesis.! !


!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 11/Jan/2026, 2:06:36 am (UTC)'!
addEquivalentNodesInCascadeNode: aCascadeNode 

	(self areNodesToLookForAPartiallyCascadeNode: aCascadeNode)
		ifTrue: [ self addToEquivalentsNodesPartiallyOf: aCascadeNode ]
		ifFalse: [ self addToEquivalentsIfOneStatementEquivalentTo: aCascadeNode. ]! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 11/Jan/2026, 2:06:36 am (UTC)'!
addToEquivalentsNodesPartiallyOf: aCascadeNode 

	| intervalToAdd cascadeNodeSourceRange |
	
	cascadeNodeSourceRange := self completeSourceRangeOf: aCascadeNode.
	
	intervalToAdd := (cascadeNodeSourceRange includesAnyOf: selectionToKeep)
		ifTrue: [ selectionToKeep ]
		ifFalse: [
			(cascadeNodeSourceRange first 
				to: (cascadeNodeSourceRange first + selectionToKeep size - 1))
				asSourceCodeInterval ].

	equivalentNodesRanges add: intervalToAdd! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 11/Jan/2026, 2:06:14 am (UTC)'!
areNodesToLookForAPartiallyCascadeNode: aCascadeNode 

	^ nodesToLookFor 
		allSatisfy: [ :aNode | 
			(aCascadeNode isEquivalentToReceiverNode: aNode) 
			or: [ aCascadeNode isEquivalentToAMessageNode: aNode ] ]! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'JEC 4/Jan/2026, 11:28:17 pm (UTC)'!
visitReturnNode: aReturnNode

	self addToEquivalentsIfOneStatementEquivalentTo: aReturnNode.

	 ^ super visitReturnNode: aReturnNode ! !


!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:29:15 pm (UTC)'!
visitAssignmentNode: anAssignmentNode 
	
	self addSingleAssigmentNodeIfNecessary: anAssignmentNode.
	
	^ super visitAssignmentNode: anAssignmentNode! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:29:25 pm (UTC)'!
visitBacktickNode: aBacktickNode 
	
	self addSingleBacktickNodeIfNecessary: aBacktickNode.
	
	^ super visitBacktickNode: aBacktickNode! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:37:48 pm (UTC)'!
visitBlockNode: blockNode

	| blockStatements |

	"See ExtractMethodFinderTest>>#test30DoesNotFailWhenLookingForABlockEquivalentToAMethod"
	blockNode = methodNode block ifFalse: [ self addSingleBlockNodeIfNeccesary: blockNode ].

	super visitBlockNode: blockNode.
	
	nodesToLookForSize > 1 ifTrue: [ 
		blockStatements := blockNode statements copy. 
		"I need to add temp declaration to statements becuase user could have selected the temp declaration
		See 
		- ExtractMethodFinderTest>>test22FindsTemporaryWhenSelectingDefinitionInInnerClosure
		- ExtractMethodFinderTest>>test22_1_FindsTemporaryWhenSelectingDefinitionInInnerClosure - Hernan"
		blockNode declaresAnyVariable ifTrue: [ blockStatements addFirst: blockNode temporariesDeclaration ].
		self addNodesIfAllMatch: blockStatements ]
		! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:29:32 pm (UTC)'!
visitBraceNode: aBraceNode 
	
	uniqueNodeVisitedCounter add: aBraceNode.
	
	self addSingleBraceNodeIfNecessary: aBraceNode.
	
	^ super visitBraceNode: aBraceNode ! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:29:40 pm (UTC)'!
visitCascadeNode: aCascadeNode 

	self addSingleCascadeNodeIfNecessary: aCascadeNode.

	 ^ super visitCascadeNode: aCascadeNode ! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:29:48 pm (UTC)'!
visitInstanceVariableNode: anInstanceVariableNode 

	uniqueNodeVisitedCounter add: anInstanceVariableNode.
	
	self addSingleInstanceVariableNodeIfNecessary: anInstanceVariableNode.

	^ super visitInstanceVariableNode: anInstanceVariableNode ! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:25:52 pm (UTC)'!
visitLiteralNode: aLiteralNode

	uniqueNodeVisitedCounter add: aLiteralNode.
	
	self addSingleLiteralNodeIfNeccesary: aLiteralNode.

	^ super visitLiteralNode: aLiteralNode! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:26:49 pm (UTC)'!
visitLiteralVariableNode: aLiteralVariableNode 
	
	uniqueNodeVisitedCounter add: aLiteralVariableNode.
	
	self addSingleLiteralVariableNodeIfNecessary: aLiteralVariableNode.
	
	^ super visitLiteralVariableNode: aLiteralVariableNode ! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:27:37 pm (UTC)'!
visitMessageNode: aMessageNode

	self addSingleMessageNodeIfNeccesary: aMessageNode.

	 ^ super visitMessageNode: aMessageNode ! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 5/Feb/2026, 12:20:38 am (UTC)'!
visitReturnNode: aReturnNode

	self addSingleReturnNodeIfNeccesary: aReturnNode.

	 ^ super visitReturnNode: aReturnNode ! !

!NodesFinder methodsFor: 'visiting' stamp: 'JEC 26/Jan/2026, 10:28:40 pm (UTC)'!
visitTempVariableNode: aTempVariableNode 
	
	uniqueNodeVisitedCounter add: aTempVariableNode.
	
	self addSingleTempVariableNodeIfNecessary: aTempVariableNode.
	
	^ super visitTempVariableNode: aTempVariableNode ! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:29 pm (UTC)'!
addSingleAssigmentNodeIfNecessary: anAssignmentNode
! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:34 pm (UTC)'!
addSingleBacktickNodeIfNecessary: aBacktickNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:37 pm (UTC)'!
addSingleBlockNodeIfNeccesary: blockNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:40 pm (UTC)'!
addSingleBraceNodeIfNecessary: aBraceNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:43 pm (UTC)'!
addSingleCascadeNodeIfNecessary: aCascadeNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:46 pm (UTC)'!
addSingleInstanceVariableNodeIfNecessary: anInstanceVariableNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:49 pm (UTC)'!
addSingleLiteralNodeIfNeccesary: aLiteralNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:53 pm (UTC)'!
addSingleLiteralVariableNodeIfNecessary: aLiteralVariableNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:56 pm (UTC)'!
addSingleMessageNodeIfNeccesary: aMessageNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 5/Feb/2026, 12:20:52 am (UTC)'!
addSingleReturnNodeIfNeccesary: aReturnNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:49:58 pm (UTC)'!
addSingleTempVariableNodeIfNecessary: aTempVariableNode! !

!NodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 10:56:11 pm (UTC)'!
nodesToLookForMatchingConditionWith: nodesToCompareWith! !

!NodesFinder methodsFor: 'private - visiting' stamp: 'JEC 27/Jan/2026, 12:36:44 pm (UTC)'!
addAllToFoundNodes: nodesToAdd

	self subclassResponsibility! !

!NodesFinder methodsFor: 'private - visiting' stamp: 'JEC 26/Jan/2026, 10:38:54 pm (UTC)'!
addNodesIfAllMatch: blockStatements 

	| start stop |
	
	start := 1.
	
	[ stop := start + nodesToLookForSize - 1.
	stop <= blockStatements size ] whileTrue: [ 
		start := self 
			nextStartAfterAddingFoundNodesOf: blockStatements 
			from: start 
			to: stop ].
		
	! !

!NodesFinder methodsFor: 'private - visiting' stamp: 'JEC 5/Feb/2026, 2:21:14 am (UTC)'!
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

!NodesFinder methodsFor: 'private - visiting' stamp: 'JEC 30/Mar/2026, 12:11:04 am (UTC)'!
completeSourceRangesOfAll: aCollectionOfNodes

	| firstRange lastRange |
	
	firstRange := self completeSourceRangeOf: aCollectionOfNodes first.
	
	"The implict self return is in the AST but does not have a range"
	lastRange := (aCollectionOfNodes last isImplicitSelfReturnIn: methodNode)
		ifTrue: [ self completeSourceRangeOf: (aCollectionOfNodes at: aCollectionOfNodes size -1) ]
		ifFalse: [ self completeSourceRangeOf: aCollectionOfNodes last ].
	
	^SourceCodeInterval from: firstRange first to: lastRange last! !

!NodesFinder methodsFor: 'private - visiting' stamp: 'JEC 26/Mar/2026, 1:02:28 pm (UTC)'!
nextStartAfterAddingFoundNodesOf: blockStatements from: start to: stop 
	
	| nodesToCompareWith nextStart |

	nodesToCompareWith := blockStatements copyFrom: start to: stop.

	nextStart := (self nodesToLookForMatchingConditionWith: nodesToCompareWith) 
		ifTrue: [ 
			self addAllToFoundNodes: nodesToCompareWith.
			stop + 1 ]
		ifFalse: [ 
			start + 1 ].
		
	^nextStart! !

!NodesFinder methodsFor: 'initialization' stamp: 'JEC 26/Jan/2026, 11:40:00 pm (UTC)'!
initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep

	nodesToLookFor := aNodesToLookFor.
	methodNode := aMethodNode.
	selectionToKeep := aSelectionToKeep.
	
	nodesToLookForSize := nodesToLookFor size.
	uniqueNodeVisitedCounter := IdentityBag new.
	foundNodes := OrderedCollection new! !


!NodesFinder class methodsFor: 'instance creation' stamp: 'JEC 26/Jan/2026, 1:00:11 pm (UTC)'!
for: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep

	^ self new initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep! !

!NodesFinder class methodsFor: 'instance creation' stamp: 'JEC 12/Feb/2026, 4:04:59 am (UTC)'!
noSelection
	
	^`0 to: 0`! !


!ParameterizableNodesFinder methodsFor: 'initialization' stamp: 'JEC 29/Mar/2026, 11:54:47 pm (UTC)'!
initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep rejectingIntervals: aCollectionOfRejectingIntervals 
	
	super initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep.
	
	rejectingIntervals := aCollectionOfRejectingIntervals.! !

!ParameterizableNodesFinder methodsFor: 'testing' stamp: 'JEC 12/Feb/2026, 3:01:51 am (UTC)'!
hasFoundParameterizableReplacements

	^ foundNodes notEmpty! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 7/Apr/2026, 3:55:17 pm (UTC)'!
addAllToFoundNodes: nodesToAdd

	| parameterizableNodes |

	parameterizableNodes := OrderedCollection new.

	nodesToAdd with: nodesToLookFor do: [ :nodeToAdd :nodeToLookFor |
		(nodeToAdd hasParameterizableDifferenceWith: nodeToLookFor)
			ifTrue: [ 
				| parameterizableNodesWithRealRanges |
				
				parameterizableNodesWithRealRanges := self
					allParameterizableNodesBetween: nodeToAdd 
					and: nodeToLookFor.
				
				parameterizableNodes addAll: parameterizableNodesWithRealRanges ] ].

	foundNodes add: { 
		nodesToAdd . 
		self completeSourceRangesOfAll: nodesToAdd . 
		parameterizableNodes }! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 24/Mar/2026, 4:24:38 pm (UTC)'!
addSingleFoundNode: aNode

	| nodeSourceRanges parameterizableNodes |

	nodeSourceRanges := self completeSourceRangeOf: aNode.
	parameterizableNodes := self allParameterizableNodesBetween: aNode and: nodesToLookFor first.
	
	foundNodes add: { 
		{ aNode } . 
		nodeSourceRanges . 
		parameterizableNodes }! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 26/Mar/2026, 1:05:20 am (UTC)'!
allArgumentNodesOf: aNode

	| messageNode |

	messageNode := self messageNodeOf: aNode. 

	^ messageNode argumentNodesSourceRangesIn: methodNode! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 7/Apr/2026, 4:15:19 pm (UTC)'!
allParameterizableNodesBetween: aNode and: nodeToLookFor

	| parameterizableNodes |
	
	parameterizableNodes := OrderedCollection new.

	(self shouldAddReceiverOf: aNode asParameterizableComparingWith: nodeToLookFor)
		ifTrue: [ parameterizableNodes add: (self receiverNodeOf: aNode) ].
	
	"Cannot use #allArgumentNodesOf: because it looks for all argument nodes and their
	ranges, but the nodeToLookFor can be a node from another method"
	(self argumentsFromNodeToLookFor: nodeToLookFor)
		with: (self allArgumentNodesOf: aNode)
		do: [ :argNodeToLookFor :argFromNode |
			(argNodeToLookFor equivalentTo: argFromNode key) not
				ifTrue: [ parameterizableNodes add: argFromNode ] ].
	
	^ parameterizableNodes! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 5/Feb/2026, 11:27:20 pm (UTC)'!
areAllNodesInMethodNode: aCollectionOfNodes

	| methodNodeNodes |
	
	methodNodeNodes := methodNode completeSourceRanges keys.

	^ aCollectionOfNodes 
		allSatisfy: [ :aNode | methodNodeNodes includes: aNode ]! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 17/Mar/2026, 11:13:12 pm (UTC)'!
argumentsFromNodeToLookFor: nodeToLookFor

	^ (self messageNodeOf: nodeToLookFor) arguments! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 7/Apr/2026, 3:55:17 pm (UTC)'!
hasOneNodeToLookForAndParameterizableWith: aNode

	^ nodesToLookForSize = 1 
		and: [ (self nodesAreFromRejectingIntervals: { aNode }) not
		and: [ nodesToLookFor first hasParameterizableDifferenceWith: aNode ] ] ! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 7/Mar/2026, 4:46:34 am (UTC)'!
haveNodesToLookFor: nodesToCompareWith 

	"Needs to check if all nodes are in the method node because one can be an implicit self return 
	and not have source range"
	^ self isLookingInMethodNode 
		and: [ (self areAllNodesInMethodNode: nodesToCompareWith)
		and: [
			((self completeSourceRangesOfAll: nodesToCompareWith) 
				intersection: selectionToKeep)
				notEmpty ] ]! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 12/Feb/2026, 4:05:30 am (UTC)'!
isLookingInMethodNode

	^ (selectionToKeep = self class noSelection) not ! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 17/Mar/2026, 11:13:46 pm (UTC)'!
messageNodeOf: aNode

	^ aNode isMessageNode
		ifTrue: [ aNode ]
		ifFalse: [ aNode value ]. "Here the node is an assignment node and the value assigning is a message node"! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 30/Mar/2026, 12:02:39 am (UTC)'!
nodesAreFromRejectingIntervals: nodesToCompare 

	| nodesToCompareRange |

	nodesToCompareRange := self completeSourceRangesOfAll: nodesToCompare.
	
	^ rejectingIntervals anySatisfy: [ :rejectingInterval |
		rejectingInterval intersectsRange: nodesToCompareRange ]! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 9/Mar/2026, 11:26:52 pm (UTC)'!
parameterizableNodeWithRealRange: parameterizableNode using: nodeSourceRange

	methodNode 
		completeSourceRangesWithin: nodeSourceRange 
		do: [ :parseNode :sourceRanges | 
			| rangeIndex |

			((parseNode equivalentTo: parameterizableNode key)
				and: [ (rangeIndex := sourceRanges findFirst: [ :r | r includesAnyOf: parameterizableNode value])
						isZero not ])
				ifTrue: [ ^ parameterizableNode key -> (sourceRanges at: rangeIndex) ] ].
	
	self shouldNotHappen! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 6/Apr/2026, 4:39:30 pm (UTC)'!
receiverNodeOf: aNode 

	| messageNode |

	messageNode := self messageNodeOf: aNode.
	
	^ messageNode receiverNodeSourceRangesIn: methodNode! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 7/Apr/2026, 2:52:01 pm (UTC)'!
shouldAddReceiverOf: aNode asParameterizableComparingWith: nodeToLookFor

	| aReceiverNode aReceiverNodeToLookFor |

	aReceiverNode := (self receiverNodeOf: aNode) key.
	aReceiverNodeToLookFor := (self receiverNodeOf: nodeToLookFor) key.

	^  (aReceiverNode isVariableNode 
		and: [ aReceiverNode isTemp or: [ aReceiverNode isUndeclared ] ])
		or: [ aReceiverNodeToLookFor isVariableNode 
			and: [ aReceiverNodeToLookFor isTemp or: [ aReceiverNodeToLookFor isUndeclared ] ] ]! !

!ParameterizableNodesFinder methodsFor: 'accessing' stamp: 'JEC 26/Jan/2026, 11:47:43 pm (UTC)'!
parameterizableNodes

	^ foundNodes! !

!ParameterizableNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 6/Apr/2026, 7:43:41 pm (UTC)'!
addNodeIfFoundAsParameterizable: aNode

	(self hasOneNodeToLookForAndParameterizableWith: aNode)
		ifTrue: [ self addSingleFoundNode: aNode ].! !

!ParameterizableNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 26/Jan/2026, 11:09:46 pm (UTC)'!
addSingleAssigmentNodeIfNecessary: anAssignmentNode
	
	self addNodeIfFoundAsParameterizable: anAssignmentNode! !

!ParameterizableNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 27/Jan/2026, 12:27:37 pm (UTC)'!
addSingleMessageNodeIfNeccesary: aMessageNode

	self addNodeIfFoundAsParameterizable: aMessageNode! !

!ParameterizableNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'JEC 29/Mar/2026, 11:59:20 pm (UTC)'!
nodesToLookForMatchingConditionWith: nodesToCompareWith

	| parameterizableNodesFound |
	
	parameterizableNodesFound := OrderedCollection new.
	
	((self haveNodesToLookFor: nodesToCompareWith) 
		or: [ self nodesAreFromRejectingIntervals: nodesToCompareWith ])
		ifTrue: [ ^ false ].

	nodesToCompareWith 
		with: nodesToLookFor 
		do: [ :nodeToCompare :nodeToLookFor |
			nodeToCompare 
				compareTo: nodeToLookFor
				ifEquivalentDo: [ :equivalentNode | ]
				ifParameterizableDo: [ :parameterizableNode | parameterizableNodesFound add: parameterizableNode ]
				ifNoneDo: [ ^ false ] ].

	^ parameterizableNodesFound notEmpty! !


!ParameterizableNodesFinder class methodsFor: 'instance creation' stamp: 'JEC 30/Mar/2026, 12:13:19 am (UTC)'!
for: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep 

	^ self 
		for: aNodesToLookFor 
		in: aMethodNode 
		keepingSelection: aSelectionToKeep 
		rejectingIntervals: #()! !

!ParameterizableNodesFinder class methodsFor: 'instance creation' stamp: 'JEC 29/Mar/2026, 11:53:31 pm (UTC)'!
for: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep rejectingIntervals: rejectingIntervals 

	^ self new 
		initializeFor: aNodesToLookFor 
		in: aMethodNode 
		keepingSelection: aSelectionToKeep 
		rejectingIntervals: rejectingIntervals ! !


!CompiledMethod methodsFor: 'accessing' stamp: 'JEC 13/Apr/2026, 4:58:03 pm (UTC)'!
messageFromSelector

	^ Message 
		selector: self selector
		arguments: (self methodNode arguments collect: [ :argument | argument name ])! !


!Stream methodsFor: 'writing' stamp: 'JEC 18/Dec/2025, 12:55:11 pm (UTC)'!
nextPutAllValueOf: aBlock when: aCondition

	aCondition ifTrue: [ self nextPutAll: aBlock value ].
	! !


!Message methodsFor: 'accessing' stamp: 'JEC 29/Dec/2025, 11:33:12 pm (UTC)'!
addKeyword: aNewSymbol withArgument: aNewArgument 

	| newSelector newArguments |

	newSelector := (self selector , aNewSymbol) asSymbol.
	
	newArguments := self arguments 
		asOrderedCollection
		add: aNewArgument;
		yourself.

	self 
		setSelector: newSelector
		arguments: newArguments! !

!Message methodsFor: 'testing' stamp: 'JEC 16/Dec/2025, 9:36:07 pm (UTC)'!
hasArgument: anArgument

	^ self arguments anySatisfy: [ :argument | argument = anArgument ]! !


!MethodReference methodsFor: 'printing' stamp: 'JEC 13/Apr/2026, 11:28:21 pm (UTC)'!
classAndSelector

	^ String streamContents: [ :stream | self printClassAndSelectorOn: stream ]! !


!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'JEC 11/Jan/2026, 2:04:22 am (UTC)'!
visitCascadeNode: aCascadeNode 

	self addEquivalentNodesInCascadeNode: aCascadeNode.

	 ^super visitCascadeNode: aCascadeNode ! !


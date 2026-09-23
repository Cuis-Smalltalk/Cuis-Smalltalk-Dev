'From Cuis7.9 [latest update: #8184] on 22 September 2026 at 3:45:17 pm'!
!classDefinition: #ParameterizableNodesFinder category: #'Compiler-Support'!
NodesFinder subclass: #ParameterizableNodesFinder
	instanceVariableNames: 'rejectingIntervals temporariesRenaming '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!classDefinition: #ParameterizableDifferencesFinder category: #'Tools-Refactoring'!
Object subclass: #ParameterizableDifferencesFinder
	instanceVariableNames: 'methodNode renaming differences'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration extractMethodProgrammer newCompiledMethod parameterizableNodes expressionReplacement sourceCodeSizeChange '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 22/Sep/2026, 4:20:13 pm (UTC)'!
differencesBetween: aNode at: aRange and: aNodeToLookFor

	^ ParameterizableDifferencesFinder
		between: aNode
		at: aRange
		and: aNodeToLookFor
		in: methodNode
		renaming: temporariesRenaming! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 22/Sep/2026, 4:20:13 pm (UTC)'!
is: aNode sameAs: aNodeToLookFor

	"The implicit self return has no source range, and is equivalent to itself alone"

	(aNode equivalentTo: aNodeToLookFor) ifTrue: [ ^ true ].

	^ (methodNode completeSourceRanges includesKey: aNode)
		and: [ (self differencesBetween: aNode at: (self completeSourceRangeOf: aNode) and: aNodeToLookFor) isEmpty ]! !


!ExtractMethodProgrammer methodsFor: 'initialization' stamp: 'HAW 22/Sep/2026, 5:59:40 pm (UTC)'!
moveBy: anAdjustment

	"The method was written again by a replacement applied before mine, which moved my piece and
	changed the method, so what I know of it is taken again from the method as it is now"

	intervalToExtract := (intervalToExtract + anAdjustment) asSourceCodeInterval.
	parameterizableNodes := parameterizableNodes collect: [ :nodeAndRange | nodeAndRange key -> (nodeAndRange value + anAdjustment) ].
	methodNode := compiledMethod methodNode.
	self initializeExtractedModifiers! !


!ParameterizableDifferencesFinder methodsFor: 'initialization' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
initializeIn: aMethodNode renaming: aRenaming

	methodNode := aMethodNode.
	renaming := aRenaming.
	differences := OrderedCollection new! !

!ParameterizableDifferencesFinder methodsFor: 'finding' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
differencesBetween: aNode at: aRange and: aNodeToLookFor

	"The nodes of aNode that differ from aNodeToLookFor, each with its range, once the temporaries
	renamed are taken as the same. A block that differs inside is answered whole, since what it holds
	is not evaluated where it is written."

	self compare: aNode at: aRange with: aNodeToLookFor.

	^differences! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:18:06 pm (UTC)'!
areAssignments: aNode and: aNodeToLookFor

	^aNode isAssignmentNode
		and: [ aNodeToLookFor isAssignmentNode
		and: [ (aNode variable equivalentTo: aNodeToLookFor variable) or: [ self isRenamed: aNode variable as: aNodeToLookFor variable ]]]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
areBraces: aNode and: aNodeToLookFor

	^aNode isBraceNode
		and: [ aNodeToLookFor isBraceNode
		and: [ aNode elements size = aNodeToLookFor elements size ]]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
areCascades: aNode and: aNodeToLookFor

	^aNode isCascadeNode
		and: [ aNodeToLookFor isCascadeNode
		and: [ aNode messages size = aNodeToLookFor messages size
		and: [ aNode messages with: aNodeToLookFor messages allSatisfy: [ :eachMessage :eachMessageToLookFor | eachMessage selector key = eachMessageToLookFor selector key ]]]]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
areSends: aNode and: aNodeToLookFor

	^aNode isMessageNode
		and: [ aNodeToLookFor isMessageNode
		and: [ aNode receiver notNil
		and: [ aNodeToLookFor receiver notNil
		and: [ aNode selector key = aNodeToLookFor selector key ]]]]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:18:06 pm (UTC)'!
compare: aNode at: aRange with: aNodeToLookFor

	(self is: aNode at: aRange sameAs: aNodeToLookFor) ifTrue: [ ^self ].
	(self areSends: aNode and: aNodeToLookFor) ifTrue: [ ^self compareSend: aNode at: aRange with: aNodeToLookFor ].
	(self areCascades: aNode and: aNodeToLookFor) ifTrue: [ ^self compareCascade: aNode at: aRange with: aNodeToLookFor ].
	(self areAssignments: aNode and: aNodeToLookFor) ifTrue: [ ^self compareAssignment: aNode at: aRange with: aNodeToLookFor ].
	(aNode isReturn and: [ aNodeToLookFor isReturn ]) ifTrue: [ ^self compareNested: aNode expr within: aRange with: aNodeToLookFor expr ].
	(self areBraces: aNode and: aNodeToLookFor) ifTrue: [ ^self compareBrace: aNode at: aRange with: aNodeToLookFor ].

	differences add: aNode -> aRange! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:21:59 pm (UTC)'!
compareArgumentsOf: aSend with: aSendToLookFor

	aSend arguments ifEmpty: [ ^self ].

	(aSend argumentNodesSourceRangesIn: methodNode)
		with: aSendToLookFor arguments
		do: [ :eachArgumentAndRange :eachArgumentToLookFor |
			self compare: eachArgumentAndRange key at: eachArgumentAndRange value with: eachArgumentToLookFor ]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
compareAssignment: anAssignment at: aRange with: anAssignmentToLookFor

	self compareNested: anAssignment value within: aRange with: anAssignmentToLookFor value! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
compareBrace: aBrace at: aRange with: aBraceToLookFor

	| previousEnd |

	previousEnd := aRange first.
	aBrace elements
		with: aBraceToLookFor elements
		do: [ :eachElement :eachElementToLookFor | | elementRange |
			elementRange := self rangeOf: eachElement within: aRange after: previousEnd.
			previousEnd := elementRange last.
			self compare: eachElement at: elementRange with: eachElementToLookFor ]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
compareCascade: aCascade at: aRange with: aCascadeToLookFor

	self compareNested: aCascade receiver within: aRange with: aCascadeToLookFor receiver.
	aCascade messages
		with: aCascadeToLookFor messages
		do: [ :eachMessage :eachMessageToLookFor | self compareArgumentsOf: eachMessage with: eachMessageToLookFor ]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
compareNested: aNode within: aRange with: aNodeToLookFor

	self compare: aNode at: (self rangeOf: aNode within: aRange) with: aNodeToLookFor! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
compareSend: aSend at: aRange with: aSendToLookFor

	| receiverAndRange |

	receiverAndRange := aSend receiverNodeSourceRangesIn: methodNode.
	self compare: receiverAndRange key at: receiverAndRange value with: aSendToLookFor receiver.
	self compareArgumentsOf: aSend with: aSendToLookFor! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:18:06 pm (UTC)'!
is: aNode at: aRange sameAs: aNodeToLookFor

	(aNode equivalentTo: aNodeToLookFor) ifTrue: [ ^true ].
	(self isRenamed: aNode as: aNodeToLookFor) ifTrue: [ ^true ].

	^aNode isBlockNode and: [ aNodeToLookFor isBlockNode and: [ self isBlock: aNode at: aRange sameAs: aNodeToLookFor ]]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:23:40 pm (UTC)'!
isBlock: aBlock at: aRange sameAs: aBlockToLookFor

	| previousEnd |

	((aBlock hasEquivalentArgumentsWith: aBlockToLookFor)
		and: [ (aBlock hasEquivalentTemporariesDeclarationWith: aBlockToLookFor)
		and: [ aBlock statements size = aBlockToLookFor statements size ]]) ifFalse: [ ^false ].

	previousEnd := aRange first.
	^aBlock statements
		with: aBlockToLookFor statements
		allSatisfy: [ :eachStatement :eachStatementToLookFor | | statementRange |
			statementRange := self rangeOf: eachStatement within: aRange after: previousEnd.
			previousEnd := statementRange last.
			(self class between: eachStatement at: statementRange and: eachStatementToLookFor in: methodNode renaming: renaming) isEmpty ]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
isRenamed: aNode as: aNodeToLookFor

	^aNode isTemp
		and: [ aNodeToLookFor isTemp
		and: [ (renaming at: aNode name ifAbsent: [ aNode name ]) = aNodeToLookFor name ]]! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
rangeOf: aNode within: aRange

	^self rangeOf: aNode within: aRange after: aRange first - 1! !

!ParameterizableDifferencesFinder methodsFor: 'finding - private' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
rangeOf: aNode within: aRange after: aPosition

	"A literal or self is one node wherever it is written, so the range wanted is the first inside
	aRange past aPosition"

	^((methodNode completeSourceRangesOf: aNode ifAbsent: [ self shouldNotHappen ])
		asSortedCollection: [ :leftRange :rightRange | leftRange first < rightRange first ])
		detect: [ :eachRange | eachRange first > aPosition and: [ aRange includesAllOf: eachRange ]]! !


!ParameterizableDifferencesFinder class methodsFor: 'instance creation' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
in: aMethodNode renaming: aRenaming

	^self new initializeIn: aMethodNode renaming: aRenaming! !

!ParameterizableDifferencesFinder class methodsFor: 'instance creation' stamp: 'HAW 22/Sep/2026, 4:16:35 pm (UTC)'!
namesOfTemporariesAssignedIn: someNodes

	| names |

	names := OrderedCollection new.
	someNodes do: [ :aNode |
		aNode nodesDo: [ :eachNode |
			(eachNode isAssignmentNode and: [ eachNode variable isTemp and: [ (names includes: eachNode variable name) not ]])
				ifTrue: [ names add: eachNode variable name ]]].

	^names! !

!ParameterizableDifferencesFinder class methodsFor: 'instance creation' stamp: 'HAW 22/Sep/2026, 4:17:29 pm (UTC)'!
renamingOf: someNodes to: someNodesToLookFor

	"The temporaries a piece assigns are its own, so the ones of two pieces are the same when they are
	assigned in the same order, whatever they are called"

	| renaming names namesToLookFor |

	renaming := Dictionary new.
	names := self namesOfTemporariesAssignedIn: someNodes.
	namesToLookFor := self namesOfTemporariesAssignedIn: someNodesToLookFor.
	1 to: (names size min: namesToLookFor size) do: [ :index |
		renaming at: (names at: index) put: (namesToLookFor at: index) ].

	^renaming! !

!ParameterizableDifferencesFinder class methodsFor: 'finding' stamp: 'HAW 22/Sep/2026, 4:20:13 pm (UTC)'!
between: aNode at: aRange and: aNodeToLookFor in: aMethodNode renaming: aRenaming

	^(self in: aMethodNode renaming: aRenaming) differencesBetween: aNode at: aRange and: aNodeToLookFor! !


!ExtractMethodParameterizableReplacement methodsFor: 'testing' stamp: 'HAW 22/Sep/2026, 3:59:31 pm (UTC)'!
anyParameterReadsATemporaryAssignedInThePiece

	"Such a parameter would be evaluated by the sender before the extracted method assigns the temporary"

	| assignedNames |

	assignedNames := self namesOfTemporariesAssignedInThePiece.

	^parameterizableNodes anySatisfy: [ :parameterizableNode |
		self does: parameterizableNode key readAnyOf: assignedNames ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'HAW 22/Sep/2026, 4:50:00 pm (UTC)'!
differencesWith: anotherParameterizableReplacement

	"The nodes of my arguments that differ from those of the other piece, taking the temporaries each
	piece assigns as the same"

	| renaming methodNode differences |

	renaming := ParameterizableDifferencesFinder renamingOf: nodes to: anotherParameterizableReplacement nodes.
	methodNode := methodToExtractFrom notOptimizedMethodNode.
	differences := OrderedCollection new.
	nodesWithArguments 
		with: anotherParameterizableReplacement nodesWithArguments 
		do: [ :nodeWithArgs :anotherNodeWithArgs |
			nodeWithArgs value
				with: anotherNodeWithArgs value
				do: [ :argNode :anotherArgNode |
					differences addAll: (ParameterizableDifferencesFinder
						between: argNode key
						at: argNode value
						and: anotherArgNode key
						in: methodNode
						renaming: renaming) ]].

	^ differences! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'HAW 22/Sep/2026, 3:59:31 pm (UTC)'!
does: aParseNode readAnyOf: someTemporaryNames

	aParseNode nodesDo: [ :eachNode |
		(eachNode isTemp and: [ someTemporaryNames includes: eachNode name ]) ifTrue: [ ^true ]].

	^false! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'HAW 22/Sep/2026, 3:59:31 pm (UTC)'!
namesOfTemporariesAssignedInThePiece

	| assignedNames |

	assignedNames := Set new.
	nodes do: [ :aNode |
		aNode nodesDo: [ :eachNode |
			(eachNode isAssignmentNode and: [ eachNode variable isTemp ])
				ifTrue: [ assignedNames add: eachNode variable name ]]].

	^assignedNames! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'HAW 22/Sep/2026, 4:50:00 pm (UTC)'!
nodes

	^ nodes! !


!ParameterizableNodesFinder methodsFor: 'testing' stamp: 'HAW 22/Sep/2026, 4:19:12 pm (UTC)'!
hasOneNodeToLookForAndParameterizableWith: aNode

	temporariesRenaming := ParameterizableDifferencesFinder renamingOf: { aNode } to: nodesToLookFor.

	^ nodesToLookForSize = 1 
		and: [ (self nodesAreFromRejectingIntervals: { aNode }) not
		and: [ nodesToLookFor first hasParameterizableDifferenceWith: aNode ] ] ! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 22/Sep/2026, 4:19:12 pm (UTC)'!
addAllToFoundNodes: nodesToAdd

	| parameterizableNodes |

	parameterizableNodes := OrderedCollection new.

	nodesToAdd with: nodesToLookFor do: [ :nodeToAdd :nodeToLookFor |
		(nodeToAdd hasParameterizableDifferenceWith: nodeToLookFor)
			ifTrue: [ parameterizableNodes addAll: (self allParameterizableNodesBetween: nodeToAdd and: nodeToLookFor) ] ].

	foundNodes add: { 
		nodesToAdd . 
		self completeSourceRangesOfAll: nodesToAdd . 
		parameterizableNodes }! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 22/Sep/2026, 4:19:12 pm (UTC)'!
addSingleFoundNode: aNode

	foundNodes add: { 
		{ aNode } . 
		self completeSourceRangeOf: aNode . 
		self allParameterizableNodesBetween: aNode and: nodesToLookFor first }! !

!ParameterizableNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 22/Sep/2026, 4:20:13 pm (UTC)'!
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

!ParameterizableNodesFinder methodsFor: 'visiting - finding nodes' stamp: 'HAW 22/Sep/2026, 4:19:12 pm (UTC)'!
nodesToLookForMatchingConditionWith: nodesToCompareWith

	| parameterizableNodesFound |
	
	parameterizableNodesFound := OrderedCollection new.
	
	((self haveNodesToLookFor: nodesToCompareWith) 
		or: [ self nodesAreFromRejectingIntervals: nodesToCompareWith ])
		ifTrue: [ ^ false ].

	temporariesRenaming := ParameterizableDifferencesFinder renamingOf: nodesToCompareWith to: nodesToLookFor.
	nodesToCompareWith 
		with: nodesToLookFor 
		do: [ :nodeToCompare :nodeToLookFor |
			(self is: nodeToCompare sameAs: nodeToLookFor) ifFalse: [
				nodeToCompare 
					compareTo: nodeToLookFor
					ifEquivalentDo: [ :equivalentNode | ]
					ifParameterizableDo: [ :parameterizableNode | parameterizableNodesFound add: parameterizableNode ]
					ifNoneDo: [ ^ false ] ]].

	^ parameterizableNodesFound notEmpty! !


!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'HAW 22/Sep/2026, 5:41:48 pm (UTC)'!
allArgumentsToParameterizeInOrderUsing: parseNodesToParameterize 

	| argumentsToParameterize |
	
	argumentsToParameterize := SortedCollection sortBlock: self sortBlockByRange.
	argumentsToParameterize addAll: parameterizableNodes.

	nodesWithArguments do: [ :nodeWithArgs |
		nodeWithArgs value do: [ :argNode |
			((parseNodesToParameterize anySatisfy: [ :parseNode | parseNode equivalentTo: argNode key ])
				and: [ argumentsToParameterize noneSatisfy: [ :argument | argument value intersectsRange: argNode value ] ])
					ifTrue: [ argumentsToParameterize add: argNode ] ] ].
	
	^ argumentsToParameterize collect: [ :argToParam |
		argToParam key -> (methodToExtractFrom sourceCode
			copyFrom: argToParam value first 
			to: argToParam value last) ]! !

!ExtractMethodParameterizableReplacement methodsFor: 'converting' stamp: 'HAW 22/Sep/2026, 4:50:00 pm (UTC)'!
createWithSameParametersAs: anotherParameterizableReplacement

	| newParameterizableNodes |

	(self representSamePieceOfCodeWith: anotherParameterizableReplacement)
		ifTrue: [ ^ self ].
	
	newParameterizableNodes := self parameterizableNodesSortedByInterval.
	
	paramReceivers
		with: anotherParameterizableReplacement paramReceivers
		do: [ :nodeWithParamReceiver :anotherNodeWithParamReceiver |
			(nodeWithParamReceiver value isNil and: [ anotherNodeWithParamReceiver value notNil ])
				ifTrue: [ newParameterizableNodes add: (self receiverNodeOf: nodeWithParamReceiver key) ] ].

	(self differencesWith: anotherParameterizableReplacement) do: [ :differenceAndRange |
		(newParameterizableNodes noneSatisfy: [ :oldParamNode | differenceAndRange value intersectsRange: oldParamNode value ])
			ifTrue: [ newParameterizableNodes add: differenceAndRange ] ].
	
	^ self class 
		for: nodes 
		interval: nodeInterval 
		parameterizableNodes: newParameterizableNodes
		in: methodToExtractFrom! !


!ExtractMethodReplacement methodsFor: 'applying' stamp: 'HAW 22/Sep/2026, 5:43:54 pm (UTC)'!
apply

	| updatedSourceCode |

	self initializeSendCollaboration.
	expressionReplacement := ExpressionReplacement
		of: intervalToExtract
		in: methodToExtractFrom notOptimizedMethodNode
		by: sendCollaboration
		withPrecedence: extractMethodProgrammer sendCollaborationPrecedence.
	updatedSourceCode := extractMethodProgrammer
		removeTemporaryDeclarationsIfNecessaryIn: expressionReplacement value.
	sourceCodeSizeChange := updatedSourceCode size - methodToExtractFrom sourceCode size.

	self sourceClass
		compile: updatedSourceCode
		classified: methodToExtractFrom category! !

!ExtractMethodReplacement methodsFor: 'applying' stamp: 'HAW 22/Sep/2026, 5:59:40 pm (UTC)'!
applyAdjusting: anAdjustment

	intervalToExtract := (intervalToExtract + anAdjustment) asSourceCodeInterval.
	extractMethodProgrammer moveBy: anAdjustment.
	self apply ! !

!ExtractMethodReplacement methodsFor: 'adjustment' stamp: 'HAW 22/Sep/2026, 5:43:54 pm (UTC)'!
adjustmentForNextReplacement

	"The declaration of a temporary the new method took with it goes as well, and it is written before the next replacement"

	^sourceCodeSizeChange! !


!ExtractMethodReplacementsFinder methodsFor: 'private-parameterizable code finding' stamp: 'HAW 22/Sep/2026, 3:59:31 pm (UTC)'!
canAddNewParamReplacement: newParamReplacement 

	^ (self intersectsWithAnyParamReplacement: newParamReplacement) not
		and: [ newParamReplacement anyParameterContainsReturnOrAssignment not
		and: [ newParamReplacement anyParameterReadsATemporaryAssignedInThePiece not ]]! !

!methodRemoval: ExtractMethodParameterizableReplacement #with:with:detectAll:ifFound: stamp: 'HAW 22/Sep/2026, 4:50:00 pm (UTC)'!
ExtractMethodParameterizableReplacement removeSelector: #with:with:detectAll:ifFound:!
!methodRemoval: ParameterizableDifferencesFinder #is:sameAs: stamp: 'HAW 22/Sep/2026, 4:18:06 pm (UTC)'!
ParameterizableDifferencesFinder removeSelector: #is:sameAs:!
!methodRemoval: ParameterizableDifferencesFinder #isBlock:sameAs: stamp: 'HAW 22/Sep/2026, 4:18:06 pm (UTC)'!
ParameterizableDifferencesFinder removeSelector: #isBlock:sameAs:!
!methodRemoval: ParameterizableNodesFinder #parameterizableNodesBetween:and: stamp: 'HAW 22/Sep/2026, 4:20:13 pm (UTC)'!
ParameterizableNodesFinder removeSelector: #parameterizableNodesBetween:and:!
!classDefinition: #ParameterizableNodesFinder category: #'Compiler-Support'!
NodesFinder subclass: #ParameterizableNodesFinder
	instanceVariableNames: 'rejectingIntervals temporariesRenaming'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!ParameterizableNodesFinder reorganize!
('initialization' initializeFor:in:keepingSelection:rejectingIntervals:)
('testing' hasFoundParameterizableReplacements hasOneNodeToLookForAndParameterizableWith:)
('private - visiting' addAllToFoundNodes: addSingleFoundNode: allArgumentNodesOf: allParameterizableNodesBetween:and: areAllNodesInMethodNode: argumentsFromNodeToLookFor: differencesBetween:at:and: haveNodesToLookFor: is:sameAs: isLookingInMethodNode messageNodeOf: nodesAreFromRejectingIntervals: parameterizableNodeWithRealRange:using: receiverNodeOf: shouldAddReceiverOf:asParameterizableComparingWith:)
('accessing' parameterizableNodes)
('visiting - finding nodes' addNodeIfFoundAsParameterizable: addSingleAssigmentNodeIfNecessary: addSingleMessageNodeIfNeccesary: nodesToLookForMatchingConditionWith:)
!

!classDefinition: #ExtractMethodReplacement category: #'Tools-Refactoring'!
Refactoring subclass: #ExtractMethodReplacement
	instanceVariableNames: 'intervalToExtract newMessage methodToExtractFrom sendCollaboration extractMethodProgrammer newCompiledMethod parameterizableNodes expressionReplacement sourceCodeSizeChange'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

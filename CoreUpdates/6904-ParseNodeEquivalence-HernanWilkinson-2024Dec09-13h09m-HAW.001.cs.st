'From Cuis7.3 [latest update: #6899] on 9 December 2024 at 1:17:25 pm'!
!classDefinition: #EquivalentNodesFinder category: #'Compiler-Support'!
ParseNodeVisitor subclass: #EquivalentNodesFinder
	instanceVariableNames: 'nodesToLookFor equivalentNodesRanges nodesToLookForSize nodeVisitedCounter methodNode selectionToKeep'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!

!CompiledMethod methodsFor: 'method node' stamp: 'HAW 11/2/2024 10:19:17'!
methodNodeForLookingEquivalentNodes

	^ self methodNodeWithoutImplicitSelfReturn movingTemporariesDeclarationToBlock! !

!CompiledMethod methodsFor: 'method node' stamp: 'HAW 10/30/2024 09:30:26'!
methodNodeWithoutImplicitSelfReturn
	
	^self methodNode withoutImplicitSelfReturn ! !

!CompiledMethod methodsFor: 'method node' stamp: 'HAW 11/2/2024 10:12:37'!
sourceRangesOfNodesEquivalentTo: nodesToLookFor 
	
	^self sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: EquivalentNodesFinder noSelection! !

!CompiledMethod methodsFor: 'method node' stamp: 'HAW 11/2/2024 19:25:36'!
sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: aSelectionToKeep
	
	^self methodNodeForLookingEquivalentNodes sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: aSelectionToKeep ! !


!ParseNode methodsFor: 'testing' stamp: 'HAW 10/30/2024 08:02:23'!
areNodesOf: leftNodes equivalentTo: rightNodes

	^leftNodes size = rightNodes size and: [ 
		leftNodes with: rightNodes allSatisfy: [ :leftNode :rightNode | leftNode equivalentTo: rightNode ]]! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 10/30/2024 09:27:03'!
isImplicitSelfReturnIn: aMethodNode

	^false! !


!BlockNode methodsFor: 'initialization' stamp: 'HAW 10/30/2024 09:27:45'!
removeImplicitSelfReturnWhenIn: aMethodNode 
	
	| potentiaImplicitSelfReturn |
	
	potentiaImplicitSelfReturn := statements lastIfEmpty: [ ^self ].
	(potentiaImplicitSelfReturn isImplicitSelfReturnIn: aMethodNode) ifTrue: [ statements removeLast ]
	! !

!BlockNode methodsFor: 'testing' stamp: 'HAW 10/31/2024 16:15:24'!
declaresAnyVariable
	
	^temporariesDeclaration notNil and: [ temporariesDeclaration declaresAnyVariable]! !


!MethodNode methodsFor: 'converting' stamp: 'HAW 10/30/2024 09:22:34'!
withoutImplicitSelfReturn
	
	block removeImplicitSelfReturnWhenIn: self.
	^self! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 10/29/2024 22:57:09'!
calculateCompleteSourceRangesOf: nodes	
	
	| sourcesRanges firstNode lastNode interval |
	
	sourcesRanges := nodes collect: [:node | 	self completeSourceRanges at: node ifAbsent: [#()]].
	
	sourcesRanges size = 1 ifTrue: [ ^sourcesRanges first ].
	
	firstNode := sourcesRanges first.
	lastNode := sourcesRanges last.
	
	interval := SourceCodeInterval from: firstNode first first to: lastNode first last.
	
	^OrderedCollection with: interval

! !

!MethodNode methodsFor: 'source mapping' stamp: 'HAW 11/2/2024 19:56:44'!
sourceRangesOfNodesEquivalentTo: nodesToLookFor keepingSelection: aSelectionToKeep
	
	| finder |
	
	finder := EquivalentNodesFinder for: nodesToLookFor in: self keepingSelection: aSelectionToKeep.
	self accept: finder.

	^finder equivalentNodesRanges! !

!MethodNode methodsFor: 'initialization' stamp: 'HAW 11/2/2024 10:13:43'!
movingTemporariesDeclarationToBlock
	
	block temporariesDeclaration: temporariesDeclaration.
	self temporariesDeclaration: TemporariesDeclarationNode empty.
	
	^self ! !


!TemporariesDeclarationNode methodsFor: 'initialization' stamp: 'HAW 10/31/2024 18:34:37'!
withOutDeclarations: aTemporariesDeclarationToRemove

	tempDeclarationNodes := tempDeclarationNodes reject: [ :aTempDeclarationNode | 
		aTemporariesDeclarationToRemove declaresVariable: aTempDeclarationNode variableNode ]! !


!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 10/31/2024 20:45:33'!
addEquivalentNodesOf: blockStatements 

	| start stop |
	
	start := 1.
	
	[ stop := start + nodesToLookForSize - 1.
	stop <= blockStatements size ] whileTrue: [ 
		start := self 
			nextStartAfterAddingEquivalentNodesOf: blockStatements 
			from: start 
			to: stop ].
		
	! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 11/2/2024 19:57:35'!
addToEquivalentsIfOneStatementEquivalentTo: aNode

	(self hasOneNodeToLookForAndEquivalentTo: aNode)
		ifTrue: [ equivalentNodesRanges add: (self completeSourceRangeOf: aNode) ].! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 11/2/2024 20:04:10'!
areNodesToLookForEquivalentTo: nodesToCompareWith
 
	| equivalent |
	
	equivalent := nodesToLookFor 
		with: nodesToCompareWith 
		allSatisfy: [:leftNode :rightNode | leftNode equivalentTo: rightNode ].
		
	^equivalent and: [ self shouldKeepSelection not or: [ self selectionToKeepIsEqualOrHasNoIntersectionWith: nodesToCompareWith ]]	! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 11/4/2024 18:42:07'!
completeSourceRangeOf: aNode

	| sourceRangeIndex completeSourceRanges completeRange trimmedRange |
	
	sourceRangeIndex := nodeVisitedCounter occurrencesOf: aNode.
	sourceRangeIndex = 0 ifTrue: [ sourceRangeIndex := 1 ].
	
	"It should always find the completeSourceRanges of aNode and a range at the sourceRangeIndex - Hernan"
	completeSourceRanges := methodNode completeSourceRanges at: aNode.
	completeRange := completeSourceRanges at: sourceRangeIndex.
	
	"See ExtractMethodFinderTest>>test27FindsInsideParentesis - Hernan"
	trimmedRange := nodesToLookForSize = 1 
		ifTrue: [ completeRange trimToMatchExpressionOn: methodNode sourceText ]
		ifFalse: [ completeRange ].
	
	^trimmedRange! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 10/31/2024 21:14:49'!
completeSourceRangesOfAll: aCollectionOfNodes

	| firstRange lastRange |
	
	firstRange := self completeSourceRangeOf: aCollectionOfNodes first.
	lastRange := self completeSourceRangeOf: aCollectionOfNodes last.
	
	^SourceCodeInterval from: firstRange first to: lastRange last! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 10/29/2024 16:26:31'!
hasOneNodeToLookForAndEquivalentTo: aNode

	^ nodesToLookForSize = 1 and: [ nodesToLookFor first equivalentTo: aNode ]! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 11/2/2024 19:57:54'!
nextStartAfterAddingEquivalentNodesOf: blockStatements from: start to: stop 
	
	| nodesToCompareWith nextStart |
	
	nodesToCompareWith := blockStatements copyFrom: start to: stop.
	
	nextStart := (self areNodesToLookForEquivalentTo: nodesToCompareWith) 
		ifTrue: [ 
			equivalentNodesRanges add: (self completeSourceRangesOfAll: nodesToCompareWith).
			stop + 1 ]
		ifFalse: [ 
			start + 1].
		
	^nextStart! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 11/2/2024 20:04:10'!
selectionToKeepIsEqualOrHasNoIntersectionWith: nodesToCompareWith 

	 | equivalentNodesSourceRanges | 

	equivalentNodesSourceRanges := self completeSourceRangesOfAll: nodesToCompareWith.
		
	^(selectionToKeep = equivalentNodesSourceRanges or: [ (selectionToKeep intersectsRange: equivalentNodesSourceRanges) not ])! !

!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'HAW 11/2/2024 10:19:08'!
shouldKeepSelection

	^selectionToKeep ~= self class noSelection ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 11/2/2024 20:12:04'!
visitBlockNode: blockNode

	| blockStatements |

	super visitBlockNode: blockNode.
	
	nodesToLookForSize > 1 ifTrue: [ 
		blockStatements := blockNode statements. 
		"I need to add temp declaration to statements becuase user could have selected the temp declaration
		See ExtractMethodFinderTest>>test22FindsTemporaryWhenSelectingDefinitionInInnerClosure - Hernan"
		blockNode declaresAnyVariable ifTrue: [ blockStatements addFirst: blockNode temporariesDeclaration ].
		self addEquivalentNodesOf: blockStatements ]
		! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 10/31/2024 20:41:10'!
visitBraceNode: aBraceNode 
	
	nodeVisitedCounter add: aBraceNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aBraceNode.
	
	^super visitBraceNode: aBraceNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 10/31/2024 20:41:22'!
visitCascadeNode: aCascadeNode 

	self addToEquivalentsIfOneStatementEquivalentTo: aCascadeNode.

	 ^super visitCascadeNode: aCascadeNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 10/31/2024 20:41:34'!
visitInstanceVariableNode: anInstanceVariableNode 

	nodeVisitedCounter add: anInstanceVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: anInstanceVariableNode.

	^super visitInstanceVariableNode: anInstanceVariableNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 10/31/2024 20:41:43'!
visitLiteralNode: aLiteralNode

	nodeVisitedCounter add: aLiteralNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aLiteralNode.

	^super visitLiteralNode: aLiteralNode! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 10/31/2024 20:41:50'!
visitLiteralVariableNode: aLiteralVariableNode 
	
	nodeVisitedCounter add: aLiteralVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aLiteralVariableNode.
	
	^super visitLiteralVariableNode: aLiteralVariableNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 10/28/2024 15:46:23'!
visitMessageNode: aMessageNode

	self addToEquivalentsIfOneStatementEquivalentTo: aMessageNode.

	 ^super visitMessageNode: aMessageNode ! !

!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'HAW 10/31/2024 20:41:59'!
visitTempVariableNode: aTempVariableNode 
	
	nodeVisitedCounter add: aTempVariableNode.
	
	self addToEquivalentsIfOneStatementEquivalentTo: aTempVariableNode.
	
	^super visitTempVariableNode: aTempVariableNode ! !

!EquivalentNodesFinder methodsFor: 'initialization' stamp: 'HAW 11/2/2024 19:55:28'!
initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep

	nodesToLookFor := aNodesToLookFor.
	methodNode := aMethodNode.
	selectionToKeep := aSelectionToKeep.
	
	nodesToLookForSize := nodesToLookFor size.
	nodeVisitedCounter := IdentityBag new.
	equivalentNodesRanges := OrderedCollection new.
	! !

!EquivalentNodesFinder methodsFor: 'ranges' stamp: 'HAW 11/2/2024 19:58:52'!
equivalentNodesRanges
	
	^equivalentNodesRanges! !


!EquivalentNodesFinder class methodsFor: 'instance creation' stamp: 'HAW 11/2/2024 19:56:18'!
for: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep

	^ self new initializeFor: aNodesToLookFor in: aMethodNode keepingSelection: aSelectionToKeep! !

!EquivalentNodesFinder class methodsFor: 'instance creation' stamp: 'HAW 11/2/2024 10:17:37'!
noSelection
	
	^`0 to: 0`! !


!Behavior methodsFor: 'methods' stamp: 'HAW 10/30/2024 09:19:58'!
methodNodeWithoutImplicitSelfReturnFor: aSourceCode

	^(self methodNodeFor: aSourceCode) withoutImplicitSelfReturn! !


!BraceNode methodsFor: 'private' stamp: 'HAW 10/30/2024 08:02:55'!
hasEquivalentElementsTo: aBraceNode

	^self areNodesOf: elements equivalentTo: aBraceNode elements ! !


!CascadeNode methodsFor: 'testing' stamp: 'HAW 10/30/2024 08:03:14'!
hasEquivalentMessagesWith: aCascadeNode

	^self areNodesOf: messages equivalentTo: aCascadeNode messages! !


!CodeNode methodsFor: 'private' stamp: 'HAW 10/30/2024 08:03:35'!
hasEquivalentArgumentsWith: aCodeNode

	^self areNodesOf: self arguments equivalentTo: aCodeNode arguments! !


!BlockNode methodsFor: 'private' stamp: 'HAW 10/30/2024 08:04:17'!
hasEquivalentStatementsWith: aBlockNode

	^self areNodesOf: statements equivalentTo: aBlockNode statements! !


!MessageNode methodsFor: 'private' stamp: 'HAW 10/30/2024 08:05:44'!
hasEquivalentArgumentsWith: aMessageNode

	^ self isCascade
		ifTrue: [ self areNodesOf: originalArguments equivalentTo: aMessageNode originalArguments ]
		ifFalse: [ self areNodesOf: arguments equivalentTo: aMessageNode arguments ]! !


!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'HAW 10/30/2024 08:06:50'!
declaresSameVariablesThan: aTemporariesDeclarationNode

	^self areNodesOf: tempDeclarationNodes equivalentTo: aTemporariesDeclarationNode temporaryDeclarationNodes ! !

!TemporariesDeclarationNode methodsFor: 'testing' stamp: 'HAW 10/31/2024 18:35:34'!
declaresVariable: aVariableNode

	^ tempDeclarationNodes anySatisfy: [ :tempDeclaration | tempDeclaration declaresVariable: aVariableNode ]! !


!ParseNodeVisitor methodsFor: 'visiting' stamp: 'HAW 10/31/2024 19:39:57'!
visitTemporaryDeclarationNode: aTemporaryDeclarationNode

	^aTemporaryDeclarationNode variableNode accept: self! !

!methodRemoval: MessageNode #compare:with: stamp: 'HAW 12/9/2024 13:15:19'!
MessageNode removeSelector: #compare:with:!

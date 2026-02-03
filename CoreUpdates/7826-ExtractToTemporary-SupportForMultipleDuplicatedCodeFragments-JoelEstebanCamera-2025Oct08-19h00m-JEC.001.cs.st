'From Cuis7.5 [latest update: #7641] on 28 January 2026 at 9:01:39 am'!
!classDefinition: #CommonScopeNodeFinder category: #'Compiler-Support'!
ParseNodeVisitor subclass: #CommonScopeNodeFinder
	instanceVariableNames: 'parseNodesToFindCommonScope scopesCollection nodesWithScopeFound methodNode actualScopeIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
!classDefinition: #ScopeNode category: #'Compiler-Support'!
Object subclass: #ScopeNode
	instanceVariableNames: 'node nodeRange foundNodesInScope'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Support'!
Smalltalk renameClassNamed: #ExtractMethodMessageSet as: #RepeatedCodeMessageSet!
!classDefinition: #RepeatedCodeMessageSet category: #'Tools-Refactoring'!
MethodSet subclass: #RepeatedCodeMessageSet
	instanceVariableNames: 'finder selectedIndex multimethod '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
Smalltalk renameClassNamed: #ExtractMethodReplacementsWindow as: #ExtractRepeatedCodeWindow!
!classDefinition: #ExtractRepeatedCodeWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #ExtractRepeatedCodeWindow
	instanceVariableNames: 'applier finder multimethod '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMultipleToTemporary category: #'Tools-Refactoring'!
ExtractToTemporary subclass: #ExtractMultipleToTemporary
	instanceVariableNames: 'collectionOfIntervalsToExtract'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractToTemporaryApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractToTemporaryApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newVariable codeProvider finder '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ReplacementsFinder category: #'Tools-Refactoring'!
Object subclass: #ReplacementsFinder
	instanceVariableNames: 'intervalToExtract sourceMethod replacements sourceCodeToExtract nodesToExtract findingStrategy'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
ReplacementsFinder subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'newMessage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractToTemporaryReplacementsFinder category: #'Tools-Refactoring'!
ReplacementsFinder subclass: #ExtractToTemporaryReplacementsFinder
	instanceVariableNames: 'newVariable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!CommonScopeNodeFinder methodsFor: 'accessing' stamp: 'JEC 1/16/2026 08:43:29'!
commonScopeNode

	| scopeToReturn nonEmptyScopes |
	
	self assertHasFoundAllNodes.
	
	"The MethodNode is the first one"
	scopeToReturn := scopesCollection first.

	nonEmptyScopes := scopesCollection select: [ :scope | scope haveNodesFound ].
	
	nonEmptyScopes
		ifNotEmpty: [
			(self scopesOf: nonEmptyScopes)
				 ifNotEmpty: [ :scopesOfNonEmptyScopes |
					| minVal |
					
					minVal := scopesOfNonEmptyScopes min: [ :scope | scope rangeSize ].
					(minVal < scopeToReturn rangeSize)
						ifTrue: [ scopeToReturn := scopesOfNonEmptyScopes detect: [ :scope | scope rangeSize = minVal ] ] ] ].

	^ scopeToReturn parseNode! !

!CommonScopeNodeFinder methodsFor: 'assertions' stamp: 'JEC 1/16/2026 08:07:45'!
assertHasFoundAllNodes

	parseNodesToFindCommonScope 
		ifNotEmpty: [ self error: self class couldNotFindANodeErrorMessage ]! !

!CommonScopeNodeFinder methodsFor: 'initialization' stamp: 'JEC 1/16/2026 08:07:53'!
initializeForAll: aCollectionOfNodes in: aMethodNode

	parseNodesToFindCommonScope := self sortByIntervalPositions: aCollectionOfNodes.
	methodNode := aMethodNode.
	actualScopeIndex := 0.
	nodesWithScopeFound := OrderedCollection new.
	scopesCollection := OrderedCollection new! !

!CommonScopeNodeFinder methodsFor: 'initialization' stamp: 'JEC 1/16/2026 08:08:24'!
sortByIntervalPositions: aCollectionOfNodes 

	^ aCollectionOfNodes asSortedCollection: [ :leftParseNode :rightParseNode | 
		leftParseNode value first < rightParseNode value first ]! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:08:32'!
visitAssignmentNode: anAssignmentNode

	self addNodeIfFound: anAssignmentNode.
	
	super visitAssignmentNode: anAssignmentNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:08:46'!
visitBacktickNode: aBacktickNode

	self addNodeIfFound: aBacktickNode.
	
	super visitBacktickNode: aBacktickNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:08:52'!
visitBlockNode: aBlockNode 

	self addNodeIfFound: aBlockNode.
	
	self visitBlockNodeWithUpdatedScope: aBlockNode.! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:08:58'!
visitBraceNode: aBraceNode

	self addNodeIfFound: aBraceNode.
	
	super visitBraceNode: aBraceNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:09:03'!
visitCascadeNode: aCascadeNode

	self addNodeIfFound: aCascadeNode.
	
	super visitCascadeNode: aCascadeNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:09:09'!
visitInstanceVariableNode: anInstanceVariableNode

	self addNodeIfFound: anInstanceVariableNode.
	
	super visitInstanceVariableNode: anInstanceVariableNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:09:13'!
visitLiteralNode: aLiteralNode

	self addNodeIfFound: aLiteralNode.
	
	super visitLiteralNode: aLiteralNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:09:23'!
visitLiteralVariableNode: aLiteralVariableNode

	self addNodeIfFound: aLiteralVariableNode.
	
	super visitLiteralVariableNode: aLiteralVariableNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:09:28'!
visitMessageNode: aMessageNode

	self addNodeIfFound: aMessageNode.
	
	super visitMessageNode: aMessageNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:09:34'!
visitMessageNodeInCascade: aMessageNode
	
	self addNodeIfFound: aMessageNode.
	
	super visitMessageNodeInCascade: aMessageNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:09:39'!
visitMethodNode: aMethodNode 

	self addToScopeList: aMethodNode.
	self addNodeIfFound: aMethodNode.
	
	self visitIfNotNil: aMethodNode temporariesDeclaration.
	
	"This is because all MethodNodes have a BlockNode but the scope node here must be the MethodNode"
	self visitMethodNodeBlockOf: aMethodNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 19:23:44'!
visitNewArrayNode: aNewArrayNode

	"This node is for #genPushNewArray: opcode only. See NewArrayNode.
	This is not tested due to its difficulty, but it is included for completeness."

	self addNodeIfFound: aNewArrayNode.
	
	super visitNewArrayNode: aNewArrayNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 19:17:30'!
visitRemoteTempVectorNode: aRemoteTempVectorNode

	"This is for visiting RemoteTempVectorNodes.
	
	There are two uses for this kind of nodes:
		- sending #analyseTempsWithin:rootNode:assignmentPools: that is implemented by several ParseNodes but
		there are no senders up to know of this message.
		- when decompiling (DecompilerConstructor>>#codeRemoteTemp:remoteTemps:), but the decompiled
		methodNode does not have the completeSourceRanges so this visitor cannot be used by it.
	
	This is not tested due to its difficulty, but it is included for completeness"

	self addNodeIfFound: aRemoteTempVectorNode.
	
	super visitRemoteTempVectorNode: aRemoteTempVectorNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:09:57'!
visitReturnNode: aReturnNode

	self addNodeIfFound: aReturnNode.

	super visitReturnNode: aReturnNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:10:01'!
visitSelectorNode: aSelectorNode

	self addNodeIfFound: aSelectorNode.

	super visitSelectorNode: aSelectorNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:10:08'!
visitTempVariableNode: aTempVariableNode

	self addNodeIfFound: aTempVariableNode.
	
	super visitTempVariableNode: aTempVariableNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:10:15'!
visitTemporariesDeclarationNode: aTemporariesDeclarationNode

	self addNodeIfFound: aTemporariesDeclarationNode.
	
	super visitTemporariesDeclarationNode: aTemporariesDeclarationNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 08:10:20'!
visitTemporaryDeclarationNode: aTemporaryDeclarationNode

	self addNodeIfFound: aTemporaryDeclarationNode.
	
	super visitTemporaryDeclarationNode: aTemporaryDeclarationNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 19:23:21'!
visitUndeclaredVariableNode: aVariableNode

	"These kind of nodes are only used by the Parser and not outside that context.
	The Encoder changes these nodes to LiteralVariableNodes.
	See Encoder>>#undeclared:. Eventually, it sends Encoder>>#global:name:, where the node is going to 
	be taken as LiteralVariableNode.
	
	This is not tested due to its difficulty, but it is included for completeness"

	self addNodeIfFound: aVariableNode.
	
	super visitUndeclaredVariableNode: aVariableNode! !

!CommonScopeNodeFinder methodsFor: 'visiting' stamp: 'JEC 1/16/2026 16:59:37'!
visitVariableNode: aVariableNode

	self addNodeIfFound: aVariableNode.
	
	super visitVariableNode: aVariableNode! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:10:36'!
actualScope

	^ scopesCollection at: actualScopeIndex! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:53:00'!
addNodeIfFound: aNode

	| nodeFound |

	self detectNodeToFind: aNode
		ifFound: [ :aNodeFound | nodeFound := aNodeFound ]
		ifNone: [ ^ false ].

	nodesWithScopeFound add: aNode.
	self actualScope addFoundNodeInScope: aNode.
	parseNodesToFindCommonScope remove: nodeFound.
	
	^ true! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:11:11'!
addToScopeList: aNode

	scopesCollection add: (ScopeNode node: aNode range: (self nodeRangeOf: aNode)).
	
	actualScopeIndex := scopesCollection size! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:11:30'!
completeSourceRangesOf: aNode

	^ (methodNode 
		completeSourceRangesOf: aNode 
		ifAbsent: [ self shouldNotHappen ])
		asSortedCollection: [ :leftRange :rightRange | leftRange first < rightRange first ]! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:11:36'!
detectNodeToFind: aNode ifFound: aFoundBlock ifNone: aNoneBlock

	^ parseNodesToFindCommonScope 
		detect: [ :nodeToFind | (nodeToFind key equivalentTo: aNode) 
			and: [ self includesActualScopeRangeAllOf: nodeToFind value ]]
		ifFound: aFoundBlock
		ifNone: aNoneBlock! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:11:44'!
includesActualScopeRangeAllOf: anInterval

	^ self actualScope rangeIncludesAllOf: anInterval! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:11:52'!
isNodeToFind: aNode

	^ self detectNodeToFind: aNode
		ifFound: [ :nodeFound | true ]
		ifNone: [ false ]! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:11:59'!
isScopeAddedWithRange: sourceRange

	^ scopesCollection anySatisfy: [ :scope | scope hasRange: sourceRange ]! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:12:08'!
nodeRangeOf: aNode 

	^ aNode isMethodNode 
		ifTrue: [ 1 to: aNode sourceText size ]
		ifFalse: [
			| sourceRanges |
			
			sourceRanges := self completeSourceRangesOf: aNode.
			sourceRanges
				detect: [ :sourceRange | (self isScopeAddedWithRange: sourceRange) not ]
				ifNone: [ self shouldNotHappen ] ]! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:12:27'!
scopesOf: nonEmptyScopes 

	^ scopesCollection allButFirst select: [ :scope | 
		nonEmptyScopes allSatisfy: [ :scopeWithNodesFound | scopeWithNodesFound isIncludedIn: scope ]  ]! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:12:14'!
visitBlockNodeWithUpdatedScope: aBlockNode 

	| oldScope |
	
	oldScope := actualScopeIndex.
	self addToScopeList: aBlockNode.
	
	super visitBlockNode: aBlockNode.
		
	actualScopeIndex := oldScope.! !

!CommonScopeNodeFinder methodsFor: 'private - visiting' stamp: 'JEC 1/16/2026 08:12:20'!
visitMethodNodeBlockOf: aMethodNode 

	| methodNodeBlock |

	methodNodeBlock := aMethodNode block.	
	self addNodeIfFound: methodNodeBlock.

	methodNodeBlock 
		statementsDo: [ :aStatement | aStatement accept: self ]! !


!CommonScopeNodeFinder class methodsFor: 'error messages' stamp: 'JEC 1/16/2026 08:12:42'!
couldNotFindANodeErrorMessage

	^ 'Could not find a node in the AST'! !

!CommonScopeNodeFinder class methodsFor: 'instance creation' stamp: 'JEC 1/16/2026 08:13:03'!
forAll: aCollectionOfNodes in: aMethodNode

	^self new initializeForAll: aCollectionOfNodes in: aMethodNode! !


!ScopeNode methodsFor: 'accessing' stamp: 'JEC 1/16/2026 08:15:25'!
addFoundNodeInScope: aFoundNode 

	foundNodesInScope add: aFoundNode! !

!ScopeNode methodsFor: 'accessing' stamp: 'JEC 1/16/2026 08:15:57'!
parseNode

	^ node -> nodeRange! !

!ScopeNode methodsFor: 'accessing' stamp: 'JEC 1/16/2026 08:19:10'!
rangeSize

	^ nodeRange size! !

!ScopeNode methodsFor: 'initialization' stamp: 'JEC 1/16/2026 08:15:38'!
initializeNode: aNode range: aNodeRange 

	node := aNode.
	nodeRange := aNodeRange.
	foundNodesInScope := OrderedCollection new.! !

!ScopeNode methodsFor: 'printing' stamp: 'JEC 1/16/2026 08:37:57'!
printOn: aStream

	super printOn: aStream.
	
	aStream nextPut: $(.
	
	node printOn: aStream.
	
	aStream space; nextPutAll: '->'; space.
	
	nodeRange printOn: aStream.
	
	aStream nextPut: $). ! !

!ScopeNode methodsFor: 'testing' stamp: 'JEC 1/16/2026 08:16:16'!
hasRange: sourceRange 

	^ nodeRange = sourceRange! !

!ScopeNode methodsFor: 'testing' stamp: 'JEC 1/16/2026 08:16:36'!
haveNodesFound

	^ foundNodesInScope notEmpty! !

!ScopeNode methodsFor: 'testing' stamp: 'JEC 1/16/2026 08:18:55'!
isIncludedIn: scope 

	^ scope rangeIncludesAllOf: nodeRange ! !

!ScopeNode methodsFor: 'testing' stamp: 'JEC 1/16/2026 08:14:58'!
rangeIncludesAllOf: anInterval 

	^ nodeRange includesAllOf: anInterval! !


!ScopeNode class methodsFor: 'instance creation' stamp: 'JEC 1/16/2026 08:13:46'!
node: aNode range: aNodeRange 

	^self new initializeNode: aNode range: aNodeRange ! !


!RepeatedCodeMessageSet methodsFor: 'initialization' stamp: 'HAW 1/Dec/2025 16:36:00'!
initializeFinder: aFinder multimethod: aBoolean
	
	finder := aFinder.
	multimethod := aBoolean ! !

!RepeatedCodeMessageSet methodsFor: 'message list' stamp: 'HAW 1/Dec/2025 16:36:00'!
isMultimethod

	^multimethod ! !


!RepeatedCodeMessageSet class methodsFor: 'instance creation' stamp: 'HAW 1/Dec/2025 16:35:00'!
finder: aFinder multimethod: aBoolean
	
	^(self messageList: aFinder methodsToReplace) initializeFinder: aFinder multimethod: aBoolean! !


!ExtractRepeatedCodeWindow methodsFor: 'actions' stamp: 'HAW 1/Dec/2025 16:40:00'!
extractOriginalSelection
	
	self closeAfter: [ applier valueWithOriginalSelection ].
	! !


!ExtractRepeatedCodeWindow class methodsFor: 'instance creation' stamp: 'HAW 1/Dec/2025 16:34:00'!
openFrom: anExtractMethodApplier with: aFinder multimethod: aBoolean 

	| window messageSet |

	messageSet := RepeatedCodeMessageSet finder: aFinder multimethod: aBoolean.
	"I have to set a autoSelectString even if I do not use it because if not the 
	autoSelect event is not triggered - Hernan"
	messageSet autoHighlightString: aFinder sourceCodeToExtract allOccurrences: false.
	
	window := self open: messageSet label: 'Select replacements'. 
	window initializeFrom: anExtractMethodApplier with: aFinder.
	
	^window 

! !


!NodesInRangeFinder methodsFor: 'evaluating - private' stamp: 'JEC 1/16/2026 20:00:19'!
signalNodesNotFound

	self error: self class nodesNotFoundErrorDescription! !


!NodesInRangeFinder class methodsFor: 'error description' stamp: 'JEC 1/16/2026 20:00:33'!
nodesNotFoundErrorDescription

	^ 'nodes not found'! !


!ExtractMethodReplacement methodsFor: 'accessing' stamp: 'HAW 1/Dec/2025 16:56:00'!
asSourceCodeInterval
	
	^intervalToExtract! !


!ExtractToTemporary class methodsFor: 'error messages' stamp: 'JEC 10/11/2025 20:47:27'!
cannotExtractTemporaryWithoutIntervalsErrorMessage

	^ 'Cannot extract temporary without intervals'! !

!ExtractToTemporary class methodsFor: 'error messages' stamp: 'JEC 10/11/2025 20:40:04'!
extractingDifferentPiecesOfCodeErrorMessage

	^ 'Intervals are from different pieces of code'! !

!ExtractToTemporary class methodsFor: 'exceptions' stamp: 'JEC 10/11/2025 20:47:58'!
signalCannotExtractTemporaryWithoutIntervals

	self refactoringError: self cannotExtractTemporaryWithoutIntervalsErrorMessage! !

!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'JEC 1/28/2026 09:00:41'!
named: aNewVariable intervals: aCollectionOfIntervalsToExtract from: aMethodToRefactor 
	
	| intervalsToExtract |
			
	"Remove duplicated intervals"
	intervalsToExtract := aCollectionOfIntervalsToExtract asSet asOrderedCollection.
	
	self assertIntervalsIsNotEmpty: intervalsToExtract. 
	
	^ intervalsToExtract size = 1
		ifTrue: [ 
			self 
				named: aNewVariable 
				at: intervalsToExtract first 
				from: aMethodToRefactor ]
		ifFalse: [
			ExtractMultipleToTemporary 
				named: aNewVariable 
				intervals: intervalsToExtract 
				from: aMethodToRefactor ]! !

!ExtractToTemporary class methodsFor: 'preconditions' stamp: 'JEC 10/11/2025 20:48:35'!
assertIntervalsIsNotEmpty: aCollectionOfIntervalsToExtract 

	aCollectionOfIntervalsToExtract isEmpty
		ifTrue: [ self signalCannotExtractTemporaryWithoutIntervals ]! !

!ExtractToTemporary class methodsFor: 'private - parsing' stamp: 'JEC 1/16/2026 08:32:47'!
methodOrBlockNodeIncludingAll: intervalsToExtract in: aMethodToRefactor

	"Finds the node that provides the common context for all the code pieces that are going to be extracted.
	If there is more than one possible node, it returns the one closest to those code pieces."
	
	| parseNodesToFindCommonScope finder |
	
	parseNodesToFindCommonScope := self 
		parseNodesToFindCommonScopeOf: intervalsToExtract 
		in: aMethodToRefactor.

	finder := CommonScopeNodeFinder forAll: parseNodesToFindCommonScope in: aMethodToRefactor.
	aMethodToRefactor accept: finder.
	
	^ finder commonScopeNode key.! !

!ExtractToTemporary class methodsFor: 'private - parsing' stamp: 'JEC 1/16/2026 08:25:48'!
parseNodesToFindCommonScopeOf: intervalsToExtract in: aMethodToRefactor

	^ intervalsToExtract collect: [ :interval |
		| parseNodes |
		
		parseNodes := aMethodToRefactor 
			parseNodesPathAt: interval first 
			ifAbsent: [ self shouldNotHappen ].
		
		"parseNodes is a SortedCollection sorted by interval size."
		parseNodes first ].! !


!ExtractMultipleToTemporary methodsFor: 'initialization' stamp: 'JEC 11/10/2025 20:07:07'!
initializeNamed: aNewVariableName extractingCodeAt: aCollectionOfIntervalsToExtract from: aMethodToRefactor methodNode: aMethodNodeToRefactor declaringTempIn: blockNodeOfNewVariable

	| anIntervalToExtract |

	newVariableName := aNewVariableName.
	collectionOfIntervalsToExtract := aCollectionOfIntervalsToExtract sort: [ :a :b | a last < b first ] .
	methodToRefactor := aMethodToRefactor.
	methodNodeToRefactor := aMethodNodeToRefactor.
	updatedSourceCode := aMethodToRefactor sourceCode.
	anIntervalToExtract := collectionOfIntervalsToExtract first.
	sourceCodeToExtract := updatedSourceCode copyFrom: anIntervalToExtract first to: anIntervalToExtract last.
	completeSourceRanges := methodNodeToRefactor completeSourceRanges.
	parseNodeWithNewVariableScope := blockNodeOfNewVariable.
	
	"This is for convenience, i keep the first interval to extract in this inst var"
	intervalToExtract := collectionOfIntervalsToExtract first.! !

!ExtractMultipleToTemporary methodsFor: 'private - applying steps' stamp: 'JEC 10/17/2025 15:14:23'!
replaceAllCodeIntervalsWithNewVariable

	updatedSourceCode := updatedSourceCode 
		copyReplacing: collectionOfIntervalsToExtract 
		with: newVariableName! !

!ExtractMultipleToTemporary methodsFor: 'private - applying steps' stamp: 'JEC 10/20/2025 18:16:18'!
resolveNewVariableAssignment

	self replaceAllCodeIntervalsWithNewVariable.
	self writeAssignmentStatementOfNewTemporaryVariable.! !


!ExtractMultipleToTemporary class methodsFor: 'exceptions' stamp: 'JEC 10/11/2025 20:40:04'!
signalExtractingDifferentPiecesOfCodeError

	self refactoringError: self extractingDifferentPiecesOfCodeErrorMessage! !

!ExtractMultipleToTemporary class methodsFor: 'instance creation' stamp: 'JEC 1/16/2026 08:46:08'!
named: aNewVariable intervals: intervalsToExtract from: aMethodToRefactor 

	|  methodNodeToRefactor trimmedNewVariable trimmedIntervalsToExtract codeNodesForNewVariable nodeToDeclareNewVariable |

	self assertCanApplyRefactoringOn: aMethodToRefactor atAll: intervalsToExtract.
	
	methodNodeToRefactor := aMethodToRefactor notOptimizedMethodNode.
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	trimmedIntervalsToExtract := self trimmedIntervalsOf: intervalsToExtract in: aMethodToRefactor.
	codeNodesForNewVariable := self codeNodesOf: intervalsToExtract in: methodNodeToRefactor.
	nodeToDeclareNewVariable := self 
		methodOrBlockNodeIncludingAll: intervalsToExtract 
		in: methodNodeToRefactor.

	"There is no need to check the new temporary preconditions for every piece of code, 
	because the validation already ensures that the new temporary is valid both in the method and in the class.
	Please see NewTemporaryPrecondition>>#value."
	self newTemporaryPreconditionClass 
		valueFor: trimmedNewVariable 
		in: codeNodesForNewVariable first 
		of: methodNodeToRefactor.

	^ self new
		initializeNamed: trimmedNewVariable
		extractingCodeAt: trimmedIntervalsToExtract
		from: aMethodToRefactor
		methodNode: methodNodeToRefactor
		declaringTempIn: nodeToDeclareNewVariable! !

!ExtractMultipleToTemporary class methodsFor: 'preconditions' stamp: 'JEC 10/11/2025 21:47:31'!
applyPreconditionsOn: codeNodesForNewVariable named: trimmedNewVariable in: methodNodeToRefactor

	codeNodesForNewVariable do: [ :codeNodeForNewVariable |
		self newTemporaryPreconditionClass 
			valueFor: trimmedNewVariable 
			in: codeNodeForNewVariable 
			of: methodNodeToRefactor ]! !

!ExtractMultipleToTemporary class methodsFor: 'preconditions' stamp: 'HAW 1/Dec/2025 15:42:00'!
assertCanApplyRefactoringOn: aMethodToRefactor atAll: intervalsToExtract 

	| methodNode firstNode |
    	
    	methodNode := aMethodToRefactor methodNode.
    	firstNode := nil.
    
    	intervalsToExtract do: [ :anIntervalToExtract |
    		| parseNodes parseNode |
    		
    		self assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract asSourceCodeInterval.
    		
    		parseNodes := methodNode 
    			parseNodesPathAt: anIntervalToExtract first 
    			ifAbsent: [ self shouldNotHappen ].
    		parseNode := parseNodes first.
    		
    		firstNode 
			ifNil: [ firstNode := parseNode key ]
			ifNotNil: [ (firstNode equivalentTo: parseNode key) ifFalse: [ self signalExtractingDifferentPiecesOfCodeError ]]]! !

!ExtractMultipleToTemporary class methodsFor: 'private' stamp: 'JEC 10/11/2025 21:43:53'!
codeNodesOf: intervalsToExtract in: methodNodeToRefactor

	^ intervalsToExtract collect: [ :interval | 
			self methodOrBlockNodeIncluding: interval in: methodNodeToRefactor ]! !

!ExtractMultipleToTemporary class methodsFor: 'private' stamp: 'JEC 10/11/2025 21:42:46'!
trimmedIntervalsOf: intervalsToExtract in: aMethodToRefactor

	^ intervalsToExtract collect: [ :interval | interval trimToMatchExpressionOn: aMethodToRefactor sourceCode ]! !


!ExtractToTemporaryApplier methodsFor: 'evaluating' stamp: 'JEC 10/12/2025 21:49:58'!
value

	requestExitBlock := [ ^self ].
	
	self requestRefactoringParametersHandlingRefactoringExceptions! !

!ExtractToTemporaryApplier methodsFor: 'evaluating' stamp: 'JEC 10/12/2025 21:50:25'!
valueCreatingWith: aRefactoringCreationBlock

	self 
		createAndSetRefactoringHandlingRefactoringExceptions: aRefactoringCreationBlock;
		applyRefactoring;
		showChanges! !

!ExtractToTemporaryApplier methodsFor: 'evaluating' stamp: 'JEC 10/12/2025 21:51:28'!
valueWithAllReplacements

	self valueCreatingWith: [ self createRefactoring ]! !

!ExtractToTemporaryApplier methodsFor: 'evaluating' stamp: 'JEC 10/12/2025 21:50:52'!
valueWithOriginalSelection
	
	self valueCreatingWith: [ self createRefactoringForOriginalSelection ]! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - creation' stamp: 'HAW 1/Dec/2025 16:54:00'!
createRefactoringForOriginalSelection

	^ self refactoringClass
		named: newVariable
		at: finder originalSelectionReplacement 
		from: methodToExtractCodeFrom! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'JEC 10/12/2025 22:21:55'!
findReplacementsInMethod

	finder := ExtractToTemporaryReplacementsFinder
		named: newVariable 
		at: intervalToExtract 
		from: methodToExtractCodeFrom.
	finder value.! !


!ReplacementsFinder methodsFor: 'evaluating' stamp: 'HAW 1/Dec/2025 16:49:00'!
value

	self 
		initializeFinding;
		find.! !

!ReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 10/12/2025 22:04:11'!
inClassReplacements
	
	^replacements select: [ :aReplacement | aReplacement isAt: sourceMethod methodClass ]! !

!ReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 10/12/2025 22:04:30'!
removeReplacementAt: anIndex

	^replacements removeAt: anIndex ! !

!ReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 10/12/2025 22:04:42'!
replacements
	
	^replacements ! !

!ReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 10/12/2025 22:04:57'!
replacementsAt: anIndex ifAbsent: ifAbsentBlock
 
	^replacements at: anIndex ifAbsent: ifAbsentBlock ! !

!ReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 10/12/2025 22:05:07'!
sourceMethodReplacements
	
	^replacements select: [:aReplacement | aReplacement isOf: sourceMethod ]! !

!ReplacementsFinder methodsFor: 'source code' stamp: 'JEC 10/12/2025 22:05:20'!
sourceCodeToExtract
	
	^sourceCodeToExtract! !

!ReplacementsFinder methodsFor: 'testing' stamp: 'JEC 10/12/2025 22:05:32'!
hasOneReplacement
	
	^replacements size = 1! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 10/12/2025 22:05:51'!
addReplacementAt: foundIntervalToExtract in: aMethod
	
	"If there is an error creating the refactoring, then the found text is not extractable and 
	therefore should not be replaced - Hernan"
	[ replacements add: (self createReplacementAt: foundIntervalToExtract in: aMethod) ] 
		on: RefactoringError 
		do: [ :anError | ].! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 10/12/2025 22:06:26'!
createReplacementAt: foundIntervalToExtract in: aMethod

	self subclassResponsibility! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 10/12/2025 22:06:49'!
defineFindingStrategy
	
	| useStringEquality |
	
	"Sadly, finding the right ranges when extracting literal nodes is almost imposible becuase literal nodes can have many
	ranges. An alternative algorithm could be to create an 'expanded AST' without reused nodes and each node with its own
	source range. Nodes of that AST could be a polimorphic proxy of the real node, adding only the source range, something
	to try ...
	For the moment, if the selection to extract has only literal nodes, uses string comparison - Hernan"
	useStringEquality := nodesToExtract allSatisfy: [ :aNode | aNode isLiteralNode ].
	
	^useStringEquality 
		ifTrue: [ [ :aMethod | self findReplacementsUsingStringEqualityIn: aMethod ]]
		ifFalse: [ [ :aMethod | self findReplacementsUsingNodeEquivalenceIn: aMethod ]]! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 1/Dec/2025 16:45:00'!
find

	self subclassResponsibility ! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 10/12/2025 22:07:48'!
findReplacementsUsingNodeEquivalenceIn: aMethod

	| rangesToExtract |
		
	rangesToExtract := sourceMethod compiledMethod = aMethod
		ifTrue: [  aMethod sourceRangesOfNodesEquivalentTo: nodesToExtract keepingSelection: intervalToExtract ]	
		ifFalse: [ aMethod sourceRangesOfNodesEquivalentTo: nodesToExtract ].
		
	rangesToExtract do: [ :sourceRangeToExtract | self addReplacementAt: sourceRangeToExtract in: aMethod ].! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 1/16/2026 09:21:19'!
findReplacementsUsingStringEqualityIn: aMethod

	| sourceCode foundIntervalToExtract sourceCodeToExtractStart theSizeToExtract |
	
	sourceCode := aMethod sourceCode.
	theSizeToExtract := intervalToExtract size - 1.
	"This is to avoid finding strings in the selector"
	sourceCodeToExtractStart := aMethod methodNode selectorLastPosition + 1.
	
	[ sourceCodeToExtractStart := sourceCode indexOfSubCollection: sourceCodeToExtract startingAt: sourceCodeToExtractStart.
	sourceCodeToExtractStart ~= 0 ] whileTrue: [ 
		foundIntervalToExtract := sourceCodeToExtractStart to: sourceCodeToExtractStart + theSizeToExtract.
		self addReplacementAt: foundIntervalToExtract in: aMethod.
		sourceCodeToExtractStart := foundIntervalToExtract last + 1 ] 
		
	 ! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 1/Dec/2025 16:49:00'!
initializeFinding

	sourceCodeToExtract := sourceMethod sourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	nodesToExtract := self nodesToExtract.
	replacements := OrderedCollection new.
	findingStrategy := self defineFindingStrategy! !

!ReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'JEC 10/12/2025 22:08:13'!
nodesToExtract

	^(NodesInRangeFinder in: sourceMethod for: intervalToExtract) value! !


!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 1/Dec/2025 16:48:00'!
find

	sourceMethod methodClass withAllSubclassesDo: [ :aClass | self findReplacementsAt: aClass]! !

!ExtractMethodReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 1/Dec/2025 16:50:00'!
findReplacementsAt: aClass

	"I live this implementationt instead of 
	
	aClass methodsDo: findingStrategy
	
	for debugging porpouses. We can do Through with this one - Hernan"
	aClass methodsDo: [ :aMethod | findingStrategy value: aMethod ]
	! !


!ExtractToTemporaryReplacementsFinder methodsFor: 'initialization' stamp: 'JEC 10/12/2025 22:10:23'!
initializeNamed: aNewVariable at: anIntervalToExtract from: aMethodToRefactor

	newVariable := aNewVariable.
	sourceMethod := aMethodToRefactor.
	intervalToExtract := anIntervalToExtract.! !

!ExtractToTemporaryReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 1/16/2026 19:48:05'!
createReplacementAt: foundIntervalToExtract in: aMethod
	
	SourceCodeOfTemporaryToBeExtractedPrecondition valueFor: foundIntervalToExtract of: aMethod.
	
	^ foundIntervalToExtract asSourceCodeInterval! !

!ExtractToTemporaryReplacementsFinder methodsFor: 'replacements' stamp: 'JEC 10/14/2025 17:42:43'!
methodsToReplace

	"Is the same method for all replacements"

	^ replacements collect: [ :aReplacement | sourceMethod ]! !

!ExtractToTemporaryReplacementsFinder methodsFor: 'replacements' stamp: 'HAW 1/Dec/2025 16:09:00'!
originalSelectionReplacement

	^ intervalToExtract! !

!ExtractToTemporaryReplacementsFinder methodsFor: 'private-replacement finding' stamp: 'HAW 1/Dec/2025 16:46:00'!
find

	findingStrategy value: sourceMethod compiledMethod.! !


!ExtractToTemporaryReplacementsFinder class methodsFor: 'instance creation' stamp: 'JEC 1/16/2026 09:17:39'!
named: aNewVariable at: anIntervalToExtract from: aMethodToRefactor

	^ self new initializeNamed: aNewVariable at: anIntervalToExtract from: aMethodToRefactor! !


!RepeatedCodeMessageSet methodsFor: 'source code ranges' stamp: 'HAW 1/Dec/2025 16:11:00'!
messageSendsRangesOf: aSelector

	| replacement |
	
	replacement := finder replacementsAt: self messageListIndex ifAbsent: [ ^#()  ].
	
	^Array with: replacement asSourceCodeInterval! !


!ExtractRepeatedCodeWindow methodsFor: 'GUI building' stamp: 'HAW 1/Dec/2025 16:37:00'!
addButtonsTo: row color: buttonColor

	self 
		addButton: self createRemoveButton to: row color: buttonColor;
		addButton: self createRefactorButton to: row color: buttonColor;
		addButton: self createExtractSelectionOnlyButton to: row color: buttonColor.
	
	model isMultimethod ifTrue: [ 
		self 
			addButton: self createExtractInMethodOnlyButton to: row color: buttonColor;
			addButton: self createExtractAllInClassButton to: row color: buttonColor ].
		
	self addButton: self createCancelButton to: row color: buttonColor.
! !

!ExtractRepeatedCodeWindow methodsFor: 'button creation' stamp: 'HAW 1/Dec/2025 16:40:00'!
createExtractSelectionOnlyButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractOriginalSelection
		label: 'Original Selection'
		balloonText: 'Applies the refactoring to the original selected code only'
! !

!ExtractRepeatedCodeWindow methodsFor: 'initialization' stamp: 'HAW 1/Dec/2025 16:37:00'!
initializeFrom: anExtractMethodApplier with: aFinder 

	applier := anExtractMethodApplier.
	finder := aFinder.
! !


!NodesInRangeFinder methodsFor: 'evaluating' stamp: 'JEC 1/16/2026 19:59:59'!
value
	
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
			
	self signalNodesNotFound
	! !


!ExtractToTemporary methodsFor: 'initialization' stamp: 'HAW 7/24/2023 15:27:55'!
initializeNamed: aNewVariable extractingCodeAt: anIntervalToExtract from: aMethodToRefactor methodNode: aMethodNodeToRefactor declaringTempIn: blockNodeOfNewVariable

	newVariableName := aNewVariable.
	intervalToExtract := anIntervalToExtract.
	methodToRefactor := aMethodToRefactor.
	methodNodeToRefactor := aMethodNodeToRefactor.
	updatedSourceCode := aMethodToRefactor sourceCode.
	sourceCodeToExtract := updatedSourceCode copyFrom: intervalToExtract first to: intervalToExtract last.
	parseNodeWithNewVariableScope := blockNodeOfNewVariable.
	completeSourceRanges := methodNodeToRefactor completeSourceRanges ! !


!ExtractToTemporary class methodsFor: 'instance creation' stamp: 'HAW 7/27/2023 17:26:41'!
named: aNewVariable at: anIntervalToExtract from: aMethodToRefactor

	|  trimmedNewVariable trimmedIntervalToExtract codeNodeForNewVariable methodNodeToRefactor |

	self assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract.
	methodNodeToRefactor := aMethodToRefactor notOptimizedMethodNode.
	trimmedNewVariable := aNewVariable withBlanksTrimmed.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: aMethodToRefactor sourceCode.
	codeNodeForNewVariable := self methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor.
	self newTemporaryPreconditionClass valueFor: trimmedNewVariable in: codeNodeForNewVariable of: methodNodeToRefactor.

	^ self new
		initializeNamed: trimmedNewVariable
		extractingCodeAt: trimmedIntervalToExtract
		from: aMethodToRefactor
		methodNode: methodNodeToRefactor
		declaringTempIn: codeNodeForNewVariable! !

!ExtractToTemporary class methodsFor: 'private - parsing' stamp: 'JEC 11/10/2025 20:11:21'!
methodOrBlockNodeIncluding: anIntervalToExtract in: methodNodeToRefactor
	"Finds the appropriate block node to define a variable that will reference the code in the interval to extract.
	The possibles results are the top-level methodNode or a block node inside some of the method statements."

	"This method will return the Node that is nearer of the code that is going to be extracted"
	^ self methodOrBlockNodeIncludingAll: { anIntervalToExtract } in: methodNodeToRefactor.! !


!ExtractMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/Dec/2025 16:27:00'!
requestRefactoringParameters

	| newMessage |
	
	newMessage := self requestNewMessage.
	self createExtractMethodNewMethodFor: newMessage.
	self findReplacementsWith: newMessage.
	
	finder hasOneReplacement 
		ifTrue: [ self valueWithAllReplacements ]
		ifFalse: [ ExtractRepeatedCodeWindow openFrom: self with: finder multimethod: true ]
		! !


!ExtractToTemporaryApplier methodsFor: 'initialization' stamp: 'JEC 10/14/2025 17:40:09'!
initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := MethodReference method: aMethodToExtractCodeFrom.! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - creation' stamp: 'HAW 1/Dec/2025 16:54:00'!
createRefactoring

	| intervals |

	intervals := finder replacements.
	^ self refactoringClass
		named: newVariable
		intervals: intervals 
		from: methodToExtractCodeFrom! !

!ExtractToTemporaryApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/Dec/2025 16:27:00'!
requestRefactoringParameters

	self askNewVariableName.
	self findReplacementsInMethod.

	finder hasOneReplacement 
		ifTrue: [ self valueWithAllReplacements ]
		ifFalse: [ ExtractRepeatedCodeWindow openFrom: self with: finder multimethod: false ]! !


!ExtractToTemporaryApplier class methodsFor: 'instance creation' stamp: 'JEC 10/12/2025 21:52:15'!
on: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom

	| trimmedIntervalToExtract sourceCode |
	
	sourceCode := aMethodToExtractCodeFrom sourceCode.
	trimmedIntervalToExtract := anIntervalToExtract trimToMatchExpressionOn: sourceCode.

	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: trimmedIntervalToExtract.

	^ self new initializeOn: aCodeProvider for: trimmedIntervalToExtract of: aMethodToExtractCodeFrom! !

!methodRemoval: ExtractToTemporaryReplacementsFinder #value stamp: 'JEC 12/3/2025 19:04:04'!
ExtractToTemporaryReplacementsFinder removeSelector: #value!
!methodRemoval: ExtractMethodReplacementsFinder #findReplacementInHierarchy stamp: 'JEC 10/12/2025 22:07:17'!
ExtractMethodReplacementsFinder removeSelector: #findReplacementInHierarchy!
!methodRemoval: ExtractMethodReplacementsFinder #removeReplacementAt: stamp: 'JEC 10/12/2025 22:04:30'!
ExtractMethodReplacementsFinder removeSelector: #removeReplacementAt:!
!methodRemoval: ExtractMethodReplacementsFinder #nodesToExtract stamp: 'JEC 10/12/2025 22:08:13'!
ExtractMethodReplacementsFinder removeSelector: #nodesToExtract!
!methodRemoval: ExtractMethodReplacementsFinder #value stamp: 'JEC 12/3/2025 19:04:04'!
ExtractMethodReplacementsFinder removeSelector: #value!
!methodRemoval: ExtractMethodReplacementsFinder #sourceMethodReplacements stamp: 'JEC 10/12/2025 22:05:07'!
ExtractMethodReplacementsFinder removeSelector: #sourceMethodReplacements!
!methodRemoval: ExtractMethodReplacementsFinder #replacementsAt:ifAbsent: stamp: 'JEC 10/12/2025 22:04:57'!
ExtractMethodReplacementsFinder removeSelector: #replacementsAt:ifAbsent:!
!methodRemoval: ExtractMethodReplacementsFinder #sourceCodeToExtract stamp: 'JEC 10/12/2025 22:05:20'!
ExtractMethodReplacementsFinder removeSelector: #sourceCodeToExtract!
!methodRemoval: ExtractMethodReplacementsFinder #defineFindingStrategy stamp: 'JEC 10/12/2025 22:06:49'!
ExtractMethodReplacementsFinder removeSelector: #defineFindingStrategy!
!methodRemoval: ExtractMethodReplacementsFinder #hasOneReplacement stamp: 'JEC 10/12/2025 22:05:32'!
ExtractMethodReplacementsFinder removeSelector: #hasOneReplacement!
!methodRemoval: ExtractMethodReplacementsFinder #findReplacementsUsingStringEqualityIn: stamp: 'JEC 10/12/2025 22:08:00'!
ExtractMethodReplacementsFinder removeSelector: #findReplacementsUsingStringEqualityIn:!
!methodRemoval: ExtractMethodReplacementsFinder #inClassReplacements stamp: 'JEC 10/12/2025 22:04:11'!
ExtractMethodReplacementsFinder removeSelector: #inClassReplacements!
!methodRemoval: ExtractMethodReplacementsFinder #replacements stamp: 'JEC 10/12/2025 22:04:42'!
ExtractMethodReplacementsFinder removeSelector: #replacements!
!methodRemoval: ExtractMethodReplacementsFinder #addReplacementAt:in: stamp: 'JEC 10/12/2025 22:05:46'!
ExtractMethodReplacementsFinder removeSelector: #addReplacementAt:in:!
!methodRemoval: ExtractMethodReplacementsFinder #findReplacementsUsingNodeEquivalenceIn: stamp: 'JEC 10/12/2025 22:07:48'!
ExtractMethodReplacementsFinder removeSelector: #findReplacementsUsingNodeEquivalenceIn:!
!methodRemoval: ReplacementsFinder #findReplacementInHierarchy stamp: 'JEC 12/3/2025 19:04:04'!
ReplacementsFinder removeSelector: #findReplacementInHierarchy!
!methodRemoval: ReplacementsFinder #findReplacementsAt: stamp: 'JEC 12/3/2025 19:04:04'!
ReplacementsFinder removeSelector: #findReplacementsAt:!
!methodRemoval: ExtractMultipleToTemporary class #parseNodeToDeclareTempFrom:of:ifAbsent: stamp: 'JEC 11/10/2025 20:09:04'!
ExtractMultipleToTemporary class removeSelector: #parseNodeToDeclareTempFrom:of:ifAbsent:!
!methodRemoval: ExtractMultipleToTemporary class #allNonEmptyBlocksNodesAt:in: stamp: 'JEC 11/10/2025 20:08:39'!
ExtractMultipleToTemporary class removeSelector: #allNonEmptyBlocksNodesAt:in:!
!methodRemoval: ExtractMultipleToTemporary class #initializeNamed:intervals:from: stamp: 'JEC 10/8/2025 19:04:46'!
ExtractMultipleToTemporary class removeSelector: #initializeNamed:intervals:from:!
!methodRemoval: ExtractMultipleToTemporary class #sortedCollectionByIntervalSizeOf: stamp: 'JEC 11/10/2025 20:09:08'!
ExtractMultipleToTemporary class removeSelector: #sortedCollectionByIntervalSizeOf:!
!methodRemoval: ExtractMultipleToTemporary class #canDeclareTempOf:In: stamp: 'JEC 11/10/2025 19:59:01'!
ExtractMultipleToTemporary class removeSelector: #canDeclareTempOf:In:!
!methodRemoval: ExtractMultipleToTemporary class #methodOrBlockNodeIncludingAll:in: stamp: 'JEC 11/10/2025 20:09:00'!
ExtractMultipleToTemporary class removeSelector: #methodOrBlockNodeIncludingAll:in:!
!methodRemoval: ExtractMultipleToTemporary class #canDeclareTempOf:in: stamp: 'JEC 11/10/2025 20:08:54'!
ExtractMultipleToTemporary class removeSelector: #canDeclareTempOf:in:!
!methodRemoval: ExtractMultipleToTemporary class #allNoneEmptyBlockParseNodesFrom:in: stamp: 'JEC 11/10/2025 20:08:43'!
ExtractMultipleToTemporary class removeSelector: #allNoneEmptyBlockParseNodesFrom:in:!
!methodRemoval: ExtractMultipleToTemporary class #parseNodeToDeclareTempFrom:ifAbsent: stamp: 'JEC 11/10/2025 20:00:30'!
ExtractMultipleToTemporary class removeSelector: #parseNodeToDeclareTempFrom:ifAbsent:!
!methodRemoval: ExtractMultipleToTemporary #compileDeclaringNewTemporaryVariable stamp: 'JEC 10/20/2025 18:15:11'!
ExtractMultipleToTemporary removeSelector: #compileDeclaringNewTemporaryVariable!
!methodRemoval: ExtractMultipleToTemporary #declareNewTemporaryVariableAndReflectSourceCodeChanges stamp: 'JEC 10/8/2025 20:27:18'!
ExtractMultipleToTemporary removeSelector: #declareNewTemporaryVariableAndReflectSourceCodeChanges!
!methodRemoval: ExtractMultipleToTemporary #sortFromMinToMaxUsingIntervalSize: stamp: 'JEC 11/9/2025 16:22:47'!
ExtractMultipleToTemporary removeSelector: #sortFromMinToMaxUsingIntervalSize:!
!methodRemoval: ExtractMultipleToTemporary #allNonEmptyBlocksNodesAt: stamp: 'JEC 11/10/2025 20:07:30'!
ExtractMultipleToTemporary removeSelector: #allNonEmptyBlocksNodesAt:!
!methodRemoval: ExtractMultipleToTemporary #positionToInsertNewTemporaryVariableAssignment stamp: 'JEC 10/17/2025 15:11:45'!
ExtractMultipleToTemporary removeSelector: #positionToInsertNewTemporaryVariableAssignment!
!methodRemoval: ExtractMultipleToTemporary #apply stamp: 'JEC 10/20/2025 18:16:55'!
ExtractMultipleToTemporary removeSelector: #apply!
!methodRemoval: ExtractMultipleToTemporary #correctFrom:to:with: stamp: 'JEC 10/20/2025 18:14:49'!
ExtractMultipleToTemporary removeSelector: #correctFrom:to:with:!
!methodRemoval: ExtractMultipleToTemporary #allNonEmptyBlocksAt: stamp: 'JEC 11/9/2025 16:03:22'!
ExtractMultipleToTemporary removeSelector: #allNonEmptyBlocksAt:!
!methodRemoval: ExtractMultipleToTemporary #canDeclareTempIn: stamp: 'JEC 11/10/2025 20:07:35'!
ExtractMultipleToTemporary removeSelector: #canDeclareTempIn:!
!methodRemoval: ExtractMultipleToTemporary #allNoneEmptyBlockParseNodesFromIntervalsToExtract stamp: 'JEC 11/10/2025 20:07:32'!
ExtractMultipleToTemporary removeSelector: #allNoneEmptyBlockParseNodesFromIntervalsToExtract!
!methodRemoval: ExtractMultipleToTemporary #allBlockParseNodesFromIntervalsToExtract stamp: 'JEC 11/9/2025 16:18:16'!
ExtractMultipleToTemporary removeSelector: #allBlockParseNodesFromIntervalsToExtract!
!methodRemoval: ExtractMultipleToTemporary #selectInvisiblyFrom:to: stamp: 'JEC 10/20/2025 18:14:49'!
ExtractMultipleToTemporary removeSelector: #selectInvisiblyFrom:to:!
!methodRemoval: ExtractMultipleToTemporary #parseNodeToDeclareTempFrom:ifAbsent: stamp: 'JEC 11/10/2025 20:07:38'!
ExtractMultipleToTemporary removeSelector: #parseNodeToDeclareTempFrom:ifAbsent:!
!methodRemoval: ExtractMultipleToTemporary #selectionInterval stamp: 'JEC 10/20/2025 18:14:49'!
ExtractMultipleToTemporary removeSelector: #selectionInterval!
!methodRemoval: ExtractMultipleToTemporary #allParseNodesAndScopeIntervals stamp: 'JEC 11/3/2025 19:02:31'!
ExtractMultipleToTemporary removeSelector: #allParseNodesAndScopeIntervals!
!methodRemoval: ExtractMultipleToTemporary #allBlockParseNodesAndScopeIntervals stamp: 'JEC 11/3/2025 20:12:25'!
ExtractMultipleToTemporary removeSelector: #allBlockParseNodesAndScopeIntervals!
!methodRemoval: ExtractMultipleToTemporary #notify:at:in: stamp: 'JEC 10/20/2025 18:14:49'!
ExtractMultipleToTemporary removeSelector: #notify:at:in:!
!methodRemoval: ExtractMultipleToTemporary #declareNewTemporaryVariable stamp: 'JEC 10/8/2025 20:22:34'!
ExtractMultipleToTemporary removeSelector: #declareNewTemporaryVariable!
!methodRemoval: ExtractMultipleToTemporary #selectFrom:to: stamp: 'JEC 10/20/2025 18:14:49'!
ExtractMultipleToTemporary removeSelector: #selectFrom:to:!
!methodRemoval: ExtractMultipleToTemporary #sortUsingIntervalSizeFromMinToMaxOf: stamp: 'JEC 11/9/2025 16:23:25'!
ExtractMultipleToTemporary removeSelector: #sortUsingIntervalSizeFromMinToMaxOf:!
!methodRemoval: ExtractMultipleToTemporary #sortedCollectionOf: stamp: 'JEC 11/9/2025 16:22:19'!
ExtractMultipleToTemporary removeSelector: #sortedCollectionOf:!
!methodRemoval: ExtractMultipleToTemporary #isValidToDeclareTemp: stamp: 'JEC 11/9/2025 16:15:52'!
ExtractMultipleToTemporary removeSelector: #isValidToDeclareTemp:!
!methodRemoval: ExtractMultipleToTemporary #text stamp: 'JEC 10/20/2025 18:14:49'!
ExtractMultipleToTemporary removeSelector: #text!
!methodRemoval: ExtractMultipleToTemporary #initializeNamed:extractingCodeAt:from:methodNode: stamp: 'JEC 11/10/2025 20:05:52'!
ExtractMultipleToTemporary removeSelector: #initializeNamed:extractingCodeAt:from:methodNode:!
!methodRemoval: ExtractMultipleToTemporary #sortedCollectionByIntervalSizeOf: stamp: 'JEC 11/10/2025 20:07:45'!
ExtractMultipleToTemporary removeSelector: #sortedCollectionByIntervalSizeOf:!
!methodRemoval: ExtractMultipleToTemporary #siblingStatementsOfTemporaryAssignment stamp: 'JEC 10/20/2025 18:17:35'!
ExtractMultipleToTemporary removeSelector: #siblingStatementsOfTemporaryAssignment!
!methodRemoval: ExtractMultipleToTemporary #sourceRangeToDeclareNewTemporary stamp: 'JEC 11/10/2025 20:07:47'!
ExtractMultipleToTemporary removeSelector: #sourceRangeToDeclareNewTemporary!
!methodRemoval: ExtractMultipleToTemporary #initializeNamed:extractingCodeAt:from:methodNode:declaringTempsIn: stamp: 'JEC 10/11/2025 21:04:35'!
ExtractMultipleToTemporary removeSelector: #initializeNamed:extractingCodeAt:from:methodNode:declaringTempsIn:!
!methodRemoval: ExtractToTemporary class #allNonEmptyBlocksNodesAt:in: stamp: 'JEC 11/10/2025 20:27:57'!
ExtractToTemporary class removeSelector: #allNonEmptyBlocksNodesAt:in:!
!methodRemoval: ExtractToTemporary class #assertThereAreIntervalsToExtract: stamp: 'JEC 10/11/2025 20:48:35'!
ExtractToTemporary class removeSelector: #assertThereAreIntervalsToExtract:!
!methodRemoval: ExtractToTemporary class #canDeclareTempOf:in: stamp: 'JEC 1/16/2026 08:24:07'!
ExtractToTemporary class removeSelector: #canDeclareTempOf:in:!
!methodRemoval: ExtractToTemporary class #allNoneEmptyBlockParseNodesFrom:in: stamp: 'JEC 11/10/2025 20:29:12'!
ExtractToTemporary class removeSelector: #allNoneEmptyBlockParseNodesFrom:in:!
!methodRemoval: ExtractToTemporary class #parseNodeToDeclareTempFrom:of:ifAbsent: stamp: 'JEC 1/16/2026 08:24:12'!
ExtractToTemporary class removeSelector: #parseNodeToDeclareTempFrom:of:ifAbsent:!
!methodRemoval: ExtractToTemporary class #errorMessageForExtractingDifferentPiecesOfCode stamp: 'JEC 10/11/2025 20:40:04'!
ExtractToTemporary class removeSelector: #errorMessageForExtractingDifferentPiecesOfCode!
!methodRemoval: ExtractToTemporary class #signalThereAreNoIntervalsToExtract stamp: 'JEC 10/11/2025 20:47:58'!
ExtractToTemporary class removeSelector: #signalThereAreNoIntervalsToExtract!
!methodRemoval: ExtractToTemporary class #sortedCollectionByIntervalSizeOf: stamp: 'JEC 1/16/2026 08:23:56'!
ExtractToTemporary class removeSelector: #sortedCollectionByIntervalSizeOf:!
!methodRemoval: ExtractToTemporary class #allBlockNodesAt:in: stamp: 'JEC 1/16/2026 08:23:21'!
ExtractToTemporary class removeSelector: #allBlockNodesAt:in:!
!methodRemoval: ExtractToTemporary class #allBlockNodesFrom:in: stamp: 'JEC 1/16/2026 08:23:32'!
ExtractToTemporary class removeSelector: #allBlockNodesFrom:in:!
!methodRemoval: ExtractMethodReplacement #intervalToExtract stamp: 'JEC 12/3/2025 19:04:04'!
ExtractMethodReplacement removeSelector: #intervalToExtract!
!methodRemoval: ExtractRepeatedCodeWindow class #openFrom:with: stamp: 'JEC 12/3/2025 19:04:04'!
ExtractRepeatedCodeWindow class removeSelector: #openFrom:with:!
!methodRemoval: ExtractRepeatedCodeWindow #extractSelectionOnly stamp: 'JEC 12/3/2025 19:04:04'!
ExtractRepeatedCodeWindow removeSelector: #extractSelectionOnly!
!methodRemoval: RepeatedCodeMessageSet class #finder: stamp: 'JEC 12/3/2025 19:04:04'!
RepeatedCodeMessageSet class removeSelector: #finder:!
!methodRemoval: RepeatedCodeMessageSet #initializeFinder: stamp: 'JEC 12/3/2025 19:04:04'!
RepeatedCodeMessageSet removeSelector: #initializeFinder:!
!methodRemoval: CommonScopeNodeFinder #addFoundNode: stamp: 'JEC 1/16/2026 08:53:02'!
CommonScopeNodeFinder removeSelector: #addFoundNode:!
!methodRemoval: MethodNode #tempsMark stamp: 'JEC 11/3/2025 18:22:24'!
MethodNode removeSelector: #tempsMark!
!methodRemoval: MethodNode #sourceRange stamp: 'JEC 12/3/2025 19:04:04'!
MethodNode removeSelector: #sourceRange!
!methodRemoval: MethodNode #temporaryVariablesInterval stamp: 'JEC 10/11/2025 19:34:14'!
MethodNode removeSelector: #temporaryVariablesInterval!
!methodRemoval: MethodNode #parseNodesPathAndIntervalsAt:ifAbsent: stamp: 'JEC 11/3/2025 19:01:05'!
MethodNode removeSelector: #parseNodesPathAndIntervalsAt:ifAbsent:!
!methodRemoval: MethodNode #blockParseNodesPathAndIntervalsAt:ifAbsent: stamp: 'JEC 11/9/2025 16:09:05'!
MethodNode removeSelector: #blockParseNodesPathAndIntervalsAt:ifAbsent:!
!methodRemoval: BlockNode #temporaryVariablesInterval stamp: 'JEC 10/11/2025 19:34:00'!
BlockNode removeSelector: #temporaryVariablesInterval!
!methodRemoval: BlockNode #isEmptyBlock stamp: 'JEC 11/10/2025 20:27:33'!
BlockNode removeSelector: #isEmptyBlock!
!methodRemoval: CodeNode #temporaryVariablesInterval stamp: 'JEC 10/11/2025 19:26:59'!
CodeNode removeSelector: #temporaryVariablesInterval!

!CommonScopeNodeFinder reorganize!
('accessing' commonScopeNode)
('assertions' assertHasFoundAllNodes)
('initialization' initializeForAll:in: sortByIntervalPositions:)
('visiting' visitAssignmentNode: visitBacktickNode: visitBlockNode: visitBraceNode: visitCascadeNode: visitInstanceVariableNode: visitLiteralNode: visitLiteralVariableNode: visitMessageNode: visitMessageNodeInCascade: visitMethodNode: visitNewArrayNode: visitRemoteTempVectorNode: visitReturnNode: visitSelectorNode: visitTempVariableNode: visitTemporariesDeclarationNode: visitTemporaryDeclarationNode: visitUndeclaredVariableNode: visitVariableNode:)
('private - visiting' actualScope addNodeIfFound: addToScopeList: completeSourceRangesOf: detectNodeToFind:ifFound:ifNone: includesActualScopeRangeAllOf: isNodeToFind: isScopeAddedWithRange: nodeRangeOf: scopesOf: visitBlockNodeWithUpdatedScope: visitMethodNodeBlockOf:)
!


!CommonScopeNodeFinder class reorganize!
('error messages' couldNotFindANodeErrorMessage)
('instance creation' forAll:in:)
!


!ScopeNode reorganize!
('accessing' addFoundNodeInScope: parseNode rangeSize)
('initialization' initializeNode:range:)
('printing' printOn:)
('testing' hasRange: haveNodesFound isIncludedIn: rangeIncludesAllOf:)
!

!classDefinition: #RepeatedCodeMessageSet category: #'Tools-Refactoring'!
MethodSet subclass: #RepeatedCodeMessageSet
	instanceVariableNames: 'finder selectedIndex multimethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ExtractRepeatedCodeWindow category: #'Tools-Refactoring'!
MethodSetWindow subclass: #ExtractRepeatedCodeWindow
	instanceVariableNames: 'applier finder multimethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!NodesInRangeFinder class reorganize!
('error description' nodesNotFoundErrorDescription)
('instance creation' in:for:)
!


!ExtractToTemporary class reorganize!
('error messages' cannotExtractTemporaryWithoutIntervalsErrorMessage errorMessageForSourceCodeSelectionOutOfBounds errorMessageForSourceCodeToExtractCanNotBeEmpty extractingDifferentPiecesOfCodeErrorMessage)
('exceptions' signalCannotExtractTemporaryWithoutIntervals signalOutOfBoundsIntervalError signalSourceCodeToExtractCanNotBeEmpty)
('instance creation' named:at:from: named:intervals:from:)
('preconditions' assertCanApplyRefactoringOn:at: assertIntervalsIsNotEmpty:)
('private - parsing' is:withinBoundsOf: methodOrBlockNodeIncluding:in: methodOrBlockNodeIncludingAll:in: parseNodesToFindCommonScopeOf:in:)
('private - preconditions' assert:isValidIntervalOn: assertSourceCodeIsNotEmpty: newTemporaryPreconditionClass)
!


!ExtractMultipleToTemporary reorganize!
('initialization' initializeNamed:extractingCodeAt:from:methodNode:declaringTempIn:)
('private - applying steps' replaceAllCodeIntervalsWithNewVariable resolveNewVariableAssignment)
!


!ExtractMultipleToTemporary class reorganize!
('exceptions' signalExtractingDifferentPiecesOfCodeError)
('instance creation' named:intervals:from:)
('preconditions' applyPreconditionsOn:named:in: assertCanApplyRefactoringOn:atAll:)
('private' codeNodesOf:in: trimmedIntervalsOf:in:)
!

!classDefinition: #ExtractToTemporaryApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #ExtractToTemporaryApplier
	instanceVariableNames: 'intervalToExtract methodToExtractCodeFrom newVariable codeProvider finder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractToTemporaryApplier reorganize!
('evaluating' value valueCreatingWith: valueWithAllReplacements valueWithOriginalSelection)
('initialization' initializeOn:for:of:)
('refactoring - changes' showChanges)
('refactoring - creation' createRefactoring createRefactoringForOriginalSelection refactoringClass)
('refactoring - parameters request' askNewVariableName findReplacementsInMethod requestRefactoringParameters)
!


!ReplacementsFinder reorganize!
('evaluating' value)
('replacements' inClassReplacements removeReplacementAt: replacements replacementsAt:ifAbsent: sourceMethodReplacements)
('source code' sourceCodeToExtract)
('testing' hasOneReplacement)
('private-replacement finding' addReplacementAt:in: createReplacementAt:in: defineFindingStrategy find findReplacementsUsingNodeEquivalenceIn: findReplacementsUsingStringEqualityIn: initializeFinding nodesToExtract)
!

!classDefinition: #ExtractMethodReplacementsFinder category: #'Tools-Refactoring'!
ReplacementsFinder subclass: #ExtractMethodReplacementsFinder
	instanceVariableNames: 'newMessage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ExtractMethodReplacementsFinder reorganize!
('initialization' initializeOfCodeIn:at:to:)
('private-replacement finding' createReplacementAt:in: find findReplacementsAt:)
('replacements' methodsToReplace originalSelectionReplacement)
('evaluating')
!


!ExtractToTemporaryReplacementsFinder reorganize!
('evaluating')
('initialization' initializeNamed:at:from:)
('replacements' createReplacementAt:in: methodsToReplace originalSelectionReplacement)
('private-replacement finding' find)
!


!ExtractToTemporaryReplacementsFinder class reorganize!
('instance creation' named:at:from:)
!


'From Cuis7.9 [latest update: #8046] on 3 July 2026 at 2:46:38 pm'!

!ParseNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025 19:06:35'!
isAssignmentToTemporaryAssigningTo: aTempVarNode

	^ false! !


!AssignmentNode methodsFor: 'testing' stamp: 'JEC 8/Jan/2026 19:05:06'!
anySourceRangesOfAssigningValueIn: methodNode satisfy: aBlock 

	| sourceRanges |

	sourceRanges := methodNode completeSourceRangesOf: value ifAbsent: [ ^ false ].
	^ sourceRanges anySatisfy: aBlock. ! !


!AssignmentNode methodsFor: 'testing' stamp: 'JEC 19/Dec/2025 19:57:24'!
assignsToVariable: aVariableNode 

	^ variable equivalentTo: aVariableNode! !


!AssignmentNode methodsFor: 'testing' stamp: 'JEC 21/Dec/2025 12:24:24'!
assignsToVariableName: aName

	^ variable isNamed: aName! !


!AssignmentNode methodsFor: 'testing' stamp: 'JEC 8/Jan/2026 19:00:08'!
isAssigningACascadeNode

	^ value isCascadeNode! !


!AssignmentNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025 19:07:26'!
isAssignmentToTemporaryAssigningTo: aTempVarNode

	^ self isAssignmentToTemporary and: [ self assignsToVariable: aTempVarNode ]! !


!CascadeNode methodsFor: 'testing' stamp: 'JEC 10/Jan/2026 21:06:06'!
isEquivalentToAMessageNode: aNode 

	^ messages anySatisfy: [ :aMessageNode | aMessageNode equivalentTo: aNode ]! !


!CascadeNode methodsFor: 'testing' stamp: 'JEC 10/Jan/2026 21:02:44'!
isEquivalentToReceiverNode: aNode

	^ receiver equivalentTo: aNode! !


!BlockNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025 15:57:13'!
temporariesDeclarationDeclares: tempVarNode 

	^ self temporariesDeclaration declaresVariable: tempVarNode! !


!MethodNode methodsFor: 'source mapping' stamp: 'JEC 31/Dec/2025 09:31:18'!
completeSourceRangesWithin: anInterval do: aBinaryBlock

	^ self completeSourceRangesDo: [ :parseNode :sourceRanges |
		| sourceRangesInInterval |
		
		sourceRangesInInterval := sourceRanges select: [ :sourceRange | anInterval includes: sourceRange first ].
		
		sourceRangesInInterval 
			ifNotEmpty: [
				aBinaryBlock 
					value: parseNode
					value: sourceRangesInInterval ] ]! !


!MethodNode methodsFor: 'source mapping' stamp: 'JEC 28/Dec/2025 15:49:06'!
nodeAndSourceRangesThatDeclares: tempVarNode

	self completeSourceRangesDo: [ :parseNode :sourceRanges |  
		(parseNode isBlockNode and: [ parseNode temporariesDeclarationDeclares: tempVarNode ])
		ifTrue: [ ^ parseNode -> sourceRanges ] ].

	"If there is not a block node that declares that variable, so the method node is the one"
	^ self -> { 1 to: self sourceText size }! !


!MethodNode methodsFor: 'source mapping' stamp: 'JEC 27/Dec/2025 20:48:38'!
sourceRangesAssignmentsTo: tempVarNode

	| assignmentsSourceRanges |
	
	assignmentsSourceRanges := OrderedCollection new.
	
	self completeSourceRangesDo: [ :parseNode :sourceRanges |
		(parseNode isAssignmentNode and: [ parseNode assignsToVariableName: tempVarNode name ])
			ifTrue: [ assignmentsSourceRanges addAll: sourceRanges ] ].
	
	^ assignmentsSourceRanges! !


!MethodNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025 19:08:13'!
anyAssignmentNodeWithin: intervalToExtract assignsTo: tempVarNode 

	^ self
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode | parseNode isAssignmentToTemporaryAssigningTo: tempVarNode ]! !


!MethodNode methodsFor: 'testing' stamp: 'JEC 27/Dec/2025 20:22:51'!
anyDeclarationNodeWithin: intervalToExtract declaresVariable: tempVarNode

	^ self
		anyParseNodeWithin: intervalToExtract
		satisfy: [ :parseNode |
			parseNode isTemporariesDeclaration
				and: [ parseNode declaresVariable: tempVarNode ] ]! !


!MethodNode methodsFor: 'testing' stamp: 'JEC 28/Dec/2025 20:31:29'!
hasReturnExpressions

	self completeSourceRangesDo: [ :parseNode :sourceRanges | 
		parseNode isReturn ifTrue: [ ^ true ] ].
	
	^ false! !


!ReturnNode methodsFor: 'printing' stamp: 'JEC 3/Jan/2026 21:46:12'!
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


!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 10/Jan/2026 22:06:36'!
addEquivalentNodesInCascadeNode: aCascadeNode 

	(self areNodesToLookForAPartiallyCascadeNode: aCascadeNode)
		ifTrue: [ self addToEquivalentsNodesPartiallyOf: aCascadeNode ]
		ifFalse: [ self addToEquivalentsIfOneStatementEquivalentTo: aCascadeNode. ]! !


!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 10/Jan/2026 22:06:36'!
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


!EquivalentNodesFinder methodsFor: 'private - visiting' stamp: 'JEC 10/Jan/2026 22:06:14'!
areNodesToLookForAPartiallyCascadeNode: aCascadeNode 

	^ nodesToLookFor 
		allSatisfy: [ :aNode | 
			(aCascadeNode isEquivalentToReceiverNode: aNode) 
			or: [ aCascadeNode isEquivalentToAMessageNode: aNode ] ]! !


!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'JEC 4/Jan/2026 19:28:17'!
visitReturnNode: aReturnNode

	self addToEquivalentsIfOneStatementEquivalentTo: aReturnNode.

	 ^ super visitReturnNode: aReturnNode ! !


!CompiledMethod methodsFor: 'accessing' stamp: 'JEC 13/Apr/2026 12:58:03'!
messageFromSelector

	^ Message 
		selector: self selector
		arguments: (self methodNode arguments collect: [ :argument | argument name ])! !


!Stream methodsFor: 'writing' stamp: 'JEC 18/Dec/2025 08:55:11'!
nextPutAllValueOf: aBlock when: aCondition

	aCondition ifTrue: [ self nextPutAll: aBlock value ].
	! !


!Message methodsFor: 'accessing' stamp: 'JEC 29/Dec/2025 19:33:12'!
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


!Message methodsFor: 'testing' stamp: 'JEC 16/Dec/2025 17:36:07'!
hasArgument: anArgument

	^ self arguments anySatisfy: [ :argument | argument = anArgument ]! !


!MethodReference methodsFor: 'printing' stamp: 'JEC 13/Apr/2026 19:28:21'!
classAndSelector

	^ String streamContents: [ :stream | self printClassAndSelectorOn: stream ]! !


!EquivalentNodesFinder methodsFor: 'visiting' stamp: 'JEC 10/Jan/2026 22:04:22'!
visitCascadeNode: aCascadeNode 

	self addEquivalentNodesInCascadeNode: aCascadeNode.

	 ^super visitCascadeNode: aCascadeNode ! !


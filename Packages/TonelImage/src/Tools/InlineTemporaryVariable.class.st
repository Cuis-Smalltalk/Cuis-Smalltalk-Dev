"
I am a refactoring that replaces references to a temporary variable for its actual value.
Implementation notes:

- If the refactoring is initiated from the declaration of the variable, then it must have at most one assignment.
- If there are no assignments, the only effect is the removal of the declaration.
- If the refactoring is triggered from a usage, the value used is the one from the previous last assignment and
the scope is limited to all usages up the next assignment, if any, or the end of the method.
"
Class {
	#name : 'InlineTemporaryVariable',
	#superclass : 'Refactoring',
	#instVars : [
		'variableToInline',
		'methodToRefactor',
		'updatedSourceCode',
		'methodNode',
		'oldVariableNode',
		'usageToInline',
		'sourceCodeChanges',
		'methodOrBlockNodeDeclaringTemporary',
		'rangeOfNodeDeclaringTemporary',
		'assignmentToInlineRange',
		'endOfNodeEnclosingAssignment'
	],
	#classInstVars : [
		'variableToInline',
		'blockNodeDeclaringTemporary'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'errors' }
InlineTemporaryVariable class >> assignmentNotFoundErrorMessage [

	^'Variable is never assigned'

]

{ #category : 'instance creation - private' }
InlineTemporaryVariable class >> findTemporaryNamed: temporaryName atUsage: usageInterval inMethodNode: aMethodNode [

	| oldVariableNodeAndUsageInterval parseNodesPath |

	parseNodesPath := aMethodNode parseNodesPathAt: usageInterval first ifAbsent: [self signalSelectedIntervalIsNotATemporary].
	oldVariableNodeAndUsageInterval := parseNodesPath
		detect: [:anAssociation |
			| parseNode |
			parseNode := anAssociation key.
			(parseNode isTemp and: [parseNode isNamed: temporaryName])]
		ifNone:[self signalSelectedIntervalIsNotATemporary].

	^{oldVariableNodeAndUsageInterval key. oldVariableNodeAndUsageInterval value}.

]

{ #category : 'errors' }
InlineTemporaryVariable class >> moreThanOneAssignmentErrorMessage [

	^'There are multiple assignments that could be inlined. Please select a specific usage to inline'.

]

{ #category : 'instance creation' }
InlineTemporaryVariable class >> named: tempVarToInlineName atUsageInterval: usageInterval inMethod: compiledMethodToRefactor [

	| oldVariableNodeAndUsageInterval methodNode |

	methodNode := compiledMethodToRefactor notOptimizedMethodNode.
	oldVariableNodeAndUsageInterval := self findTemporaryNamed: tempVarToInlineName atUsage: usageInterval
		inMethodNode: methodNode.

	^self new
		initializeOldVariableNode: oldVariableNodeAndUsageInterval first
		usage: oldVariableNodeAndUsageInterval second
		method: compiledMethodToRefactor
		methodNode: methodNode.
]

{ #category : 'errors' }
InlineTemporaryVariable class >> selectionIsNotATemporaryVariableErrorMessage [

	^'Selected interval is not a temporary variable'.
]

{ #category : 'errors' }
InlineTemporaryVariable class >> signalAssignmentNotFound [

	^self refactoringError: (self assignmentNotFoundErrorMessage)






]

{ #category : 'errors' }
InlineTemporaryVariable class >> signalMoreThanOneAssignmentError [

	self refactoringError: self moreThanOneAssignmentErrorMessage.






]

{ #category : 'errors' }
InlineTemporaryVariable class >> signalSelectedIntervalIsNotATemporary [

	^self refactoringError: self selectionIsNotATemporaryVariableErrorMessage

]

{ #category : 'errors' }
InlineTemporaryVariable class >> signalTemporaryAssignedAfterUsedInBlock [

	^self refactoringError: (self temporaryAssignedAfterUsedInBlockErrorMessage)






]

{ #category : 'errors' }
InlineTemporaryVariable class >> signalTemporaryIsAssignedInsideInBlockBeforeReference [

	^self refactoringError: (self temporaryIsAssignedInBlockBeforeReferenceErrorMessage)






]

{ #category : 'errors' }
InlineTemporaryVariable class >> temporaryAssignedAfterUsedInBlockErrorMessage [

	^'Temporary is assigned after being referenced in a block'.






]

{ #category : 'errors' }
InlineTemporaryVariable class >> temporaryIsAssignedInBlockBeforeReferenceErrorMessage [

	^'Temporary to inline is previously is assigned inside a block'






]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> addParenthesesIfNeededTo: anExpression [

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |

 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) == $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) == $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [anExpression]
		ifFalse: ['(', anExpression, ')'].
]

{ #category : 'applying' }
InlineTemporaryVariable >> apply [
	
	self usageToInlineIsTemporaryDeclaration
		ifTrue: [self inlineFromTemporaryDeclaration]
		ifFalse: [self inlineFromNonDeclarationUsage].

	self compileChanges.


]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> assertTemporaryHasAtMostOneAssignment [

	self atMostOneAssignment ifFalse: [self class signalMoreThanOneAssignmentError].
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> assertTemporaryIsNotAssignedAfterUsedInBlock [
	
	| assignmentToTemporaryPositions |
	
	assignmentToTemporaryPositions := self selectCompleteRangesOfAssignmentsToTemporaryFrom: methodNode 
		thatMatch: [:assignmentRange | rangeOfNodeDeclaringTemporary includesAllOf: assignmentRange.].
	
	methodNode completeSourceRanges associationsDo: [:parseNodeAndRanges | (parseNodeAndRanges key isBlockNode 
		and: [parseNodeAndRanges value anySatisfy: [:range | (rangeOfNodeDeclaringTemporary includesAllOf: range)
				and: [(methodNode positionsForTemporaryVariable: variableToInline ifAbsent: [#()]) 
					anySatisfy: [:tempReferenceInterval | range includesAllOf: tempReferenceInterval]]
				and: [assignmentToTemporaryPositions anySatisfy: [:tempAssignmentPosition | tempAssignmentPosition first > range last]]]])
					ifTrue: [self class signalTemporaryAssignedAfterUsedInBlock]]
	

]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> assertTemporaryNotAssignedInBlock [

	methodNode completeSourceRangesDo: [:node :ranges |
		
		(node isBlockNode and: [ranges first last < usageToInline first] and: [
			(self selectCompleteRangesOfAssignmentsToTemporaryFrom: node 
				thatMatch: 	[:assignmentRange | rangeOfNodeDeclaringTemporary includesAllOf: assignmentRange ]) isEmpty not.
		]) ifTrue: [self class signalTemporaryIsAssignedInsideInBlockBeforeReference]
	]
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> assertUnassignedTemporaryIsNotReferenced [

	((methodNode positionsForTemporaryVariable: variableToInline
		ifAbsent: [self shouldNotHappen]) size > 1) ifTrue: [self class signalAssignmentNotFound]







]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> assignmentNodeToInline: assignmentToInline [

	methodNode nodesDo: [:aNode | ((self nodeIsAssignmentToTemporaryToInline: aNode)
		and: [
			|ranges|

			ranges := (methodNode rangeForNode: aNode ifAbsent: [#()]).
			(methodNode isMultipleRanges: ranges) ifFalse: [ranges := {ranges}].
			ranges anySatisfy: [:range | assignmentToInline includesAllOf: range].
		])
		ifTrue: [^aNode]].

	self shouldNotHappen.





]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> assignmentsToVariableCount [

	| count |

	count := 0.
	methodOrBlockNodeDeclaringTemporary nodesDo: [:aNode |
		(self nodeIsAssignmentToTemporaryToInline: aNode) ifTrue: [count := count + 1 ].
	].

	^count.






]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> atMostOneAssignment [

	^self assignmentsToVariableCount <= 1.






]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> closestAssignmentBeforeUsage: assignments [


	^(assignments
		select: [:assignmentRange | (assignmentRange includesAllOf: usageToInline)
			or: [assignmentRange last < usageToInline first]])
		detectMax: [:assignmentRange | assignmentRange last]





]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> compileChanges [

	updatedSourceCode := updatedSourceCode copyReplacing: sourceCodeChanges.

	^methodToRefactor methodClass compile: updatedSourceCode classified: methodToRefactor category
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> computeEndOfNodeEnclosingAssignment [

	endOfNodeEnclosingAssignment := (methodNode parseNodesPathAt: assignmentToInlineRange first
		ifAbsent: [self shouldNotHappen])
			detect: [:nodeAndRange | nodeAndRange key isBlockNode]
			ifFound: [:nodeAndRangeOfAssignment | nodeAndRangeOfAssignment value last ]
			ifNone: [methodNode sourceText size].





]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> findAssignmentOfUsage [

	| lastAssignment assignments |

	assignments := self selectCompleteRangesOfAssignmentsToTemporaryFrom: methodOrBlockNodeDeclaringTemporary thatMatch:
		[:range | (range includesAllOf: usageToInline) or: [range last < usageToInline first] ].

	lastAssignment := assignments detectMax: [:assignment | assignment last].

	lastAssignment ~= nil ifTrue: [^lastAssignment ] ifFalse: [self class signalAssignmentNotFound]






]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> findLastIndexOfFirstAssignmentAfter: assignmentToInline [

	| nextAssignmentsRanges |


	nextAssignmentsRanges := self selectCompleteRangesOfAssignmentsToTemporaryFrom: methodOrBlockNodeDeclaringTemporary
		thatMatch: [:range | range first > assignmentToInline last].

	^nextAssignmentsRanges collect: [:assignmentRange | assignmentRange last]
		andFold: [:rangeEnd1 :rangeEnd2 | rangeEnd1 min: rangeEnd2]
		ifEmpty: nil







]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> findOnlyAssignmentRangeInNode: aMethodOrBlockNode [

	^(self selectCompleteRangesOfAssignmentsToTemporaryFrom: aMethodOrBlockNode
	 	thatMatch: [:aRange | true])
			ifEmpty: [^nil]
			ifNotEmpty: [:rangesSet | rangesSet anyOne].
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> findReferencesBetweenAssignmentAndIndex: anIndex [

	^((methodNode completeSourceRangesOf: oldVariableNode ifAbsent: [self shouldNotHappen])
		select: [:range | (range first >= assignmentToInlineRange last) and: [range first <= anIndex]
			and: [(self rangeIsLeftPartOfAssignmentToTemporary: range) not	]])
]

{ #category : 'initialization' }
InlineTemporaryVariable >> initializeOldVariableNode: aVariableNodeToInline usage: aUsageToInline method: aCompiledMethodToRefactor methodNode: aMethodNode [

	| parseNodesPath  |

	oldVariableNode := aVariableNodeToInline.
	usageToInline := aUsageToInline.
	variableToInline := aVariableNodeToInline name.
	methodToRefactor := aCompiledMethodToRefactor.
	updatedSourceCode := methodToRefactor sourceCode.
	methodNode := aMethodNode.
	parseNodesPath := methodNode parseNodesPathAt: usageToInline first ifAbsent: [self class signalSelectedIntervalIsNotATemporary].
	methodOrBlockNodeDeclaringTemporary := (parseNodesPath collect: [:anAssociation | anAssociation key])
		detect: [:aParseNode | aParseNode isBlockNode and: [aParseNode hasArgumentOrTemporaryNamed: variableToInline]]
		ifNone: methodNode.
	rangeOfNodeDeclaringTemporary := (methodOrBlockNodeDeclaringTemporary = methodNode)
		ifTrue: [1 to: (methodNode sourceText size)]
		ifFalse: [(methodNode completeSourceRangesOf: methodOrBlockNodeDeclaringTemporary ifAbsent: [self shouldNotHappen])
			detect: [:aSourceRange | aSourceRange includesAllOf: usageToInline]].
	sourceCodeChanges := SortedCollection sortBlock: [ :left :right | left key first < right key first ].


]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineAssignment [

	| endOfNextAssignmentIndex endOfRefactoringScopeIndex |

	endOfNextAssignmentIndex := self findLastIndexOfFirstAssignmentAfter: assignmentToInlineRange.
	self assertTemporaryNotAssignedInBlock.
	self assertTemporaryIsNotAssignedAfterUsedInBlock.
	self computeEndOfNodeEnclosingAssignment.
	endOfRefactoringScopeIndex := endOfNextAssignmentIndex ifNil: [endOfNodeEnclosingAssignment ].
	
	self inlineAssignment: assignmentToInlineRange upTo: endOfRefactoringScopeIndex.

	(self atMostOneAssignment and: [self noReferencesToTemporaryAfter: endOfRefactoringScopeIndex ])
		ifTrue: [self removeTemporaryVariableDeclaration].





]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineAssignment: assignmentToInline upTo: anIndex [

	| assignmentNode assignmentNodeValue expression expressionRange |

	assignmentNode := self assignmentNodeToInline: assignmentToInline.
	assignmentNodeValue := assignmentNode value.
	expressionRange := (methodNode completeSourceRangesOf: assignmentNodeValue ifAbsent: [self shouldNotHappen])
		detect: [:range | assignmentToInline includesAllOf: range].
	expression := methodNode sourceText copyFrom: expressionRange first to: expressionRange last.

	self inlineAssignmentNode: assignmentNode withExpression: expression upTo: anIndex;
		removeAssignment: assignmentToInline.
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineAssignmentNode: anAssignmentNode withExpression: anExpression upTo: anIndex [

	| assignmentNodeValue |

	assignmentNodeValue := anAssignmentNode value.
	assignmentNodeValue isMessageNode
		ifTrue: [
			assignmentNodeValue isUnaryMessageSend
				ifTrue: [self inlineTemporaryNodeWithLiteralOrUnaryMessageExpression: anExpression
					upTo: anIndex].
			assignmentNodeValue isInfix
				ifTrue: [self inlineTemporaryNodeWithBinaryMessageExpression: anExpression
					upTo: anIndex].
			assignmentNodeValue isKeywordMessageSend
				ifTrue: [self inlineTemporaryNodeWithKeywordMessageExpression: anExpression
					upTo: anIndex].

		]
		ifFalse: [self inlineTemporaryNodeWithLiteralOrUnaryMessageExpression: anExpression upTo: anIndex].
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineFromNonDeclarationUsage [

	assignmentToInlineRange := self findAssignmentOfUsage.
	self inlineAssignment.

]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineFromTemporaryDeclaration [

	self assertTemporaryHasAtMostOneAssignment.

	assignmentToInlineRange := self findOnlyAssignmentRangeInNode: methodOrBlockNodeDeclaringTemporary.

	assignmentToInlineRange ~= nil
		ifTrue: [self inlineAssignment]
		ifFalse: [
			self assertUnassignedTemporaryIsNotReferenced.
			self removeTemporaryVariableDeclaration
		]







]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineTemp: aTempNode usedIn: aRange insideBinaryMessageNode: aBinaryMessageNode withBinaryExpression: aBinaryExpression [

	(self temporaryNode: aTempNode referencedIn: aRange isArgumentIn: aBinaryMessageNode)
		ifTrue: [self replaceRange: aRange withParenthesisedExpression: aBinaryExpression]
		ifFalse: [self replaceRange: aRange withExpression: aBinaryExpression]


					
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineTemporaryNodeWithBinaryMessageExpression: anExpression upTo: anIndex [

	| referencesToInline |

	referencesToInline := self findReferencesBetweenAssignmentAndIndex: anIndex.
	referencesToInline do: [:range |
		| parentNode  |

		parentNode := (methodNode parseNodesPathAt: range first ifAbsent: [self shouldNotHappen]) second key.

		((parentNode isMessageNode) and: [parentNode isKeywordMessageSend not])
			ifTrue: [
				parentNode isUnaryMessageSend ifTrue: [self replaceRange: range withParenthesisedExpression: anExpression ].
				parentNode isInfix ifTrue: [self inlineTemp: oldVariableNode usedIn: range insideBinaryMessageNode: parentNode
					withBinaryExpression: anExpression].
			]
			ifFalse: [self replaceRange: range withExpression: anExpression.]
	].	
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineTemporaryNodeWithKeywordMessageExpression: anExpression upTo: anIndex [

	| referencesToInline |

	referencesToInline := self findReferencesBetweenAssignmentAndIndex: anIndex.
	referencesToInline do: [:range |
		| parentNode  |

		parentNode := (methodNode parseNodesPathAt: range first ifAbsent: [self shouldNotHappen]) second key.

		(parentNode isMessageNode)
			ifTrue: [
				parentNode isUnaryMessageSend ifTrue: [self replaceRange: range withParenthesisedExpression: anExpression ].
				((parentNode isInfix) or: parentNode isKeywordMessageSend) ifTrue: [(self tempNodeIsReceiverOrArgumentOfReceiverWhenUsedIn: range)
					ifTrue: [self replaceRange: range withParenthesisedExpression: anExpression]
					ifFalse: [self replaceRange: range withExpression: anExpression ]]
			]
			ifFalse: [self replaceRange: range withExpression: anExpression.]
	].	
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> inlineTemporaryNodeWithLiteralOrUnaryMessageExpression: anExpression upTo: anIndex [

	| referencesToInline |

	referencesToInline := self findReferencesBetweenAssignmentAndIndex: anIndex.

	referencesToInline do: [:range |
		self replaceRange: range withExpression: anExpression.
	].



	
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> noReferencesToTemporaryAfter: endOfRefactoringScopeIndex [

	^(methodNode consolidateAsCollection: (methodNode rangeForNode: oldVariableNode
		ifAbsent: [self shouldNotHappen ]))
			noneSatisfy: [:aPosition | aPosition first > endOfRefactoringScopeIndex].





]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> nodeIsAssignmentToTemporaryToInline: aNode [

	^ (aNode isAssignmentToTemporary) and: [aNode variable name = variableToInline]
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> range: aRange isArgumentIn: aBinaryMessageNode [

	^ (methodNode consolidateAsCollection: (methodNode rangeForNode: aBinaryMessageNode ifAbsent: 	[self shouldNotHappen]))
					anySatisfy: [:aBinaryMessageNodeRange | aBinaryMessageNodeRange includesAllOf: aRange]
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> rangeIsLeftPartOfAssignmentToTemporary: aRange [

	methodNode nodesDo: [:aNode |
		(self nodeIsAssignmentToTemporaryToInline: aNode)	 ifTrue: [
			| ranges completeRanges |

			ranges := methodNode consolidateAsCollection: (methodNode rangeForNode: aNode ifAbsent: [self shouldNotHappen ]).
			completeRanges := methodNode consolidateAsCollection:(methodNode completeSourceRangesOf: aNode ifAbsent: [self shouldNotHappen ]).
			((completeRanges anySatisfy: [:aCompleteRange | aCompleteRange includesAllOf: aRange])
				and: [ranges noneSatisfy: [:aNonCompleteRange | aNonCompleteRange includesAllOf: aRange ]]) ifTrue: [^true]
		]
	].

	^false

		
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> rangeIsPartOfTemporaryDeclaration: aRange [

	^(methodNode parseNodesPathAt: aRange first ifAbsent: [self shouldNotHappen])
		detect: [:aNodeAndRange | aNodeAndRange key isTemporariesDeclaration ]
		ifFound: [:foundNodeAndRange | ^true]
		ifNone: [^false].
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> removeAssignment: assignmentToInline [

	self replaceRange: assignmentToInline withExpression: ''.






]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> removeTemporaryVariableDeclaration [

	| temporariesDeclarationNodes temporariesDeclarationNode intervalToRemove sourceText |

	temporariesDeclarationNode := methodOrBlockNodeDeclaringTemporary temporariesDeclaration.
	temporariesDeclarationNodes := temporariesDeclarationNode temporaryDeclarationNodes.
	sourceText := methodNode sourceText.
	temporariesDeclarationNodes size = 1
		ifTrue: [
			intervalToRemove := (methodNode rangeForNode: temporariesDeclarationNode
				ifAbsent: [self shouldNotHappen ])
					asSourceCodeInterval expandUntilStartOfNextStatementOn: sourceText.
		]
		ifFalse: [
			| temporaryDeclarationNode |
			temporaryDeclarationNode := temporariesDeclarationNodes detect: [:aTemporaryDeclarationNode |
				aTemporaryDeclarationNode declaresVariable: oldVariableNode ].
			intervalToRemove := (methodNode rangeForNode: temporaryDeclarationNode ifAbsent: [self shouldNotHappen ])
				asSourceCodeInterval expandUntilNextNonBlankCharacterOn: sourceText.
		].

	self replaceRange: intervalToRemove withExpression: ''
	
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> replaceRange: aRange withExpression: anExpression [

	sourceCodeChanges add: (aRange -> anExpression).

	
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> replaceRange: aRange withParenthesisedExpression: anExpression [

	self replaceRange: aRange withExpression: (self addParenthesesIfNeededTo: anExpression).

	
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> selectCompleteRangesOfAssignmentsToTemporaryFrom: aParseNode thatMatch: aConditionBlock [

	| assignments |

	assignments := Set new.
	aParseNode nodesDo: [:aNode |
		(self nodeIsAssignmentToTemporaryToInline: aNode)
			ifTrue: [
				| nodeRanges satisfyingRanges |
				nodeRanges := methodNode completeSourceRanges at: aNode ifAbsent: nil.
				satisfyingRanges := (nodeRanges select: aConditionBlock)
					collect:[:range | range asSourceCodeInterval
						expandUntilStartOfNextStatementOn: methodNode sourceText].
				assignments addAll: (satisfyingRanges)]
	].

	^assignments







]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> tempNodeIsReceiverOrArgumentOfReceiverWhenUsedIn: aRange [

	methodNode nodesDo: [ :aNode |
		(aNode isMessageNode
			and: [(methodNode completeSourceRangesOf: aNode ifAbsent: [self shouldNotHappen])
				anySatisfy: [:completeRange | (completeRange includesAllOf: aRange) and: [completeRange last > aRange last] ]]
		 	and: [(aNode receiver equivalentTo: oldVariableNode)
				or: [aNode receiver isMessageNode and: [aNode receiver arguments
						anySatisfy: [:argument | argument equivalentTo: oldVariableNode]]]]	)
			ifTrue: [^true]
	].
	^false.
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> tempNodeToInline [

	^methodNode tempNodes select: [:aNode | aNode name = variableToInline].
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> temporaryNode: aTempNode referencedIn: aRange isArgumentIn: aBinaryMessageNode [

	^ (aBinaryMessageNode arguments first equivalentTo: aTempNode) and: [self range: aRange isArgumentIn: aBinaryMessageNode]
]

{ #category : 'applying primitives - private' }
InlineTemporaryVariable >> usageToInlineIsTemporaryDeclaration [

	^(methodNode parseNodesPathAt: usageToInline first ifAbsent: [self shouldNotHappen ])
		anySatisfy: [:aNodeAndRangeAssociation | aNodeAndRangeAssociation key isTemporaryDeclaration].






]

'From Cuis7.9 [latest update: #8180] on 14 September 2026 at 12:36:05 pm'!

!ParseNode methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 11:21:22 pm (UTC)'!
requiresParenthesesOnAnArgumentForAnExpressionOfPrecedence: aPrecedence

	^false! !

!ParseNode methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 11:21:22 pm (UTC)'!
requiresParenthesesOnTheReceiverForAnExpressionOfPrecedence: aPrecedence

	^false! !


!CascadeNode methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 11:21:22 pm (UTC)'!
requiresParenthesesOnTheReceiverForAnExpressionOfPrecedence: aPrecedence

	^messages first
		requiresParenthesesOnTheReceiverForAnExpressionOfPrecedence: aPrecedence! !


!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 11:26:08 pm (UTC)'!
isAWholeExpressionTheFirstOf: anEnclosingNodes

	^anEnclosingNodes
		ifEmpty: [ false ]
		ifNotEmpty: [ self isAWholeExpression: anEnclosingNodes first value ]! !

!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 11:26:08 pm (UTC)'!
isTheReceiverOf: aParentNodeAndRange

	| parentRange |

	parentRange := aParentNodeAndRange value
		trimToMatchExpressionOn: methodNode sourceText.

	^parentRange first = self intervalToReplace first! !


!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 12/Sep/2026, 11:17:47 pm (UTC)'!
replaceReference: aRange with: anExpression withPrecedence: aPrecedence

	| replacement |

	replacement := ExpressionReplacement
		of: aRange
		in: methodNode
		by: anExpression
		withPrecedence: aPrecedence.

	self
		replaceRange: replacement intervalToReplace
		withExpression: replacement sourceCodeToWrite! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 12/Sep/2026, 11:27:35 pm (UTC)'!
valueRangeOf: anAssignmentNode within: anAssignmentRange

	| valueRanges valueRange |

	valueRanges := methodNode
		completeSourceRangesOf: anAssignmentNode value
		ifAbsent: [ self shouldNotHappen ].
	valueRange := valueRanges
		detect: [ :aRange | anAssignmentRange includesAllOf: aRange ].

	^valueRange asSourceCodeInterval trimToMatchExpressionOn: methodNode sourceText! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 12/Sep/2026, 11:26:08 pm (UTC)'!
valueSourceCodeOf: anAssignmentNode within: anAssignmentRange

	| valueRange |

	valueRange := self valueRangeOf: anAssignmentNode within: anAssignmentRange.

	^methodNode sourceText copyFrom: valueRange first to: valueRange last! !


!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 11:26:08 pm (UTC)'!
requiresParentheses

	| enclosingNodes parentNodeAndRange |

	enclosingNodes := self enclosingNodes.
	(self isAWholeExpressionTheFirstOf: enclosingNodes) ifFalse: [ ^false ].
	parentNodeAndRange := enclosingNodes at: 2 ifAbsent: [ ^false ].

	^(self isTheReceiverOf: parentNodeAndRange)
		ifTrue: [
			parentNodeAndRange key
				requiresParenthesesOnTheReceiverForAnExpressionOfPrecedence: precedence ]
		ifFalse: [
			parentNodeAndRange key
				requiresParenthesesOnAnArgumentForAnExpressionOfPrecedence: precedence ]! !


!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 12/Sep/2026, 11:27:35 pm (UTC)'!
inlineAssignment: assignmentToInline upTo: anIndex

	| assignmentNode valueSourceCode |

	assignmentNode := self assignmentNodeToInline: assignmentToInline.
	valueSourceCode := self valueSourceCodeOf: assignmentNode within: assignmentToInline.
	self
		inlineAssignmentNode: assignmentNode
		withExpression: valueSourceCode
		upTo: anIndex.
	self removeAssignment: assignmentToInline! !

!InlineTemporaryVariable methodsFor: 'applying primitives - private' stamp: 'HAW 12/Sep/2026, 11:17:47 pm (UTC)'!
inlineAssignmentNode: anAssignmentNode withExpression: anExpression upTo: anIndex

	| precedence |

	precedence := anAssignmentNode value expressionPrecedence.
	(self findReferencesBetweenAssignmentAndIndex: anIndex) do: [ :aRange |
		self replaceReference: aRange with: anExpression withPrecedence: precedence ]! !

!methodRemoval: InlineTemporaryVariable #replaceRange:withParenthesisedExpression: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #replaceRange:withParenthesisedExpression:!
!methodRemoval: InlineTemporaryVariable #inlineTemporaryNodeWithLiteralOrUnaryMessageExpression:upTo: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #inlineTemporaryNodeWithLiteralOrUnaryMessageExpression:upTo:!
!methodRemoval: InlineTemporaryVariable #addParenthesesIfNeededTo: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #addParenthesesIfNeededTo:!
!methodRemoval: InlineTemporaryVariable #inlineTemporaryNodeWithKeywordMessageExpression:upTo: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #inlineTemporaryNodeWithKeywordMessageExpression:upTo:!
!methodRemoval: InlineTemporaryVariable #tempNodeIsReceiverOrArgumentOfReceiverWhenUsedIn: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #tempNodeIsReceiverOrArgumentOfReceiverWhenUsedIn:!
!methodRemoval: InlineTemporaryVariable #inlineTemporaryNodeWithBinaryMessageExpression:upTo: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #inlineTemporaryNodeWithBinaryMessageExpression:upTo:!
!methodRemoval: InlineTemporaryVariable #temporaryNode:referencedIn:isArgumentIn: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #temporaryNode:referencedIn:isArgumentIn:!
!methodRemoval: InlineTemporaryVariable #range:isArgumentIn: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #range:isArgumentIn:!
!methodRemoval: InlineTemporaryVariable #inlineTemp:usedIn:insideBinaryMessageNode:withBinaryExpression: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
InlineTemporaryVariable removeSelector: #inlineTemp:usedIn:insideBinaryMessageNode:withBinaryExpression:!
!methodRemoval: ExpressionReplacement #parentIn:requiresParenthesesToReplace: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
ExpressionReplacement removeSelector: #parentIn:requiresParenthesesToReplace:!
!methodRemoval: MessageNode #requiresParenthesesToReplace:withAnExpressionOfPrecedence: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
MessageNode removeSelector: #requiresParenthesesToReplace:withAnExpressionOfPrecedence:!
!methodRemoval: CascadeNode #requiresParenthesesToReplace:withAnExpressionOfPrecedence: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
CascadeNode removeSelector: #requiresParenthesesToReplace:withAnExpressionOfPrecedence:!
!methodRemoval: ParseNode #requiresParenthesesToReplace:withAnExpressionOfPrecedence: stamp: 'HAW 14/Sep/2026, 3:34:55 pm (UTC)'!
ParseNode removeSelector: #requiresParenthesesToReplace:withAnExpressionOfPrecedence:!

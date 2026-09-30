'From Cuis7.9 [latest update: #8184] on 28 September 2026 at 5:24:49 pm'!

!ParseNode methodsFor: 'testing' stamp: 'HAW 28/Sep/2026, 7:53:34 pm (UTC)'!
evaluatesOnlyLiteralsBefore: aParseNode

	^false! !


!AssignmentNode methodsFor: 'testing' stamp: 'HAW 28/Sep/2026, 7:53:34 pm (UTC)'!
evaluatesOnlyLiteralsBefore: aParseNode

	^true! !


!MessageNode methodsFor: 'testing' stamp: 'HAW 28/Sep/2026, 7:57:01 pm (UTC)'!
evaluatesOnlyLiteralsBefore: aParseNode

	| evaluatedNodes index |

	evaluatedNodes := self nodesInEvaluationOrder.
	index := evaluatedNodes identityIndexOf: aParseNode.

	^index > 0 and: [
		(evaluatedNodes first: index - 1) allSatisfy: [ :anEvaluatedNode |
			anEvaluatedNode isLiteralNode ] ]! !

!MessageNode methodsFor: 'accessing' stamp: 'HAW 28/Sep/2026, 7:57:01 pm (UTC)'!
nodesInEvaluationOrder

	^(Array with: self receiverOrCascadeReceiver) , originalArguments! !


!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 28/Sep/2026, 7:55:49 pm (UTC)'!
firstIncludingReturnOn: aSourceCode

	| index |

	index := self indexBeforeOpeningParenthesesOn: aSourceCode.

	^(index > 0 and: [ (aSourceCode at: index) = $^ ])
		ifTrue: [ index ]
		ifFalse: [ self first ]! !

!SourceCodeInterval methodsFor: 'source code' stamp: 'HAW 28/Sep/2026, 7:55:49 pm (UTC)'!
indexBeforeOpeningParenthesesOn: aSourceCode

	^(aSourceCode first: self first - 1) findLast: [ :aCharacter |
		aCharacter isSeparator not and: [ aCharacter ~= $( ] ]! !


!MessageNodeReference methodsFor: 'testing' stamp: 'HAW 28/Sep/2026, 7:55:49 pm (UTC)'!
isEnclosedBy: aSourceRange inside: aStatementSourceRange

	^(aStatementSourceRange includesAllOf: aSourceRange)
		and: [ aSourceRange includesAllOf: completeSourceRange ]! !

!MessageNodeReference methodsFor: 'testing' stamp: 'HAW 28/Sep/2026, 7:57:01 pm (UTC)'!
isFirstEvaluatedInside: aStatementSourceRange

	| nodes |

	nodes := self nodesEnclosingInside: aStatementSourceRange.

	^(1 to: nodes size - 1) allSatisfy: [ :anIndex |
		(nodes at: anIndex + 1) evaluatesOnlyLiteralsBefore: (nodes at: anIndex) ]! !

!MessageNodeReference methodsFor: 'accessing' stamp: 'HAW 28/Sep/2026, 7:56:34 pm (UTC)'!
nodesEnclosingInside: aStatementSourceRange

	| enclosingNodesAndRanges |

	enclosingNodesAndRanges := self nodesPath asArray select: [ :aNodeAndRange |
		self isEnclosedBy: aNodeAndRange value inside: aStatementSourceRange ].

	^enclosingNodesAndRanges collect: [ :aNodeAndRange | aNodeAndRange key ]! !

!MessageNodeReference methodsFor: 'accessing' stamp: 'HAW 28/Sep/2026, 7:55:49 pm (UTC)'!
nodesPath

	^self methodNode
		parseNodesPathAt: completeSourceRange first
		ifAbsent: [ #() ]! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 7:55:49 pm (UTC)'!
lineSeparatorAt: anIndex in: aSourceCode

	^String newLineString , (self indentationUpTo: anIndex in: aSourceCode)! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 7:57:01 pm (UTC)'!
changesEvaluationOrderHoistingAt: aStatementSourceRange of: aMessageNodeReference

	| sendsInStatement |

	sendsInStatement := self
		sendsToInlineAt: aStatementSourceRange
		of: aMessageNodeReference.

	^sendsInStatement size > 1 or: [
		(aMessageNodeReference isFirstEvaluatedInside: aStatementSourceRange) not ]! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 7:55:49 pm (UTC)'!
insert: aCollectionOfStatements before: aStatementSourceRange in: aSenderMethod

	| insertionPoint separator |

	aCollectionOfStatements ifEmpty: [ ^self ].
	insertionPoint := aStatementSourceRange asSourceCodeInterval
		firstIncludingReturnOn: aSenderMethod sourceCode.
	separator := self lineSeparatorAt: insertionPoint in: aSenderMethod sourceCode.

	self
		replaceRange: (insertionPoint to: insertionPoint - 1)
		withNewSourceCode: (separator join: aCollectionOfStatements) , separator
		inMethod: aSenderMethod! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 7:56:34 pm (UTC)'!
shouldEvaluateInBlock: aCollectionOfInlinedStatements at: aStatementSourceRange of: aMessageNodeReference

	^aCollectionOfInlinedStatements size > 1 and: [
		self
			changesEvaluationOrderHoistingAt: aStatementSourceRange
			of: aMessageNodeReference ]! !

!methodRemoval: InlineMethod #expressionRangeAt:enclosing: stamp: 'HAW 28/Sep/2026, 7:53:34 pm (UTC)'!
InlineMethod removeSelector: #expressionRangeAt:enclosing:!
!methodRemoval: InlineMethod #isFirstEvaluated:at: stamp: 'HAW 28/Sep/2026, 7:57:01 pm (UTC)'!
InlineMethod removeSelector: #isFirstEvaluated:at:!
!methodRemoval: InlineMethod #nodesEnclosing:at: stamp: 'HAW 28/Sep/2026, 7:56:34 pm (UTC)'!
InlineMethod removeSelector: #nodesEnclosing:at:!

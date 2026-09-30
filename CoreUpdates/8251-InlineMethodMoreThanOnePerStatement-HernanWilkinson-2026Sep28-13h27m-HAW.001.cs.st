'From Cuis7.9 [latest update: #8184] on 28 September 2026 at 2:45:28 pm'!

!MessageNodeReference methodsFor: 'source code' stamp: 'HAW 28/Sep/2026, 5:07:19 pm (UTC)'!
periodOf: anExpression endingStatementAt: aStatementSourceRange

	^((self isAtTheEndOf: aStatementSourceRange) and: [ anExpression last = $. ])
		ifTrue: [ '.' ]
		ifFalse: [ '' ]! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 5:07:19 pm (UTC)'!
insert: aCollectionOfStatements before: aStatementSourceRange in: aSenderMethod

	| insertionPoint separator |

	aCollectionOfStatements ifEmpty: [ ^self ].
	insertionPoint := aStatementSourceRange first.
	separator := String newLineString ,
		(self indentationUpTo: insertionPoint in: aSenderMethod sourceCode).

	self
		replaceRange: (insertionPoint to: insertionPoint - 1)
		withNewSourceCode: (separator join: aCollectionOfStatements) , separator
		inMethod: aSenderMethod! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 5:07:19 pm (UTC)'!
replace: aMessageNodeReference usedInStatementAt: aStatementSourceRange by: anExpression withPrecedence: aPrecedence

	| replacement period |

	replacement := ExpressionReplacement
		of: aMessageNodeReference completeSourceRange
		in: aMessageNodeReference methodNode
		by: (anExpression withoutPossibleSuffix: '.')
		withPrecedence: aPrecedence.
	period := aMessageNodeReference
		periodOf: anExpression
		endingStatementAt: aStatementSourceRange.

	self
		replaceRange: replacement intervalToReplace
		withNewSourceCode: replacement sourceCodeToWrite , period
		inMethod: aMessageNodeReference compiledMethod! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 5:04:22 pm (UTC)'!
sourceCodeOf: aCollectionOfStatements inlinedIn: aMessageNodeReference

	| replacements senderIndentation |

	self
		calculateReplacementsFrom: aMessageNodeReference
		usingRanges: aMessageNodeReference methodNode completeSourceRanges.
	self declareTemporariesIfNeededFor: aMessageNodeReference.
	replacements := self replacementsMapForMessageSend: aMessageNodeReference.
	senderIndentation := aMessageNodeReference lineIndentation.

	^aCollectionOfStatements collect: [ :aStatementToInline |
		self
			sourceCodeOfImplementorStatement: aStatementToInline
			withReplacements: replacements
			removingReturn: true
			indentedAs: senderIndentation ]! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 5:06:26 pm (UTC)'!
inlineStatements: aCollectionOfStatements ofMessageWithReturnValue: aMessageNodeReference usedInStatementAt: aStatementSourceRange

	| inlinedStatements |

	inlinedStatements := self
		sourceCodeOf: aCollectionOfStatements
		inlinedIn: aMessageNodeReference.
	self
		replace: aMessageNodeReference
		usedInStatementAt: aStatementSourceRange
		by: inlinedStatements last
		withPrecedence: aCollectionOfStatements last expr expressionPrecedence.
	self
		insert: inlinedStatements allButLast
		before: aStatementSourceRange
		in: aMessageNodeReference compiledMethod! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 28/Sep/2026, 5:06:26 pm (UTC)'!
sourceCodeOfImplementorStatement: aStatementToInline withReplacements: replacementsDict removingReturn: removeReturn indentedAs: aSenderIndentation

	| sourceRange originalSourceCode statementReplacements implementorSourceCode inlinedStatement implementorIndentation |

	implementorSourceCode := methodToInline sourceCode.
	sourceRange := (self findRangesOf: aStatementToInline in: implementorCompleteSourceRanges) anyOne
		asSourceCodeInterval expandUntilStatementEndOn: implementorSourceCode.
	originalSourceCode := implementorSourceCode copyFrom: sourceRange first to: sourceRange last.
	"Replacements must be adjusted because the statement string begins at index 1 instead of the
	original position on the source code"
	statementReplacements := ((replacementsDict associations select: [:anAssociation | sourceRange includesAllOf: anAssociation key])
		collect: [:anAssociation | ((anAssociation key first - sourceRange first + 1) to: (anAssociation key last - sourceRange first + 1)) -> anAssociation value])
		asSortedCollection: [ :left :right | left key first < right key first ].

	(removeReturn and: [aStatementToInline isReturn]) ifTrue: [
		statementReplacements add: ((1 to: 1) asSourceCodeInterval expandUntilNextNonBlankCharacterOn: originalSourceCode) -> ''].
	inlinedStatement := originalSourceCode copyReplacing: statementReplacements.
	implementorIndentation := implementorSourceCode
		indentationOfLineAt: sourceRange first.

	^implementorIndentation
		ifEmpty: [ inlinedStatement ]
		ifNotEmpty: [
			inlinedStatement
				withIndentationOfLinesAfterTheFirst: implementorIndentation
				replacedBy: aSenderIndentation ]! !

!InlineMethod methodsFor: 'inlining - private' stamp: 'HAW 28/Sep/2026, 5:04:22 pm (UTC)'!
inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReferenceWithReturnValue

	| enclosingBlock |
	
	enclosingBlock := self enclosingBlockOf: aMessageNodeReferenceWithReturnValue.
	(self parseNode: enclosingBlock returns: aMessageNodeReferenceWithReturnValue messageNode)
		ifTrue: [^self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReferenceWithReturnValue
			removingReturn: false].

	"Find the outermost statement containing the usage to inline that's inside the enclosing block"
	self findOutermostStatementContaining: aMessageNodeReferenceWithReturnValue enclosedBy: enclosingBlock
		ifFound: [ :outermostNodeAndRange |
			self inlineStatements: statementsToInline ofMessageWithReturnValue: aMessageNodeReferenceWithReturnValue
				usedInStatementAt: outermostNodeAndRange value
		]
		ifNone: [ 
			self inlineStatements: statementsToInline ofMessageWithoutAssigningPossibleReturnValue: aMessageNodeReferenceWithReturnValue
				removingReturn: true
		]! !

!methodRemoval: InlineMethod #buildInlinedSourceCodeStatementsFrom:withParametersFrom:usedInNode: stamp: 'HAW 28/Sep/2026, 5:04:22 pm (UTC)'!
InlineMethod removeSelector: #buildInlinedSourceCodeStatementsFrom:withParametersFrom:usedInNode:!
!methodRemoval: InlineMethod #periodOf:inlinedIn:endingStatementAt: stamp: 'HAW 28/Sep/2026, 5:07:19 pm (UTC)'!
InlineMethod removeSelector: #periodOf:inlinedIn:endingStatementAt:!
!methodRemoval: InlineMethod #statementWithMessageSend:usedIn:lastStatement:replacement: stamp: 'HAW 28/Sep/2026, 5:04:22 pm (UTC)'!
InlineMethod removeSelector: #statementWithMessageSend:usedIn:lastStatement:replacement:!
!methodRemoval: InlineMethod #sourceCodeOf:followedBy: stamp: 'HAW 28/Sep/2026, 5:07:19 pm (UTC)'!
InlineMethod removeSelector: #sourceCodeOf:followedBy:!
!methodRemoval: InlineMethod #statementWithMessageSend:usedIn:inlinedWith:withPrecedence: stamp: 'HAW 28/Sep/2026, 5:04:22 pm (UTC)'!
InlineMethod removeSelector: #statementWithMessageSend:usedIn:inlinedWith:withPrecedence:!
!methodRemoval: InlineMethod #inlineStatements:ofMessageWithReturnValue:usedInParseNode: stamp: 'HAW 28/Sep/2026, 5:04:22 pm (UTC)'!
InlineMethod removeSelector: #inlineStatements:ofMessageWithReturnValue:usedInParseNode:!
!methodRemoval: InlineMethod #sourceRangeOf:enclosing: stamp: 'HAW 28/Sep/2026, 5:04:22 pm (UTC)'!
InlineMethod removeSelector: #sourceRangeOf:enclosing:!

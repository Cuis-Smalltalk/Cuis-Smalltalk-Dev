'From Cuis7.9 [latest update: #8170] on 14 September 2026 at 9:16:46 am'!

!ParseNode methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 10:52:54 pm (UTC)'!
expressionPrecedence

	^self class atomicExpressionPrecedence! !


!ParseNode class methodsFor: 'precedence' stamp: 'HAW 12/Sep/2026, 10:52:54 pm (UTC)'!
atomicExpressionPrecedence

	^0! !


!AssignmentNode methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 10:52:54 pm (UTC)'!
expressionPrecedence

	^self class assignmentAndCascadePrecedence! !


!CascadeNode methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 10:52:54 pm (UTC)'!
expressionPrecedence

	^self class assignmentAndCascadePrecedence! !


!MessageNode methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 10:52:54 pm (UTC)'!
expressionPrecedence

	^precedence! !


!CharacterSequence methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 10:55:41 pm (UTC)'!
trailingPeriod

	^(self endsWith: '.')
		ifTrue: [ '.' ]
		ifFalse: [ '' ]! !

!CharacterSequence methodsFor: 'copying' stamp: 'HAW 12/Sep/2026, 10:55:41 pm (UTC)'!
withoutTrailingPeriod

	^(self endsWith: '.')
		ifTrue: [ self allButLast ]
		ifFalse: [ self ]! !


!MessageNodeReference methodsFor: 'testing' stamp: 'HAW 12/Sep/2026, 10:55:41 pm (UTC)'!
isAtTheEndOf: aSourceRange

	^self completeSourceRange last >= aSourceRange last! !

!MessageNodeReference methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 10:55:41 pm (UTC)'!
sourceCodeAt: aSourceRange

	^self compiledMethod sourceCode
		copyFrom: aSourceRange first
		to: aSourceRange last! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 12/Sep/2026, 10:55:41 pm (UTC)'!
sourceRangeOf: aParseNode enclosing: aMessageNodeReference

	^(self
		findRangesOf: aParseNode
		in: aMessageNodeReference methodNode completeSourceRanges) first! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 12/Sep/2026, 11:11:50 pm (UTC)'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode inlinedWith: anExpression withPrecedence: aPrecedence

	| enclosingNodeSourceRange replacement rangeInEnclosingNode expressionToWrite |

	enclosingNodeSourceRange := self
		sourceRangeOf: aParseNode
		enclosing: aMessageNodeReference.
	replacement := ExpressionReplacement
		of: aMessageNodeReference completeSourceRange
		in: aMessageNodeReference methodNode
		by: anExpression withoutTrailingPeriod
		withPrecedence: aPrecedence.
	rangeInEnclosingNode := replacement intervalToReplace + (1 - enclosingNodeSourceRange first).
	expressionToWrite := replacement sourceCodeToWrite.
	(aMessageNodeReference isAtTheEndOf: enclosingNodeSourceRange) ifTrue: [
		expressionToWrite := expressionToWrite , anExpression trailingPeriod ].

	^(aMessageNodeReference sourceCodeAt: enclosingNodeSourceRange)
		copyReplacing: { rangeInEnclosingNode -> expressionToWrite }! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 12/Sep/2026, 10:52:54 pm (UTC)'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode lastStatement: implementorLastStatementToInline
	replacement: lastStatementWithReplacements

	^self
		statementWithMessageSend: aMessageNodeReference
		usedIn: aParseNode
		inlinedWith: lastStatementWithReplacements
		withPrecedence: implementorLastStatementToInline expr expressionPrecedence! !

!methodRemoval: InlineMethod #messageSendIsInsideMessageNode: stamp: 'HAW 14/Sep/2026, 12:13:36 pm (UTC)'!
InlineMethod removeSelector: #messageSendIsInsideMessageNode:!
!methodRemoval: InlineMethod #statementWithMessageSend:usedIn:inlinedWith:addingParentheses: stamp: 'HAW 14/Sep/2026, 12:13:36 pm (UTC)'!
InlineMethod removeSelector: #statementWithMessageSend:usedIn:inlinedWith:addingParentheses:!
!methodRemoval: InlineMethod #addParenthesesIfNeededTo: stamp: 'HAW 14/Sep/2026, 12:13:36 pm (UTC)'!
InlineMethod removeSelector: #addParenthesesIfNeededTo:!

'From Cuis7.9 [latest update: #8170] on 12 September 2026 at 5:45:46 pm'!

!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 12/Sep/2026, 2:49:33 pm (UTC)'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode lastStatement: implementorLastStatementToInline
	replacement: lastStatementWithReplacements

	| requiresParenthesis expressionToInline |

	expressionToInline := implementorLastStatementToInline expr.
	requiresParenthesis := (expressionToInline isMessageNode
		or: [ expressionToInline isCascadeNode ])
		and: [ self messageSendIsInsideMessageNode: aMessageNodeReference ].

	^self
		statementWithMessageSend: aMessageNodeReference
		usedIn: aParseNode
		inlinedWith: lastStatementWithReplacements
		addingParentheses: requiresParenthesis! !


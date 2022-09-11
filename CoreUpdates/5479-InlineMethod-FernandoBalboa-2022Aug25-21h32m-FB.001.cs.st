'From Cuis 6.0 [latest update: #5466] on 27 August 2022 at 9:16:15 pm'!

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 8/27/2022 21:12:02'!
messageSendIsInsideMessageNode: aMessageNodeReference

	| completeSourceRanges |
	
	completeSourceRanges := aMessageNodeReference methodNode completeSourceRanges.
	aMessageNodeReference methodNode nodesDo: [:aNode | 
		(aNode isMessageNode and: 	[(self findRangesOf: aNode in: completeSourceRanges) 
			anySatisfy: [:aMessageNodeRange |
				| messageSendRange |
				messageSendRange := aMessageNodeReference completeSourceRange.
				(aMessageNodeRange includesAllOf: messageSendRange) and: [aMessageNodeRange ~= messageSendRange]]
			])
				ifTrue: [^true]
	].
	
	^false! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 8/27/2022 21:06:09'!
addParenthesesIfNeededTo: anExpression

	"TODO: duplicated code in InlineTemporaryVariable"

	| firstCharacterIsOpeningParentheses lastCharacterIsClosingParentheses |
	
 	firstCharacterIsOpeningParentheses := (anExpression at: (anExpression firstNonSeparator)) = $(.
	lastCharacterIsClosingParentheses := (anExpression at: (anExpression lastNonSeparator)) = $).

	^(firstCharacterIsOpeningParentheses and: [lastCharacterIsClosingParentheses])
		ifTrue: [^anExpression]
		ifFalse: [ (anExpression endsWith: '.')
			ifTrue: [^('(', anExpression copyUpToLast: $.), ').']
	 		ifFalse: [^'(', anExpression, ')']
		].! !

!InlineMethod methodsFor: 'source code building - private' stamp: 'FB 8/27/2022 21:14:38'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode lastStatement: implementorLastStatementToInline
	replacement: lastStatementWithReplacements

	| requiresParenthesis |

	requiresParenthesis := (implementorLastStatementToInline expr isMessageNode) 
		and:[ self messageSendIsInsideMessageNode: aMessageNodeReference].
	
	^self statementWithMessageSend: aMessageNodeReference usedIn: aParseNode
				inlinedWith: lastStatementWithReplacements addingParentheses: requiresParenthesis.
		! !


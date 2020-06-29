'From Cuis 5.0 [latest update: #4242] on 27 June 2020 at 7:13:18 pm'!

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 19:12:36'!
primaryExpression

	hereType == #word ifTrue: [ ^self variableExpression ].
	hereType == #leftBracket ifTrue: [ ^self blockExpression ].
	hereType == #leftBrace ifTrue: [ ^self braceExpression ].
	hereType == #leftParenthesis ifTrue: [ ^self parenthesisExpression ].
	(self isLiteralExpression) ifTrue: [ ^self literalExpression ].
	(self isLiteralNumberExpression) ifTrue: [ ^self literalNumberExpression ].
	hereType == #backtick ifTrue: [ ^self backtickExpression ].

	^false! !


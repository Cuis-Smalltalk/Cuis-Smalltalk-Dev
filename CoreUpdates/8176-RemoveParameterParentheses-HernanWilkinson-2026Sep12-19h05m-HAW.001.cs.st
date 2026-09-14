'From Cuis7.9 [latest update: #8170] on 14 September 2026 at 8:44:12 am'!

!Encoder methodsFor: 'source mapping' stamp: 'HAW 12/Sep/2026, 10:26:52 pm (UTC)'!
messageSendRangesOf: aSelector ifAbsent: absentBlock

	| ranges |

	ranges := sourceRanges keys
		select: [ :aParseNode | aParseNode isMessageNamed: aSelector ]
		thenCollect: [ :aMessageSendNode |
			(completeSourceRanges at: aMessageSendNode) anyOne ].

	^ranges ifEmpty: absentBlock! !


!MethodNode methodsFor: 'source mapping' stamp: 'HAW 12/Sep/2026, 10:21:21 pm (UTC)'!
messageSendRangesOf: aSelector ifAbsent: aBlock

	self completeSourceRanges.

	^encoder messageSendRangesOf: aSelector ifAbsent: aBlock! !


!ExpressionReplacement methodsFor: 'accessing' stamp: 'HAW 12/Sep/2026, 10:19:48 pm (UTC)'!
parenthesesEdits

	^self requiresParentheses
		ifTrue: [ self parenthesesEditsOpening: '(' closing: ')' ]
		ifFalse: [ self parenthesesEditsOpening: '' closing: '' ]! !

!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 10:26:52 pm (UTC)'!
closingParenthesesRange

	^interval last + 1 to: self intervalToReplace last! !

!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 10:26:52 pm (UTC)'!
openingParenthesesRange

	^self intervalToReplace first to: interval first - 1! !

!ExpressionReplacement methodsFor: 'private' stamp: 'HAW 12/Sep/2026, 10:26:52 pm (UTC)'!
parenthesesEditsOpening: anOpening closing: aClosing

	^{
		self openingParenthesesRange -> anOpening.
		self closingParenthesesRange -> aClosing }! !


!ExpressionReplacement class methodsFor: 'instance creation' stamp: 'HAW 12/Sep/2026, 10:32:23 pm (UTC)'!
of: anInterval in: aMethodNode withPrecedence: aPrecedence

	| sourceText expressionInterval expression |

	sourceText := aMethodNode sourceText.
	expressionInterval := anInterval asSourceCodeInterval
		trimToMatchExpressionOn: sourceText.
	expression := sourceText
		copyFrom: expressionInterval first
		to: expressionInterval last.

	^self
		of: expressionInterval
		in: aMethodNode
		by: expression
		withPrecedence: aPrecedence! !


!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:38:32 pm (UTC)'!
addKeywordAndParameterRangesOf: aMethodNode to: rangesToKeywords

	| senderSourceCode keywordAndParameterPositions |

	senderSourceCode := aMethodNode sourceText.
	keywordAndParameterPositions := self
		keywordAndParameterPositionsIn: aMethodNode.
	keywordAndParameterPositions do: [ :aKeywordAndParameterPosition |
		| rangeToRemove |
		rangeToRemove := self
			rangeToRemoveFor: aKeywordAndParameterPosition
			in: senderSourceCode.
		rangesToKeywords add: rangeToRemove -> senderReplacementString ]! !

!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:32:23 pm (UTC)'!
addParenthesesRangesOf: aMethodNode to: rangesToKeywords

	| sendRanges |

	newSelector isUnary ifFalse: [ ^self ].
	sendRanges := aMethodNode
		messageSendRangesOf: oldSelector
		ifAbsent: [ #() ].
	sendRanges do: [ :aSendRange |
		rangesToKeywords
			addAll: (self parenthesesRangesOf: aSendRange in: aMethodNode) ]! !

!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:19:48 pm (UTC)'!
parenthesesRangesOf: aSendRange in: aMethodNode

	| replacement |

	replacement := ExpressionReplacement
		of: aSendRange
		in: aMethodNode
		withPrecedence: newSelector precedence.

	^replacement parenthesesEdits! !


!ExpressionReplacement methodsFor: 'initialization' stamp: 'HAW 12/Sep/2026, 10:32:23 pm (UTC)'!
initializeOf: anInterval in: aMethodNode by: aSourceCode withPrecedence: aPrecedence

	methodNode := aMethodNode.
	interval := anInterval asSourceCodeInterval
		trimToMatchExpressionOn: aMethodNode sourceText.
	sourceCode := aSourceCode.
	precedence := aPrecedence! !


!ExpressionReplacement class methodsFor: 'instance creation' stamp: 'HAW 12/Sep/2026, 10:32:23 pm (UTC)'!
of: anInterval in: aMethodNode by: aSourceCode withPrecedence: aPrecedence

	^self new
		initializeOf: anInterval
		in: aMethodNode
		by: aSourceCode
		withPrecedence: aPrecedence! !


!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:26:52 pm (UTC)'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

	self
		addKeywordAndParameterRangesOf: aMethodNode to: rangesToKeywords;
		addParenthesesRangesOf: aMethodNode to: rangesToKeywords! !


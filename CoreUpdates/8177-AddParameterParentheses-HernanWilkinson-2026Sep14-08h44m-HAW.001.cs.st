'From Cuis7.9 [latest update: #8170] on 14 September 2026 at 8:54:15 am'!

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:44:08 pm (UTC)'!
addParenthesesRangesOf: aMethodNode to: rangesToKeywords

	| sendRanges |

	self changesTheSendPrecedence ifFalse: [ ^self ].
	sendRanges := aMethodNode
		messageSendRangesOf: oldSelector
		ifAbsent: [ #() ].
	sendRanges do: [ :aSendRange |
		rangesToKeywords
			addAll: (self parenthesesRangesOf: aSendRange in: aMethodNode) ]! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:44:08 pm (UTC)'!
changesTheSendPrecedence

	^oldSelector precedence ~= newSelector precedence! !

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:44:08 pm (UTC)'!
parenthesesRangesOf: aSendRange in: aMethodNode

	| replacement |

	replacement := ExpressionReplacement
		of: aSendRange
		in: aMethodNode
		withPrecedence: newSelector precedence.

	^replacement parenthesesEdits! !


!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:44:54 pm (UTC)'!
rangesToKeywordsOf: aMethod

	| methodNode rangesToKeywords |

	methodNode := aMethod notOptimizedMethodNode.
	rangesToKeywords := SortedCollection sortBlock: [ :left :right |
		left key first <= right key first ].

	self addMessageSendSelectorKeywordRangesOf: methodNode to: rangesToKeywords.
	self addParenthesesRangesOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords.

	^rangesToKeywords! !


!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 12/Sep/2026, 10:44:08 pm (UTC)'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

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

!methodRemoval: RemoveParameter #addKeywordAndParameterRangesOf:to: stamp: 'HAW 14/Sep/2026, 11:50:52 am (UTC)'!
RemoveParameter removeSelector: #addKeywordAndParameterRangesOf:to:!
!methodRemoval: RemoveParameter #addParenthesesRangesOf:to: stamp: 'HAW 14/Sep/2026, 11:50:52 am (UTC)'!
RemoveParameter removeSelector: #addParenthesesRangesOf:to:!
!methodRemoval: RemoveParameter #parenthesesRangesOf:in: stamp: 'HAW 14/Sep/2026, 11:50:52 am (UTC)'!
RemoveParameter removeSelector: #parenthesesRangesOf:in:!

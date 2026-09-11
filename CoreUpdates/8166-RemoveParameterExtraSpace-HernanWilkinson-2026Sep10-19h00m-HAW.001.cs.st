'From Cuis7.9 [latest update: #8163] on 11 September 2026 at 9:22:41 am'!

!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 10/Sep/2026, 10:20:10 pm (UTC)'!
firstSeparatorIndexIn: aSourceCode endingAt: aPosition

	| firstPosition |

	firstPosition := aPosition.
	[ firstPosition > 1 and: [ (aSourceCode at: firstPosition - 1) isSeparator ] ] whileTrue: [ firstPosition := firstPosition - 1 ].

	^firstPosition! !

!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 10/Sep/2026, 10:20:07 pm (UTC)'!
rangeToRemoveFor: aKeywordAndParameterPosition in: aSenderSourceCode

	^(isLastParameter and: [ newSelector isUnary not ])
		ifTrue: [ (self firstSeparatorIndexIn: aSenderSourceCode endingAt: aKeywordAndParameterPosition first) to: aKeywordAndParameterPosition last ]
		ifFalse: [ aKeywordAndParameterPosition first to: (self lastSeparatorIndexIn: aSenderSourceCode startingFrom: aKeywordAndParameterPosition last) ]! !


!RemoveParameter methodsFor: 'create new implementors - private' stamp: 'HAW 10/Sep/2026, 10:20:00 pm (UTC)'!
writeBeforeKeywordIn: newSourceStream from: originalSource removing: selectorToRemovePosition

	| beforeKeywordPosition |

	newSelector isUnary ifTrue: [ ^newSourceStream nextPutAll: newSelector ].

	beforeKeywordPosition := isLastParameter
		ifTrue: [ (self firstSeparatorIndexIn: originalSource endingAt: selectorToRemovePosition first) - 1 ]
		ifFalse: [ selectorToRemovePosition first - 1 ].
	newSourceStream nextPutAll: (originalSource copyFrom: 1 to: beforeKeywordPosition)! !

!RemoveParameter methodsFor: 'rename senders - private' stamp: 'HAW 10/Sep/2026, 10:20:02 pm (UTC)'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords
	
	| keywordAndParameterPositions senderSourceCode |
	
	senderSourceCode := aMethodNode sourceText.
	keywordAndParameterPositions := self keywordAndParameterPositionsIn: aMethodNode.
	keywordAndParameterPositions do: [ :aKeywordAndParameterPosition |
		rangesToKeywords add: (self rangeToRemoveFor: aKeywordAndParameterPosition in: senderSourceCode) -> senderReplacementString ]! !


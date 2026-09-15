'From Cuis7.9 [latest update: #8181] on 14 September 2026 at 1:34:34 pm'!

!Interval methodsFor: '*Tools-Refactoring' stamp: 'HAW 14/Sep/2026, 4:23:59 pm (UTC)'!
strictlyIncludesRange: anInterval

	^(self includes: anInterval first)
		and: [ anInterval last <= self last ]
		and: [ self ~= anInterval ]! !


!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 14/Sep/2026, 6:10:01 pm (UTC)'!
rangesToKeywordsOf: aMethod

	| methodNode rangesToKeywords |

	methodNode := aMethod notOptimizedMethodNode.
	rangesToKeywords := SortedCollection sortBlock: [ :left :right |
		left key first <= right key first ].

	self addMessageSendSelectorKeywordRangesOf: methodNode to: rangesToKeywords.
	self addParenthesesRangesOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralOf: methodNode to: rangesToKeywords.
	self addRangesForLiteralInLiteralArrayOf: methodNode to: rangesToKeywords.

	^self withoutTheRangesToNewStringsInsideAnotherOf: rangesToKeywords! !


!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 14/Sep/2026, 6:10:01 pm (UTC)'!
withoutTheRangesToNewStringsInsideAnotherOf: rangesToNewStrings

	^rangesToNewStrings reject: [ :aRangeToNewString |
		rangesToNewStrings anySatisfy: [ :anotherRangeToNewString |
			anotherRangeToNewString key strictlyIncludesRange: aRangeToNewString key ] ]! !

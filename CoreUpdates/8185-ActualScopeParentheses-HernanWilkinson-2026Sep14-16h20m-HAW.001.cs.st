'From Cuis7.9 [latest update: #8181] on 14 September 2026 at 6:12:45 pm'!

!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 14/Sep/2026, 9:06:31 pm (UTC)'!
sendRangesToReparenthesizeIn: aMethodNode

	"Redefine by subclasses in LiveTyping - do not remove - Hernan"
	^aMethodNode messageSendRangesOf: oldSelector ifAbsent: [ #() ]! !


!ChangeSelector methodsFor: 'rename senders - private' stamp: 'HAW 14/Sep/2026, 9:06:31 pm (UTC)'!
addParenthesesRangesOf: aMethodNode to: rangesToKeywords

	self changesTheSendPrecedence ifFalse: [ ^self ].
	(self sendRangesToReparenthesizeIn: aMethodNode) do: [ :aSendRange |
		rangesToKeywords
			addAll: (self parenthesesRangesOf: aSendRange in: aMethodNode) ]! !


'From Cuis7.7 [latest update: #7821] on 29 January 2026 at 11:22:52 am'!

!Theme class methodsFor: 'user interface' stamp: 'ER 29/Jan/2026 11:18:49'!
largerDefaultFont
	| current larger |
	current := Preferences at: #defaultFontSize.
	larger := current < 20 ifTrue: [current +1] ifFalse: [(current * 1.08) rounded].
	larger := larger min: 96*0+121.
	self setDefaultFontSize: larger print! !

!Theme class methodsFor: 'user interface' stamp: 'ER 29/Jan/2026 11:16:22'!
smallerDefaultFont
	| current smaller |
	current := Preferences at: #defaultFontSize.
	smaller := current <= 20 ifTrue: [current -1] ifFalse: [(current / 1.08) rounded].
	smaller := smaller max: 4.
	self setDefaultFontSize: smaller print! !


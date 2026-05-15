'From Cuis7.7 [latest update: #7940] on 15 May 2026 at 1:20:46 pm'!

!ArrayedCollection methodsFor: 'math functions' stamp: 'fg 15/May/2026 13:20:16'!
incrementFractions
	"
	#(10 12.5 15 20) incrementFractions
	"
	| displaced answer |
	displaced := self class new: self size.
	displaced replaceFrom: 2 to: self size with: self startingAt: 1.
	displaced at: 1 put: self first.
	answer := self copy.
	answer -= displaced.
	^answer / displaced! !

!methodRemoval: ArrayedCollection #incrementFraction stamp: 'fg 15/May/2026 13:20:19'!
ArrayedCollection removeSelector: #incrementFraction!

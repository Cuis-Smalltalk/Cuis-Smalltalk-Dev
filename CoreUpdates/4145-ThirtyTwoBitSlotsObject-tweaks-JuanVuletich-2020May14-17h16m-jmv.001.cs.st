'From Cuis 5.0 [latest update: #4140] on 14 May 2020 at 5:49:46 pm'!

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:49:24'!
intSlotAt: index
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word _ self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/14/2020 17:49:30'!
intSlotAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word _ 16r100000000 + anInteger"
				word _ (anInteger + 1) negated bitInvert32]
		ifFalse:[word _ anInteger].
	self  basicAt: index put: word.
	^anInteger! !

!methodRemoval: ThirtyTwoBitSlotsObject #integerSlotAt: stamp: 'jmv 5/14/2020 17:49:36'!
ThirtyTwoBitSlotsObject removeSelector: #integerSlotAt:!
!methodRemoval: ThirtyTwoBitSlotsObject #integerSlotAt:put: stamp: 'jmv 5/14/2020 17:49:39'!
ThirtyTwoBitSlotsObject removeSelector: #integerSlotAt:put:!

'From Cuis 6.0 [latest update: #5602] on 20 December 2022 at 2:26:01 pm'!

!String methodsFor: 'copying' stamp: 'jmv 12/20/2022 14:25:31'!
copyWith: newElement
	newElement isUnicodeCodePoint ifTrue: [
		^self asUnicodeString copyWith: newElement ].
	^super copyWith: newElement! !


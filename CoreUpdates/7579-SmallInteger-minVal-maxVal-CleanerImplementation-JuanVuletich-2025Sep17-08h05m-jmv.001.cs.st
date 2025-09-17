'From Cuis7.5 [latest update: #7578] on 17 September 2025 at 8:09:11 am'!

!SmallInteger class methodsFor: 'constants' stamp: 'jmv 9/17/2025 08:08:33'!
maxVal
	"Answer the maximum value for a SmallInteger."
	maxVal ifNil: [
		"Don't create any LargeInteger!!"
		maxVal := -1 - self minVal ].
	^maxVal.! !

!SmallInteger class methodsFor: 'constants' stamp: 'jmv 9/17/2025 08:08:30'!
minVal
	"Answer the minimum value for a SmallInteger."
	minVal ifNil: [
		"Don't create any LargeInteger!!"
		minVal := Smalltalk wordSize = 8
			ifTrue: [((1<<59) negated <<1)]
			ifFalse: [((1<<29) negated <<1)]].
	^minVal.! !

!methodRemoval: SmallInteger class #initMinValAndMaxVal stamp: 'jmv 9/17/2025 08:06:20'!
SmallInteger class removeSelector: #initMinValAndMaxVal!

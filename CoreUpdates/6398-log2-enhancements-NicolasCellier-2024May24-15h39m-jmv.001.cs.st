'From Cuis7.1 [latest update: #6395] on 24 May 2024 at 3:43:08 pm'!

!LargePositiveInteger methodsFor: 'mathematical functions' stamp: 'nice 2/8/2019 18:26'!
log2
	"This function is defined because super log2 might overflow."
	| res h |
	res := super log2.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^h + (self / (1 << h)) asFloat log2! !


!LargeNegativeInteger methodsFor: 'mathematical functions' stamp: 'nice 2/8/2019 18:24'!
log2
	^DomainError signal: 'log2 is only defined for x > 0'! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 5/24/2024 11:48:32'!
log2
	"Answer the base-2 logarithm of the receiver.
	See comment at #log:"

	self <= 0 ifTrue: [DomainError signal: 'log2 is only defined for x > 0'].
	^self asFloat log2! !


!Float methodsFor: 'mathematical functions' stamp: 'nice 3/4/2021 21:34'!
log2
	"Answer the base 2 logarithm of the receiver.
	Arrange to answer exact result in case of exact power of 2."

	|  s  |
	 s := self significand.
	^s > 1.3333333333333333
		ifTrue: [(0.5 * s) ln / Ln2 + (1 + self exponent)]
		ifFalse: [s ln / Ln2 + self exponent]! !


!Fraction methodsFor: 'mathematical functions' stamp: 'nice 2/5/2019 13:58'!
log2
	"This function is defined because super log2 might overflow."
	| res |
	self <= 0 ifTrue: [DomainError signal: 'log2 is only defined for x > 0'].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow.
	Also, give an integer answer if possible."
	numerator < denominator ifTrue: [^self reciprocal log2 negated].
	res := super log2.
	res isFinite ifTrue: [^res].
	^numerator log2 - denominator log2! !


'From Cuis7.1 [latest update: #6395] on 24 May 2024 at 3:39:16 pm'!

!LargePositiveInteger methodsFor: 'mathematical functions' stamp: 'nice 7/15/2011 14:06'!
ln
	"This function is defined because super ln might overflow."
	| res h |
	res := super ln.
	res isFinite ifTrue: [^res].
	h := self highBit.
	^2 ln * h + (self / (1 << h)) asFloat ln! !


!LargeNegativeInteger methodsFor: 'mathematical functions' stamp: 'nice 9/22/2013 00:59'!
ln
	^DomainError signal: 'ln is only defined for x > 0'! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 5/24/2024 12:09:43'!
ln
	"Answer the natural log of the receiver.
	See comment at #log:"

	self <= 0 ifTrue: [DomainError signal: 'ln is only defined for x > 0'].
	^self asFloat ln! !


!Fraction methodsFor: 'mathematical functions' stamp: 'nice 11/1/2011 18:09'!
ln
	"This function is defined because super ln might overflow."
	| res |
	self <= 0 ifTrue: [DomainError signal: 'ln is only defined for x > 0'].
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow."
	numerator < denominator ifTrue: [^self reciprocal ln negated].
	res := super ln.
	res isFinite ifTrue: [^res].
	^numerator ln - denominator ln! !

!methodRemoval: Integer #ln stamp: 'jmv 5/24/2024 15:38:38'!
Integer removeSelector: #ln!

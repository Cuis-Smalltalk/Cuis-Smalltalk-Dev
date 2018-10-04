'From Cuis 5.0 of 7 November 2016 [latest update: #3468] on 3 October 2018 at 5:52:09 pm'!

!Fraction methodsFor: 'mathematical functions' stamp: 'nice 4/25/2018 09:43'!
floorLog: radix
	"Unlike super, this version is exact when radix is integer"
	
	| d n |
	radix isInteger ifFalse: [^super floorLog: radix].
	n := numerator floorLog: radix.
	d := denominator floorLog: radix.
	^(numerator * (radix raisedTo: d))
		< (denominator * (radix raisedTo: n))
		ifTrue: [n - d - 1]
		ifFalse: [n - d]! !


!Integer methodsFor: 'mathematical functions' stamp: 'nice 4/25/2018 09:43'!
floorLog: radix
	"Unlike super, this version is exact when radix is integer"
	
	radix isInteger ifFalse: [^super floorLog: radix].
	self <= 0 ifTrue: [^DomainError signal: 'floorLog: is only defined for x > 0.0'].
	^(self numberOfDigitsInBase: radix) - 1! !


!Number methodsFor: 'mathematical functions' stamp: 'nice 12/11/2012 19:18'!
floorLog: radix
	"Answer the floor of the log base radix of the receiver."

	^(self log: radix) floor! !


!Float methodsFor: 'mathematical functions' stamp: 'nice 6/3/2012 17:26'!
floorLog: radix
	"Answer the floor of the log base radix of the receiver.
	The result may be off by one due to rounding errors, except in base 2."

	(radix = 2 and: [self > 0.0 and: [self isFinite]]) ifTrue: [^self exponent].
	^ (self log: radix) floor
! !


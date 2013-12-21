'From Cuis 4.2 of 25 July 2013 [latest update: #1867] on 14 December 2013 at 4:07:39.563835 pm'!

!Integer methodsFor: 'testing' stamp: 'sqr 12/14/2013 16:07'!
nextPrime

	| pivot |
	self < 2 ifTrue: [^2].
	pivot := self + 1 bitOr: 1.
	[pivot isPrime] whileFalse: [pivot := pivot + 2].
	^pivot! !


'From Cuis 5.0 of 7 November 2016 [latest update: #3467] on 3 October 2018 at 5:18:30 pm'!

!Float methodsFor: 'arithmetic' stamp: 'nice 12/20/2012 23:16'!
negated
	"Answer a Number that is the negation of the receiver.
	Implementation note: this version cares of negativeZero."

	^-1.0 * self! !


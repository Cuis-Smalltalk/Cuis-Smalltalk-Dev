'From Cuis 5.0 of 7 November 2016 [latest update: #3517] on 11 December 2018 at 1:42:08 pm'!

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/11/2018 13:10:34'!
ulp
	"Answer the unit of least precision of the receiver.
	Follow John Harrison's definition as described at
	https://en.wikipedia.org/wiki/Unit_in_the_last_place"
	
	self isFinite ifFalse: [^self abs].
	self isZero ifTrue: [^0.0 nextAwayFromZero].
	^ (self - self nextTowardsZero) abs! !


!Float class methodsFor: 'constants' stamp: 'jmv 12/11/2018 13:21:21'!
epsilon
	"Answer difference between 1.0 and next representable value.
	Note: does not equal 1.0 ulp."
	
	^1.0 successor - 1.0! !


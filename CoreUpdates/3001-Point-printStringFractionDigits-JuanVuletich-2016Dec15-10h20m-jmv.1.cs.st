'From Cuis 5.0 of 7 November 2016 [latest update: #3000] on 15 December 2016 at 12:11:15 pm'!

!Point methodsFor: 'printing' stamp: 'jmv 12/15/2016 10:20:58'!
printStringFractionDigits: placesDesired
	^(x printStringFractionDigits: placesDesired), '@', (y printStringFractionDigits: placesDesired)! !


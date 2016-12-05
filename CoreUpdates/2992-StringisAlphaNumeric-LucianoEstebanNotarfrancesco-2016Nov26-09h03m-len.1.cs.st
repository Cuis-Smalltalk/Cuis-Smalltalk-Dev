'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 26 November 2016 at 9:06:05 am'!

!String methodsFor: 'testing' stamp: 'len 11/26/2016 09:05:35'!
isAlphaNumeric
	"Answer true if the receiver contains only letters or digits."
	^ self allSatisfy: [:each| each isAlphaNumeric]! !


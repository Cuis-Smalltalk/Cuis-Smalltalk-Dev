'From Cuis 4.2 of 25 July 2013 [latest update: #2076] on 8 August 2014 at 10:34:14.8925 am'!

!Complex methodsFor: 'accessing' stamp: 'jmv 8/7/2014 10:35'!
argument
	"Answer the argument of the receiver."

	self isZero ifTrue: [ ^0.0 ].
	^imaginary arcTan: real! !

!Complex methodsFor: 'accessing' stamp: 'jmv 8/7/2014 10:34'!
magnitude
	"Answer the distance of the receiver from zero (0 + 0 i)."

	^ ((real * real) + (imaginary * imaginary)) sqrt! !

!Complex methodsFor: 'accessing' stamp: 'jmv 8/7/2014 10:35'!
phase
	"Answer the argument of the receiver."

	self isZero ifTrue: [ ^0.0 ].
	^imaginary arcTan: real! !


'From Cuis 5.0 of 7 November 2016 [latest update: #3353] on 25 June 2018 at 12:07:56 pm'!

!Number methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2018 12:03:06'!
argument
	"Answer the argument of the receiver (see Complex | argument).
	Answer is in (-Pi .. +Pi]
	Note: Assume the convention of 0+0i having argument=0"

	^self < 0
		ifFalse: [0]
		ifTrue: [Float pi]! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 6/25/2018 12:03:18'!
phase
	"Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]
	Note: Assume the convention of 0+0i having argument=0"

	^ self argument! !


!Number methodsFor: 'arithmetic' stamp: 'jmv 6/25/2018 11:58:51'!
arg
	"Answer the argument of the receiver (see Complex | arg).
	Note: #argument and #phase assume the convention of 0+0i having argument=0"
	
	self isZero ifTrue: [self error: 'Zero (0 + 0 i) does not have an argument.'].
	^self < 0
		ifFalse: [0]
		ifTrue: [Float pi]! !


!Complex methodsFor: 'accessing' stamp: 'jmv 6/25/2018 12:01:46'!
argument
	"Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]"

	self isZero ifTrue: [ ^0.0 ].
	^imaginary arcTan: real! !

!Complex methodsFor: 'accessing' stamp: 'jmv 6/25/2018 11:52:36'!
magnitude
	"Answer the distance of the receiver from zero (0 + 0 i)."

	^ self abs! !

!Complex methodsFor: 'accessing' stamp: 'jmv 6/25/2018 12:03:15'!
phase
	"Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]
	Note: Assume the convention of 0+0i having argument=0"

	^ self argument! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 6/25/2018 12:02:03'!
arg
	"Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]
	Note: #argument and #phase assume the convention of 0+0i having argument=0"

	self isZero ifTrue: [self error: 'zero has no argument.'].
	^imaginary arcTan: real! !


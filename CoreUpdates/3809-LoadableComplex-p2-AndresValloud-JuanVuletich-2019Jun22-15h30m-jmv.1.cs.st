'From Cuis 5.0 of 7 November 2016 [latest update: #3808] on 22 June 2019 at 3:36:06 pm'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:34:46'!
sqrtNonPrimitive
	"Answer the square root of the receiver. "
	| exp guess delta |

	self = 0.0 ifTrue: [
		^self ].	"Answer 0.0 for 0.0, but -0.0 for -0.0. See IEEE 754 standard"

	self <= 0.0
		ifTrue: [
			^NegativePowerError new signalBase: self selector: #raisedTo: argument: 1/2 ].

	"NaN and Infinity"
	self isFinite ifFalse: [
		^ self ].

	"Newton-Raphson"
	"first guess is half the exponent"
	exp _ self exponent // 2.
	guess _ self timesTwoPower: 0 - exp.
	[
		delta _ self - (guess * guess) / (guess * 2.0).
		delta abs >= guess ulp ]
	whileTrue: [
		guess _ guess + delta ].
	^ guess! !


!LargeNegativeInteger methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:32:24'!
sqrt
	"Answer the square root of the receiver."
	^NegativePowerError new signalBase: self selector: #raisedTo: argument: 1/2! !


!SmallInteger methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:32:02'!
sqrt

	self positive ifTrue: [^super sqrt].
	^NegativePowerError new signalBase: self selector: #raisedTo: argument: 1/2! !


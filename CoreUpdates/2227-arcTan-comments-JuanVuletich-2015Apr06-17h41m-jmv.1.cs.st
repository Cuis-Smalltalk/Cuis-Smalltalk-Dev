'From Cuis 4.2 of 25 July 2013 [latest update: #2226] on 6 April 2015 at 5:44:28.304086 pm'!

!Complex methodsFor: 'accessing' stamp: 'jmv 4/6/2015 16:55'!
argument
	"Answer the argument of the receiver.
	Answer is between -Pi and +Pi"

	self isZero ifTrue: [ ^0.0 ].
	^imaginary arcTan: real! !

!Complex methodsFor: 'accessing' stamp: 'jmv 4/6/2015 16:54'!
phase
	"Answer the argument of the receiver.
	Answer is between -Pi and +Pi"

	self isZero ifTrue: [ ^0.0 ].
	^imaginary arcTan: real! !

!Complex methodsFor: 'arithmetic' stamp: 'nice 4/20/2011 22:18'!
arg
	"Answer the argument of the receiver.
	Answer is between -Pi and +Pi"

	self isZero ifTrue: [self error: 'zero has no argument.'].
	^imaginary arcTan: real! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 4/6/2015 16:05'!
arcTan
	"The receiver is the tangent of an angle. Answer the angle measured in  radians.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Comples>>#argument"

	^self asFloat arcTan! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 4/6/2015 16:04'!
arcTan
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Also see comment at Number>>#arcTan"

	| theta eps step sinTheta cosTheta |
	<primitive: 57>

	"Newton-Raphson"
	self < 0.0 ifTrue: [ ^ 0.0 - (0.0 - self) arcTan ].

	"first guess"
	theta _ (self * Halfpi) / (self + 1.0).

	"iterate"
	eps _ Halfpi * Epsilon.
	step _ theta.
	[(step * step) > eps] whileTrue: [
		sinTheta _ theta sin.
		cosTheta _ theta cos.
		step _ (sinTheta * cosTheta) - (self * cosTheta * cosTheta).
		theta _ theta - step].
	^ theta! !


!Point methodsFor: 'polar coordinates' stamp: 'jmv 4/6/2015 16:53'!
theta
	"Answer the angle the receiver makes with origin in radians.
	Answer is between Zero and TwoPi
	If coordinates are in Display convention (y increases downwards), angles increase clockwise.
	If coordinates are in math convention (y increases upwards), angles increase counter-clockwise 
	"

	| tan theta |
	^x = 0
		ifTrue: [
			y >= 0
				ifTrue: [ 1.570796326794897 	"90.0 degreesToRadians"]
				ifFalse: [ 4.71238898038469	"270.0 degreesToRadians"]]
		ifFalse: [
			tan _ y asFloat / x asFloat.
			theta _ tan arcTan.
			x >= 0
				ifTrue: [
					y >= 0
						ifTrue: [ theta ]
						ifFalse: [
							6.283185307179586 "360.0 degreesToRadians"
							+ theta ]]
				ifFalse: [
					3.141592653589793 "180.0 degreesToRadians"
					+ theta ]]! !


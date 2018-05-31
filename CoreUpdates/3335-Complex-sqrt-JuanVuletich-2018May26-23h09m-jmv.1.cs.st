'From Cuis 5.0 of 7 November 2016 [latest update: #3334] on 26 May 2018 at 11:10:23 pm'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 17:51:33'!
sqrt
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive."
	| exp guess eps delta prim |
	prim _ self primSqrt.
	prim isNaN ifFalse: [ ^prim ].

	"Newton-Raphson"
	self <= 0.0
		ifTrue: [
			^self = 0.0
				ifTrue: [0.0]
				ifFalse: [
					(0.0 - self) sqrt i ]].
	"first guess is half the exponent"
	exp := self exponent // 2.
	guess := self timesTwoPower: 0 - exp.
	"get eps value"
	eps := guess * Epsilon.
	eps := eps * eps.
	delta := self - (guess * guess) / (guess * 2.0).
	[delta * delta > eps]
		whileTrue: 
			[guess := guess + delta.
			delta := self - (guess * guess) / (guess * 2.0)].
	^ guess! !


!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 5/21/2018 18:02:08'!
primSqrt
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 55>
	^Float nan! !


!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 5/21/2018 18:02:14'!
primSqrt
	"Answer the square root of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 555>
	^Float nan! !


!Complex methodsFor: 'testing' stamp: 'jmv 5/26/2018 23:00:13'!
isNaN
	^real isNaN or: [ imaginary isNaN ]! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 23:01:31'!
sqrt
	| d n answer |
	n _ numerator sqrt.
	d _ denominator sqrt.
	"The #sqrt method in integer will only answer a Float if there's no exact square root.
	So, we need a float anyway."
	(n isInfinite or: [ d isInfinite ]) ifTrue: [
		^self asFloat sqrt ].
	answer _ n / d.
	answer isNaN ifTrue: [
		^self asFloat sqrt ].
	^ answer! !


!LargeNegativeInteger methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 17:52:29'!
sqrt
	"Answer the square root of the receiver."
	^ self negated sqrt i! !


!SmallInteger methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 17:52:59'!
sqrt
	self negative ifTrue: [
		^ self negated sqrt i ].
	^ super sqrt! !

!methodRemoval: SmallFloat64 #sqrt!
SmallFloat64 removeSelector: #sqrt!
!methodRemoval: BoxedFloat64 #sqrt!
BoxedFloat64 removeSelector: #sqrt!

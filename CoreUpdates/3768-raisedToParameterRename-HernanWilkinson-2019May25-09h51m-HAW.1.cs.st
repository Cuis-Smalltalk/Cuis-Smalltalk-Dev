'From Cuis 5.0 of 7 November 2016 [latest update: #3766] on 25 May 2019 at 10:26:14 am'!

!Number methodsFor: 'mathematical functions' stamp: 'HAW 5/25/2019 10:22:42'!
raisedToNegativeInteger: negativeExponent
	
	| firstTry positiveExponent exponent1 exponent2 |

	positiveExponent := negativeExponent negated.
	firstTry := self raisedToInteger: positiveExponent.
	^firstTry isInfinite
		ifFalse: [firstTry reciprocal]
		ifTrue: [
			exponent1 _ positiveExponent // 2.
			exponent2 _ positiveExponent - exponent1.
			(self raisedToInteger: exponent1) reciprocal * (self raisedToInteger: exponent2) reciprocal ]! !


!Number methodsFor: 'mathematical functions' stamp: 'HAW 5/25/2019 10:24:39'!
raisedTo: exponent 
	"Answer the receiver raised to aNumber."

	exponent isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: exponent].
	exponent isFraction ifTrue: [
		"Special case for fraction power by Nicolas Cellier:
		If aNumber is a fraction, but result must be a Float, learn it as quickly as possible, and give quick Float answer
		Allows evaluating:
		(2009/2000) raisedTo: (3958333/100000)
		"
		^ self raisedToFraction: exponent].
	self < 0 ifTrue: [
		^(Complex basicReal: self imaginary: 0) raisedTo: exponent ].
	0 = exponent ifTrue: [^ self class one].	"Special case of exponent=0"
	1 = exponent ifTrue: [^ self].	"Special case of exponent=1"
	0 = self ifTrue: [				"Special case of self = 0"
		exponent < 0
			ifTrue: [^ (ZeroDivide dividend: self) signal]
			ifFalse: [^ self]].
	^ (exponent * self ln) exp		"Otherwise use logarithms"! !

!Number methodsFor: 'mathematical functions' stamp: 'HAW 5/25/2019 10:24:15'!
raisedToFraction: exponent
	self isZero ifTrue: [
		exponent negative ifTrue: [
			^ (ZeroDivide dividend: 1) signal ].
		^ self ].
	self negative ifFalse: [
		^ (self ln * exponent) exp ].
	exponent denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: exponent numerator ].
	(self negative and: [ exponent denominator even ]) ifTrue: [
		^ (Complex basicReal: self imaginary: 0) raisedToFraction: exponent].
	^ (self negated ln * exponent) exp negated! !

!Number methodsFor: 'mathematical functions' stamp: 'HAW 5/25/2019 10:23:49'!
raisedToInteger: exponent

	"The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must
	be handled as an indeterminate form.
	I take the first context because that's the way that was previously handled.
	Maybe further discussion is required on this topic."
	
	| bitProbe result |

	exponent negative ifTrue: [^self raisedToNegativeInteger: exponent ].

	bitProbe := 1 bitShift: exponent highBit - 1.
 	result := self class one.
 
 	[(exponent bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
       bitProbe := bitProbe bitShift: -1.
	bitProbe > 0 ] whileTrue: [result := result * result].
	
	^result! !


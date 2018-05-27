'From Cuis 5.0 of 7 November 2016 [latest update: #3336] on 26 May 2018 at 10:48:43 pm'!

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:20:24'!
raisedToFraction: aFraction
	^ (self nthRoot: aFraction denominator) raisedToInteger: aFraction numerator! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:28:10'!
raisedTo: aNumber 
	"Answer the receiver raised to aNumber."

	aNumber isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: aNumber].
	aNumber isFraction ifTrue: [
		"Special case for fraction power by Nicolas Cellier:
		If aNumber is a fraction, but result must be a Float, learn it as quickly as possible, and give quick Float answer
		Allows evaluating:
		(2009/2000) raisedTo: (3958333/100000)
		"
		^ self raisedToFraction: aNumber].
	self < 0 ifTrue: [
		^(Complex basicReal: self imaginary: 0) raisedTo: aNumber ].
	0 = aNumber ifTrue: [^ self class one].	"Special case of exponent=0"
	1 = aNumber ifTrue: [^ self].	"Special case of exponent=1"
	0 = self ifTrue: [				"Special case of self = 0"
		aNumber < 0
			ifTrue: [^ (ZeroDivide dividend: self) signal]
			ifFalse: [^ self]].
	^ (aNumber * self ln) exp		"Otherwise use logarithms"! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:22:44'!
raisedToFraction: aFraction
	self isZero ifTrue: [
		aFraction negative ifTrue: [
			^ (ZeroDivide dividend: 1) signal ].
		^ self ].
	self negative ifFalse: [
		^ (self ln * aFraction) exp ].
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^ (Complex basicReal: self imaginary: 0) raisedToFraction: aFraction].
	^ (self negated ln * aFraction) exp negated! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:21:48'!
raisedToFraction: aFraction
	| root |
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^ (Complex basicReal: self imaginary: 0) raisedToFraction: aFraction].
	root _ (self numerator nthRootTruncated: aFraction denominator) /
			(self denominator nthRootTruncated: aFraction denominator).
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:19:10'!
raisedToFraction: aFraction
	| root |
	self = 0 ifTrue: [ ^0 ].
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^ (Complex basicReal: self imaginary: 0) raisedToFraction: aFraction].
	root _ self nthRootTruncated: aFraction denominator.
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !


!Complex methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:24:56'!
raisedTo: aNumber 
	"Answer the receiver raised to aNumber."

	aNumber isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: aNumber].
	aNumber isFraction ifTrue: [
		^ self raisedToFraction: aNumber].
	
	0 = aNumber ifTrue: [^ self class one].	"Special case of exponent=0"
	1 = aNumber ifTrue: [^ self].	"Special case of exponent=1"
	0 = self ifTrue: [				"Special case of self = 0"
		aNumber < 0
			ifTrue: [^ (ZeroDivide dividend: self) signal]
			ifFalse: [^ self]].
	^ (aNumber * self ln) exp		"Otherwise use logarithms"! !


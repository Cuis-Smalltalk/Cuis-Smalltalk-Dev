'From Cuis 5.0 of 7 November 2016 [latest update: #3140] on 17 August 2017 at 9:45:47 pm'!

!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/17/2017 21:37:11'!
raisedToFraction: aFraction
	self isZero ifTrue: [
		aFraction negative ifTrue: [
			^ (ZeroDivide dividend: 1) signal ].
		^ self ].
	self negative ifFalse: [
		^ (self ln * aFraction) exp ].
	aFraction denominator even ifTrue: [
		^ ArithmeticError signal: 'nth root only defined for positive Integer n.' ].
	^ (self negated ln * aFraction) exp negated! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 8/17/2017 21:37:35'!
raisedToFraction: aFraction
	| root |
	root _ (self numerator nthRootTruncated: aFraction denominator) /
			(self denominator nthRootTruncated: aFraction denominator).
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/17/2017 21:37:42'!
raisedToFraction: aFraction
	| root |
	root _ self nthRootTruncated: aFraction denominator.
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 8/17/2017 21:44:25'!
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
		^ ArithmeticError signal: 'Negative numbers can''t be raised to float powers.' ].
	0 = aNumber ifTrue: [^ self class one].	"Special case of exponent=0"
	1 = aNumber ifTrue: [^ self].	"Special case of exponent=1"
	0 = self ifTrue: [				"Special case of self = 0"
		aNumber < 0
			ifTrue: [^ (ZeroDivide dividend: self) signal]
			ifFalse: [^ self]].
	^ (aNumber * self ln) exp		"Otherwise use logarithms"! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 8/17/2017 21:38:55'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	| guess |
	guess _ (numerator nthRootTruncated: aPositiveInteger) /
				(denominator nthRootTruncated: aPositiveInteger).
	(guess raisedTo: aPositiveInteger) = self ifTrue: [
		^ guess ].
	"There is no exact nth root, so answer a Float approximation"
	^ (self abs ln / aPositiveInteger) exp * self sign! !


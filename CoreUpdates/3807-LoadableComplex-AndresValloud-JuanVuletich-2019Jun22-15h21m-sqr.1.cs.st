'From Cuis 5.0 of 7 November 2016 [latest update: #3805] on 22 June 2019 at 3:26:49 pm'!
!classDefinition: #NegativePowerError category: #'Exceptions Kernel'!
ArithmeticError subclass: #NegativePowerError
	instanceVariableNames: 'base argument selector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!NegativePowerError methodsFor: 'initialization' stamp: 'jmv 6/22/2019 15:00:02'!
base: aNumber selector: aSymbol argument: otherNumber
	base _ aNumber.
	selector _ aSymbol.
	argument _ otherNumber ! !

!NegativePowerError methodsFor: 'initialization' stamp: 'jmv 6/22/2019 15:01:03'!
signalBase: aNumber selector: aSymbol argument: otherNumber
	^self
		base: aNumber selector: aSymbol argument: otherNumber;
		signal! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:01:22'!
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
		^NegativePowerError new signalBase: self selector: #raisedTo: argument: exponent].
	0 = exponent ifTrue: [^ self class one].	"Special case of exponent=0"
	1 = exponent ifTrue: [^ self].	"Special case of exponent=1"
	0 = self ifTrue: [				"Special case of self = 0"
		exponent < 0
			ifTrue: [^ (ZeroDivide dividend: self) signal]
			ifFalse: [^ self]].
	^ (exponent * self ln) exp		"Otherwise use logarithms"! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:04:09'!
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
		^NegativePowerError new signalBase: self selector: #raisedToFraction: argument: aFraction].
	^ (self negated ln * aFraction) exp negated! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:05:38'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	aPositiveInteger = 2 ifTrue: [
		^self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].
	
	^self negative
		ifTrue: [
			aPositiveInteger even
				ifTrue: [ NegativePowerError new signalBase: self selector: #nthRoot: argument: aPositiveInteger ]
				ifFalse: [ (self negated nthRoot: aPositiveInteger) negated ]]
		ifFalse: [ self raisedTo: 1.0 / aPositiveInteger ]! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:06:05'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	| guess |
	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].

	(self negative and: [ aPositiveInteger even ]) ifTrue: [
		^ NegativePowerError new signalBase: self selector: #nthRoot: argument: aPositiveInteger  ].
	
	guess _ (numerator nthRootTruncated: aPositiveInteger) /
				(denominator nthRootTruncated: aPositiveInteger).
	(guess raisedTo: aPositiveInteger) = self ifTrue: [
		^ guess ].
	"There is no exact nth root, so answer a Float approximation"
	^ (self abs ln / aPositiveInteger) exp * self sign! !

!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:03:56'!
raisedToFraction: aFraction
	| root |
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalBase: self selector: #raisedToFraction: argument: aFraction].
	root _ (self numerator nthRootTruncated: aFraction denominator) /
			(self denominator nthRootTruncated: aFraction denominator).
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:06:14'!
nthRoot: aPositiveInteger
          "Answer the nth root of the receiver.
          Answer an Integer if root is exactly this Integer, else answer the Float nearest the exact root."
  
	| guess excess scaled nBits |
	self = 0 ifTrue: [ ^0 ].

	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].

	(self negative and: [ aPositiveInteger even ]) ifTrue: [
		^  NegativePowerError new signalBase: self selector: #nthRoot: argument: aPositiveInteger  ].

	guess _ self nthRootRounded: aPositiveInteger.
	excess _ (guess raisedTo: aPositiveInteger) - self.
	excess = 0 ifTrue: [ ^ guess ].

	nBits _ Float precision - guess highBitOfMagnitude.
	nBits <= 0 ifTrue: [ ^(Fraction numerator: guess * 4 - excess sign denominator: 4) asFloat].

	scaled _ self << (nBits * aPositiveInteger).
	guess _ scaled nthRootRounded: aPositiveInteger.
	excess _ (guess raisedTo: aPositiveInteger) - scaled.
	^(Fraction numerator: guess * 4 - excess sign denominator: 1 << (nBits + 2)) asFloat! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 6/22/2019 15:04:35'!
raisedToFraction: aFraction
	| root |
	self = 0 ifTrue: [ ^0 ].
	aFraction denominator = 2 ifTrue: [
		^ self sqrt raisedToInteger: aFraction numerator ].
	(self negative and: [ aFraction denominator even ]) ifTrue: [
		^NegativePowerError new signalBase: self selector: #raisedToFraction: argument: aFraction].
	root _ self nthRootTruncated: aFraction denominator.
	(root raisedToInteger: aFraction denominator) = self ifTrue: [
		^ root raisedToInteger: aFraction numerator ].
	^ super raisedToFraction: aFraction! !

!methodRemoval: Number #asComplex!
Number removeSelector: #asComplex!
!methodRemoval: Number #i!
Number removeSelector: #i!
!classRemoval: #Complex!
Smalltalk removeClassNamed: #Complex!

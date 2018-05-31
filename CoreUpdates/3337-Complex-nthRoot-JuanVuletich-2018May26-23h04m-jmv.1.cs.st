'From Cuis 5.0 of 7 November 2016 [latest update: #3336] on 26 May 2018 at 11:05:24 pm'!

!Complex methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:57:33'!
nthRoot: aPositiveInteger

	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].

	^ Complex abs: (self abs nthRoot: aPositiveInteger) arg: self arg / aPositiveInteger! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 21:52:19'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	aPositiveInteger = 2 ifTrue: [
		^self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].
	
	^self negative
		ifTrue: [
			aPositiveInteger even
				ifTrue: [ (Complex basicReal: self imaginary: 0) nthRoot: aPositiveInteger ]
				ifFalse: [ (self negated nthRoot: aPositiveInteger) negated ]]
		ifFalse: [ self raisedTo: 1.0 / aPositiveInteger ]! !


!Fraction methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:57:41'!
nthRoot: aPositiveInteger
	"Answer the nth root of the receiver."
	| guess |
	aPositiveInteger = 2 ifTrue: [
		^ self sqrt ].

	(aPositiveInteger isInteger not or: [ aPositiveInteger negative ])
		ifTrue: [^ ArithmeticError signal: 'nth root only defined for positive Integer n.'].

	(self negative and: [ aPositiveInteger even ]) ifTrue: [
		^(Complex basicReal: self imaginary: 0) nthRoot: aPositiveInteger ].
	
	guess _ (numerator nthRootTruncated: aPositiveInteger) /
				(denominator nthRootTruncated: aPositiveInteger).
	(guess raisedTo: aPositiveInteger) = self ifTrue: [
		^ guess ].
	"There is no exact nth root, so answer a Float approximation"
	^ (self abs ln / aPositiveInteger) exp * self sign! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 5/26/2018 22:57:47'!
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
		^ (Complex basicReal: self imaginary: 0) nthRoot: aPositiveInteger ].

	guess _ self nthRootRounded: aPositiveInteger.
	excess _ (guess raisedTo: aPositiveInteger) - self.
	excess = 0 ifTrue: [ ^ guess ].

	nBits _ Float precision - guess highBitOfMagnitude.
	nBits <= 0 ifTrue: [ ^(Fraction numerator: guess * 4 - excess sign denominator: 4) asFloat].

	scaled _ self << (nBits * aPositiveInteger).
	guess _ scaled nthRootRounded: aPositiveInteger.
	excess _ (guess raisedTo: aPositiveInteger) - scaled.
	^(Fraction numerator: guess * 4 - excess sign denominator: 1 << (nBits + 2)) asFloat! !


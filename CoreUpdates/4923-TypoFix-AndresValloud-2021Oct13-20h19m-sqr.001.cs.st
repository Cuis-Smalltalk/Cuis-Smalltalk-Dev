'From Cuis 5.0 [latest update: #4922] on 13 October 2021 at 8:21:22 pm'!

!Integer methodsFor: 'mathematical functions' stamp: 'sqr 10/11/2021 21:24:29'!
ifMultipleOf2And5Do: aBlock otherwise: anotherBlock
	"If our prime factorization consists only of 2's and 5's, evaluate aBlock with the exponents.
	Otherwise evaluate anotherBlock.
	Be fast!!"

	| exponent2 exponent5 without2Factors |
	exponent2 _ self lowBit-1.
	without2Factors _ self bitShift: exponent2 negated.
	exponent5 _ ( 0.430676558073393 "2 ln / 5 ln" * without2Factors highBit) truncated.
	(5 raisedToInteger: exponent5) = without2Factors
		ifTrue: [
			aBlock value: exponent2 value: exponent5 ]
		ifFalse: [
			anotherBlock value ]! !


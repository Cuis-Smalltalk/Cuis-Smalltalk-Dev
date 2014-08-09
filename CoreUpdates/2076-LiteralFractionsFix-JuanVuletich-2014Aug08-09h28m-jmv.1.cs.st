'From Cuis 4.2 of 25 July 2013 [latest update: #2074] on 8 August 2014 at 9:42:25.309375 am'!

!Fraction methodsFor: 'printing' stamp: 'jmv 8/8/2014 09:37'!
isLiteral
	"
	1e-3 isLiteral
	(1/3) isLiteral
	"
	denominator
		ifMultipleOf2And5Do: [ :exponent2 :exponent5 |
			^true]
		otherwise: [
			^false]! !

!Fraction methodsFor: 'printing' stamp: 'jmv 8/8/2014 09:41'!
storeOn: aStream
	| e f |
	
	"If possible, store as a literal of the form 9999e-99
	If denominator's prime factors are just 2 and 5, then we can be printed as a literal
	of the form 999999e-99"
	"
	123e-12 storeString
	123e-12 printString
	123e-12000 storeString
	123e-12000 printString
	(3/4) storeString
	(3/4) printString
	(-3/4) storeString
	(-3/4) printString
	(1/3) storeString
	(1/3) printString
	"
	denominator
		ifMultipleOf2And5Do: [ :exponent2 :exponent5 |
			exponent2 > exponent5
				ifTrue: [
					e _ exponent2.
					f _ 5 raisedToInteger: e-exponent5 ]
				ifFalse: [
					e _ exponent5.
					f _ 2 bitShift: e-exponent2-1 ].
			numerator*f storeOn: aStream base: 10.
			aStream nextPut: $e; nextPut: $-.
			e storeOn: aStream base: 10 ]
		otherwise: [
			super storeOn: aStream ]
! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/8/2014 09:34'!
ifMultipleOf2And5Do: aBlock otherwise: anotherBlock
	"If our prime factorization consists only of 2's and 5's, evaluata aBlock with the exponents.
	Otherwise evaluate anotherBlock.
	Be fast!!"

	| exponent2 exponent5 without2Factors |
	exponent2 _ self lowBit-1.
	without2Factors _ self bitShift: exponent2 negated.
	exponent5 _ (without2Factors highBit / (5 log / 2 log)) rounded.
	(5 raisedToInteger: exponent5) = without2Factors
		ifTrue: [
			aBlock value: exponent2 value: exponent5 ]
		ifFalse: [
			anotherBlock value ]! !


!Object methodsFor: 'printing' stamp: 'jmv 8/8/2014 09:35'!
isLiteral
	"Answer whether the receiver has a literal text form recognized by the compiler.
	The literal form must be provided by #storeOn:
	"

	^false! !


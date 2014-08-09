'From Cuis 4.2 of 25 July 2013 [latest update: #2076] on 8 August 2014 at 7:39:14.257514 pm'!

!Fraction methodsFor: 'printing' stamp: 'nice 12/29/2012 23:01'!
printAsLiteralOn: aStream
	| n d base powers pow2 maximumBase |
	n := numerator.
	d := denominator.
	powers := Dictionary new.
	(pow2 := d lowBit - 1) = 0
		ifTrue: 
			[base := 1]
		ifFalse:
			[base := 2.
			powers at: 2 put: pow2.
			d := d >> pow2].
	maximumBase := 36.
	Integer largePrimesUpTo: (d sqrtFloor max: 2) do: [:p |
		| pow |
		(d rem: p) = 0
			ifTrue:
				[pow := 1.
				[((d := d quo: p) rem: p) = 0] whileTrue: [pow := pow + 1].
				base := base * p.
				base > maximumBase ifTrue: [self halt: 'cannot print in base > ' , maximumBase printString].
				powers at: p put: pow].
		p squared > d
			ifTrue:
				["Decomposition done, we can now print"
				| maximumPower b |
				d = 1 ifFalse: [base := base * d. powers at: d put: 1].
				base > maximumBase ifTrue: [self halt: 'cannot print in base > ' , maximumBase printString].
				base = 2 ifTrue: [base := 10. powers at: 5 put: 0].
				maximumPower := powers detectMax: [:each | each].
				powers keysAndValuesDo: [:k :v | n := n * (k raisedTo: maximumPower - v)].
				b := base.
				[maximumPower > 1 and: [b * base <= maximumBase]]
					whileTrue:
						[b := b * base.
						maximumPower := maximumPower - 1].
				n storeOn: aStream base: b.
				aStream nextPutAll: 'e-'.
				maximumPower storeOn: aStream.
				^self]]! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 8/8/2014 19:36'!
ifMultipleOf2And5Do: aBlock otherwise: anotherBlock
	"If our prime factorization consists only of 2's and 5's, evaluata aBlock with the exponents.
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


!Fraction reorganize!
('arithmetic' * + - / negated)
('comparing' < <= = hash)
('converting' adaptToInteger:andSend: asFloat asFraction isFraction)
('mathematical' reciprocal)
('mathematical functions' ln log nthRoot: raisedToInteger: sqrt squared)
('printing' isLiteral printAsLiteralOn: printOn:base: printOn:fractionDigits: storeOn: storeOn:base:)
('testing' negative)
('truncation and round off' truncated)
('private' denominator numerator reduced setNumerator:denominator:)
!


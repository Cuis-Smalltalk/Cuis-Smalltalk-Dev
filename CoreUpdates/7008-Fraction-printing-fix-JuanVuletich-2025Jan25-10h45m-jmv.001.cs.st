'From Cuis7.3 [latest update: #7006] on 25 January 2025 at 11:03:27 am'!

!Fraction methodsFor: 'printing' stamp: 'jmv 1/25/2025 10:58:09'!
printAsLiteralIfAtAllPossibleOn: aStream
	| n d base powers pow2 maximumBase |
	n := numerator.
	d := denominator.
	powers := Dictionary new.
	(pow2 := d lowBit - 1) = 0
		ifTrue: [
			base := 1]
		ifFalse: [
			base := 2.
			powers at: 2 put: pow2.
			d := d >> pow2].
	maximumBase := 36.
	Integer largePrimesUpTo: (d sqrtFloor max: 2) do: [ :p |
		| pow |
		(d rem: p) = 0
			ifTrue: [
				pow := 1.
				[((d := d quo: p) rem: p) = 0] whileTrue: [pow := pow + 1].
				base := base * p.
				base > maximumBase ifTrue: [
					"self halt: 'cannot print in base > ' , maximumBase printString."
					^self printOn: aStream.
					].
				powers at: p put: pow].
		p squared > d
			ifTrue: [ "Decomposition done, we can now print"
				| maximumPower b |
				d = 1 ifFalse: [base := base * d. powers at: d put: 1].
				base > maximumBase ifTrue: [
					"self halt: 'cannot print in base > ' , maximumBase printString."
					^self printOn: aStream.
					].
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
				^self ]].! !


!Fraction methodsFor: 'printing' stamp: 'jmv 1/25/2025 11:03:25'!
printAsLiteralOn: aStream
	"
	{123e-12000} storeString
	{123e-12000} printString
	"
	self storeOn: aStream! !


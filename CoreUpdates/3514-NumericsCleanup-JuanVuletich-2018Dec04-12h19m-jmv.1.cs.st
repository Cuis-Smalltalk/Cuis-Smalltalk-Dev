'From Cuis 5.0 of 7 November 2016 [latest update: #3512] on 4 December 2018 at 12:21:28 pm'!

!Float methodsFor: 'converting' stamp: 'jmv 12/4/2018 12:20:55'!
partValues: aThreeArgumentBlock ifInfinite: infinityBlock ifNaN: nanBlock
	"
	Float pi hex print
	Float pi partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	0.0 partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	For 0.0, exponent will be the minimum possible, i.e.  -1023, and mantissa will be 0.
	"
	| sign exponent mantissa |

	self partBits: [ :signBit :exponentBits :mantissaBits |

		"Extract the sign"
		sign _ signBit = 0 ifTrue: [1] ifFalse: [-1].

		"Special cases: infinites and NaN"
		exponentBits = 16r7FF ifTrue: [
			^mantissaBits = 0
				ifTrue: [ infinityBlock valueWithPossibleArgument: self ]
				ifFalse: [ nanBlock valueWithPossibleArgument: self and: mantissaBits ]].

		"Unbias exponent: 16r3FF is bias"
		exponent _ exponentBits - 16r3FF.

		"Add back implicit leading 1 in fraction if appropriate"
		"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa"	
		exponentBits = 0
			ifTrue: [
				mantissa _ mantissaBits.
				exponent _ exponent + 1 ]
			ifFalse: [
				mantissa _ 16r0010000000000000 bitOr: mantissaBits ].

		"Evaluate the block"
		^aThreeArgumentBlock value: sign value: exponent value: mantissa
	]! !


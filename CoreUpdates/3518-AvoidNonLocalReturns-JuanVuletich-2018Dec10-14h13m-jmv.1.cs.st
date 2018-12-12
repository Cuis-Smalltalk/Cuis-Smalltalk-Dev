'From Cuis 5.0 of 7 November 2016 [latest update: #3517] on 10 December 2018 at 6:59:08 pm'!
!Float methodsFor: 'converting' stamp: 'jmv 12/10/2018 18:49:42'!
partValues: aThreeArgumentBlock ifInfinite: infinityBlock ifNaN: nanBlock
	"
	Float pi hex print
	Float pi partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	0.0 partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	For 0.0, exponent will be the minimum possible, i.e.  -1023, and mantissa will be 0.
	"
	| sign exponent mantissa |

	^ self partBits: [ :signBit :exponentBits :mantissaBits |

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
		aThreeArgumentBlock value: sign value: exponent value: mantissa
	]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/10/2018 18:49:08'!
exponentBits
	"
	Actual bits for the exponent part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not substract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f exponentBits. f exponentPart. f exponent } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | exponentBits ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/10/2018 18:49:15'!
mantissaBits
	"
	Actual bits for the mantissa part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not substract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f mantissaBits. f mantissaPart. f significand. } print ].
	"
	^ self partBits: [ :signBit :exponentBits :mantissaBits | mantissaBits ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/10/2018 18:54:58'!
nextAwayFromZero
	"Answer the Float with smallest magnitude but larger than ours, with the same sign
	Only for finite numbers."

	| exponent mantissa |
	^ self partValues: [ :sign :myExponent :myMantissa |
		myMantissa = 16r1FFFFFFFFFFFFF
			ifTrue: [
				mantissa _ 16r10000000000000.
				exponent _ myExponent +1 ]
			ifFalse: [
				mantissa _ myMantissa+1.
				exponent _ myExponent ].
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/10/2018 18:55:04'!
nextTowardsZero
	"Answer the Float with largest magnitude but smaller than ours, with the same sign.
	Only for finite, non zero numbers."

	| exponent mantissa |
	^ self partValues: [ :sign :myExponent :myMantissa |
		myMantissa isZero ifTrue: [ self error: 'Not for zero values' ].
		(myMantissa = 16r10000000000000 and: [myExponent > Float emin])
			ifTrue: [
				mantissa _ 16r1FFFFFFFFFFFFF.
				exponent _ myExponent -1 ]
			ifFalse: [
				mantissa _ myMantissa-1.
				exponent _ myExponent ].
		Float signPart: sign mantissaPart: mantissa exponentPart:  exponent ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/10/2018 18:49:24'!
signBit
	"
	Actual bits for the exponent part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not substract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0. 0.0. -0.0} do: [ :f |
		{ f. f signBit. f signPart. f sign } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | signBit ]! !


!Integer methodsFor: 'bit manipulation' stamp: 'jmv 12/10/2018 18:36:21'!
lowBit
	"Answer the index of the low order bit of this number"

	| byte byteIndex byteSize |
	byteIndex _ 1.
	byteSize _ self digitLength.
	[ byteIndex <= byteSize ] whileTrue: [
		byte _ self at: byteIndex.
		byte > 0 ifTrue: [ ^ byteIndex - 1 * 8 + byte lowBit ].
		byteIndex _ byteIndex + 1 ].
	^ 0.! !

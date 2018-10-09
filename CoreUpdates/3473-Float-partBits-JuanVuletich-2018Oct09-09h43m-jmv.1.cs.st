'From Cuis 5.0 of 7 November 2016 [latest update: #3472] on 9 October 2018 at 9:53:17 am'!

!Float methodsFor: 'converting' stamp: 'jmv 10/8/2018 18:05:43'!
partBits: aThreeArgumentBlock
	"
	Extract the bits for Sign, Mantissa and Exponent parts of the Floating Point representation.
	Just extract the bits. Do not add implicit bit. Do not correct denormals. Do not substract exponent bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	| allBits signBit exponentBits mantissaBits |

	" Extract the bits of an IEEE double float "
	allBits _ ((self basicAt: 1) bitShift: 32) + (self basicAt: 2).

	" Extract the sign and the biased exponent "
	signBit _ allBits bitShift: -63.
	exponentBits _ (allBits bitShift: -52) bitAnd: 16r7FF.

	" Extract fractional part "
	mantissaBits _ allBits bitAnd: 16r000FFFFFFFFFFFFF.

	"Evaluate the block"
	^aThreeArgumentBlock value: signBit value: exponentBits value: mantissaBits! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 10/8/2018 18:10:16'!
exponentBits
	"
	Actual bits for the exponent part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not substract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f exponentBits. f exponentPart. f exponent } print ].
	"

	self partBits: [ :signBit :exponentBits :mantissaBits |
		^ exponentBits ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 10/8/2018 18:23:14'!
mantissaBits
	"
	Actual bits for the mantissa part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not substract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f mantissaBits. f mantissaPart. f significand. } print ].
	"
	self partBits: [ :signBit :exponentBits :mantissaBits |
		^ mantissaBits ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 10/8/2018 18:15:24'!
signBit
	"
	Actual bits for the exponent part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not substract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0. 0.0. -0.0} do: [ :f |
		{ f. f signBit. f signPart. f sign } print ].
	"

	self partBits: [ :signBit :exponentBits :mantissaBits |
		^ signBit ]! !


!Float methodsFor: 'converting' stamp: 'jmv 10/8/2018 18:17:37'!
partValues: aThreeArgumentBlock ifInfinite: infiniryBlock ifNaN: nanBlock
	"
	Float pi hex print
	Float pi partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	0.0 partValues: [ :sign :exponent :mantissa | { sign hex. exponent hex. mantissa hex} print ]
	For 0.0, exponent will be the minimum possible, i.e.  -1023, and mantissa will be 0.
	"
	| sign exponent mantissa |

	self partBits: [ :signBit :exponentBits :mantissaBits |

		" Extract the sign"
		sign _ signBit = 0 ifTrue: [1] ifFalse: [-1].

		" Special cases: infinites and NaN"
		exponentBits = 16r7FF ifTrue: [
			^mantissaBits = 0
				ifTrue: [ infiniryBlock valueWithPossibleArgument: self ]
				ifFalse: [ nanBlock valueWithPossibleArgument: self and: mantissaBits ]].

		"Unbias exponent: 16r3FF is bias"
		exponent _ exponentBits - 16r3FF.

		" Replace omitted leading 1 in fraction if appropriate"
		"If expPart = 0, I am +/-zero or a denormal value. In such cases, no implicit leading bit in mantissa"	
		exponentBits = 0
			ifTrue: [
				mantissa _ mantissaBits.
				exponent _ exponent + 1 ]
			ifFalse: [
				mantissa _ mantissaBits bitOr: 16r0010000000000000 ].

		"Evaluate the block"
		^aThreeArgumentBlock value: sign value: exponent value: mantissa
	]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 10/8/2018 18:13:53'!
significandAsInteger
	"The actual bits in the mantissa of the receiver, as an integer number, including the implicit leading 1 if appropriate.
	See #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self mantissaPart! !


!BoxedFloat64 methodsFor: 'truncation and round off' stamp: 'jmv 10/8/2018 17:52:44'!
exponent
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa between one and two  or between -1 and -2 (#significand).
	 Answer with the SmallInteger to whose power two is raised.
	Optional. See Object documentation whatIsAPrimitive.
	See also #signPart,  #significandAsInteger and #exponentPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. f significand . f exponent. f significand * (2 raisedToInteger: f exponent) = f } print ].
	"

	| positive |
	<primitive: 53>
	self >= 1.0 ifTrue: [^self floorLog: 2].
	self > 0.0
		ifTrue: 
			[positive _ (1.0 / self) exponent.
			self = (1.0 / (1.0 timesTwoPower: positive))
				ifTrue: [^positive negated]
				ifFalse: [^positive negated - 1]].
	self = 0.0 ifTrue: [^-1].
	^self negated exponent! !


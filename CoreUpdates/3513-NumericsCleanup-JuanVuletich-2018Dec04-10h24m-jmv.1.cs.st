'From Cuis 5.0 of 7 November 2016 [latest update: #3512] on 4 December 2018 at 12:19:11 pm'!

!Float methodsFor: 'converting' stamp: 'jmv 12/4/2018 12:11:37'!
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

	"Extract the bits of an IEEE double float "
	allBits _ ((self basicAt: 1) bitShift: 32) + (self basicAt: 2).

	"Extract the sign and the biased exponent "
	signBit _ allBits bitShift: -63.
	exponentBits _ (allBits bitShift: -52) bitAnd: 16r7FF.

	"Extract fractional part "
	mantissaBits _ allBits bitAnd: 16r000FFFFFFFFFFFFF.

	"Evaluate the block"
	^aThreeArgumentBlock value: signBit value: exponentBits value: mantissaBits! !

!Float methodsFor: 'converting' stamp: 'jmv 12/4/2018 12:12:49'!
partValues: aThreeArgumentBlock ifInfinite: infiniryBlock ifNaN: nanBlock
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
				ifTrue: [ infiniryBlock valueWithPossibleArgument: self ]
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

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 12:13:25'!
mantissaPart
	"Equivalent to #significandAsInteger."
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	Does not include de sign.
	See #exponentPart and #signPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f mantissaPart * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | mantissa ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 12:15:28'!
predecessor
	"Answer the largest Float smaller than self"
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	ulp := self ulp.
	^self - (0.5 * ulp) = self
		ifTrue: [self - ulp]
		ifFalse: [self - (0.5 * ulp)]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 12:13:37'!
significandAsInteger
	"The actual bits in the mantissa of the receiver, as an integer number, including the implicit leading 1 if appropriate.
	See #mantissaPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self mantissaPart! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 12:15:19'!
successor
	"Answer the smallest Float greater than self"
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	ulp := self ulp.
	^self + (0.5 * ulp) = self
		ifTrue: [self * -1.0 - ulp * -1.0 "This trick is for obtaining a negativeZero"]
		ifFalse: [self + (0.5 * ulp)]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/4/2018 12:15:44'!
ulp
	"Answer the unit of least precision of the receiver"
	
	| exponent |
	self isFinite ifFalse: [^self abs].
	self isZero ifTrue: [^Float fmin].
	exponent := self exponent.
	^exponent < self class emin
		ifTrue: [Float fminDenormalized]
 		ifFalse: [Float epsilon timesTwoPower: exponent]! !

!Float methodsFor: 'printing' stamp: 'jmv 12/4/2018 12:17:36'!
printOn: aStream base: base
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:" 

	self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
	self > 0.0
		ifTrue: [self absPrintOn: aStream base: base]
		ifFalse:
			[self sign = -1
				ifTrue: [aStream nextPutAll: '-'].
			self isZero
				ifTrue: [aStream nextPutAll: '0.0']
				ifFalse: [self negated absPrintOn: aStream base: base]]! !


!Float class methodsFor: 'constants' stamp: 'jmv 12/4/2018 11:57:45'!
emin
	"Answer the exponent of the non-denormal value with smallest magnitude"
	
	^-1022! !

!Float class methodsFor: 'constants' stamp: 'jmv 12/4/2018 11:58:23'!
precision
	"Answer the apparent precision of the floating point representation.
	That is the maximum number of radix-based digits (bits if radix=2) representable in floating point without rounding error.
	Technically, 52 bits are stored in the representation, and normalized numbers have an implied leading 1 that does not need to be stored.
	Note that denormalized floating point numbers don't have the implied leading 1, and thus gradually lose precision.
	This format implements the IEEE-754 binary64 format."
	
	^53! !


!Fraction methodsFor: 'converting' stamp: 'jmv 12/4/2018 11:57:12'!
asFloat
	"Answer a Float that closely approximates the value of the receiver.
	This implementation will answer the closest floating point number to the receiver.
	In case of a tie, it will use the IEEE 754 round to nearest even mode.
	In case of overflow, it will answer +/- Float infinity."

	| a b mantissa exponent floatMantissa hasTruncatedBits lostBit n ha hb hm |
	a := numerator abs.
	b := denominator.	"denominator is always positive"
	ha := a highBit.
	hb := b highBit.
	
	"Number of bits to keep in mantissa plus one to handle rounding."
	n := 1 + Float precision.

	"If both numerator and denominator are represented exactly as floating point number,
	float division is fastest."
	(ha < n and: [hb < n]) ifTrue: [^numerator asFloat / denominator asFloat].

	"Shift the fraction by a power of two exponent so as to obtain a mantissa with n bits.
	The first guess is approximate, the mantissa might have n+1 bits."
	exponent := ha - hb - n.
	exponent >= 0
		ifTrue: [b := b bitShift: exponent]
		ifFalse: [a := a bitShift: exponent negated].
	mantissa := a quo: b.
	hasTruncatedBits := a > (mantissa * b).
	hm := mantissa highBit.
	
	"Check for gradual underflow, in which case the mantissa will lose bits.
	Keep at least one bit to let underflow preserve the sign of zero."
	lostBit := Float emin - (exponent + hm - 1).
	lostBit > 0 ifTrue: [n := n - lostBit max: 1].

	"Remove excess bits in the mantissa."
	hm > n
		ifTrue:
			[exponent := exponent + hm - n.
			hasTruncatedBits := hasTruncatedBits or: [mantissa anyBitOfMagnitudeFrom: 1 to: hm - n].
			mantissa := mantissa bitShift: n - hm].

	"Check if mantissa must be rounded upward.
	The case of tie will be handled by Integer>>asFloat."
	(hasTruncatedBits and: [mantissa odd])
		ifTrue: [mantissa := mantissa + 1].

	floatMantissa := mantissa asFloat.
	self positive ifFalse: [floatMantissa := floatMantissa negated].
	^floatMantissa timesTwoPower: exponent! !


!Integer methodsFor: 'bit manipulation' stamp: 'jmv 12/4/2018 11:59:58'!
highBitOfMagnitude
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Receiver has to be positive!!"

	| shifted bitNo |
	shifted := self < 0 ifTrue: [0 - self] ifFalse: [self].
	bitNo := 0.
	[shifted < 65536]
		whileFalse: 
			[shifted := shifted bitShift: -16.
			bitNo := bitNo + 16].
	shifted < 256
		ifFalse: 
			[shifted := shifted bitShift: -8.
			bitNo := bitNo + 8].
		
	"The high bits table can be obtained with:
	(1 to: 8) inject: #[0] into: [:highBits :rank | highBits , (highBits collect: [:e | rank])]."
	^bitNo + ( #[0 1 2 2 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] at: shifted + 1)! !


!LargePositiveInteger methodsFor: 'bit manipulation' stamp: 'jmv 12/4/2018 11:59:42'!
highBit
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."
	^ self highBitOfMagnitude! !

!LargePositiveInteger methodsFor: 'bit manipulation' stamp: 'jmv 12/4/2018 12:04:04'!
highBitOfMagnitude
	"Answer the position of the leading bit or zero if the  
	receiver is zero. 
	This method is used for LargeNegativeIntegers as well,  
	since Squeak's LargeIntegers are sign/magnitude."
	| byteIndex msByte |
	byteIndex := self digitLength.
	[byteIndex > 0] whileTrue:
		[
			msByte  := self at: byteIndex.
			msByte  > 0 ifTrue: [^byteIndex - 1 * 8 + msByte  highBit].
			byteIndex := byteIndex - 1
		].
	^0! !


!LargeNegativeInteger methodsFor: 'bit manipulation' stamp: 'jmv 12/4/2018 11:59:30'!
highBit
	"Answer the position of the leading bit or zero if the  
	receiver is zero. Raise an error if the receiver is negative, since  
	negative integers are defined to have an infinite number of leading 1's 
	in 2's-complement arithmetic. Use >>highBitOfMagnitude if you want to  
	get the highest bit of the magnitude."

	^self error: 'highBit is not defined for negative integers'! !


'From Cuis 5.0 of 7 November 2016 [latest update: #3538] on 21 December 2018 at 2:53:25 pm'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 12/21/2018 11:06:01'!
expNonPrimitive
	"Answer e raised to the receiver power."

	| base fract correction delta div |

	"Taylor series"
	"check the special cases"
	self < 0.0 ifTrue: [^ (self negated exp) reciprocal].
	self = 0.0 ifTrue: [^ 1].
	self abs > MaxValLn ifTrue: [self error: 'exp overflow'].

	"get first approximation by raising e to integer power"
	base _ E raisedToInteger: (self truncated).

	"now compute the correction with a short Taylor series"
	"fract will be 0..1, so correction will be 1..E"
	"in the worst case, convergance time is logarithmic with 1/Epsilon"
	fract _ self fractionPart.
	fract = 0.0 ifTrue: [ ^ base ].  "no correction required"

	correction _ 1.0 + fract.
	delta _ fract * fract / 2.0.
	div _ 2.0.
	[delta >= base ulp] whileTrue: [
		correction _ correction + delta.
		div _ div + 1.0.
		delta _ delta * fract / div].
	correction _ correction + delta.
	^ base * correction! !

!Float methodsFor: 'converting' stamp: 'jmv 12/21/2018 11:12:14'!
partBits: aThreeArgumentBlock
	"
	Extract the bits for Sign, Mantissa and Exponent parts of the Floating Point representation.
	Just extract the bits. Do not add implicit bit. Do not correct denormals. Do not subtract exponent bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	| signBit exponentBits mantissaBits leastSignificativeWord mostSignificativeWord |

	mostSignificativeWord _ self basicAt: 1.
	leastSignificativeWord _ self basicAt: 2.
	signBit _ mostSignificativeWord bitShift: -31 .
	exponentBits _ (mostSignificativeWord bitShift: -20 ) bitAnd: 16r7FF.
	mantissaBits _ ((mostSignificativeWord bitAnd: 16r000FFFFF) bitShift: 32) + leastSignificativeWord.

	"Evaluate the block"
	^aThreeArgumentBlock value: signBit value: exponentBits value: mantissaBits! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/21/2018 11:12:06'!
exponentBits
	"
	Actual bits for the exponent part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f exponentBits. f exponentPart. f exponent } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | exponentBits ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/21/2018 11:12:10'!
mantissaBits
	"
	Actual bits for the mantissa part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f mantissaBits. f mantissaPart. f significand. } print ].
	"
	^ self partBits: [ :signBit :exponentBits :mantissaBits | mantissaBits ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/21/2018 11:12:18'!
signBit
	"
	Actual bits for the exponent part of the Floating Point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0. 0.0. -0.0} do: [ :f |
		{ f. f signBit. f signPart. f sign } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | signBit ]! !


!Float class methodsFor: 'constants' stamp: 'jmv 12/21/2018 11:02:26'!
e
	"Answer the constant, e."

	^E! !

!Float class methodsFor: 'constants' stamp: 'jmv 12/21/2018 12:54:06'!
emax
	"Answer exponent of the maximal representable value"
	
	^1023! !

!Float class methodsFor: 'constants' stamp: 'jmv 12/21/2018 12:54:26'!
epsilon
	"Answer the difference between 1.0 and the next representable value.
	Note: does not equal 1.0 ulp."
	
	^1.0 successor - 1.0! !


!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 12/21/2018 11:04:53'!
exp
	"Answer e raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 59>
	^ self expNonPrimitive! !


!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 12/21/2018 11:05:17'!
exp
	"Answer e raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 559>
	^ self expNonPrimitive! !


!Fraction methodsFor: 'printing' stamp: 'jmv 12/21/2018 11:15:47'!
printOn: aStream fractionDigits: placesDesired
	"Same as super, but provides a faster implementation by inlining some Fraction protocol thus avoiding intermediate Fraction creation."
	
	| roundedFractionPart integerPart scaling |
	placesDesired > 0
		ifFalse: [self rounded printOn: aStream]
		ifTrue: [
			scaling := 10 raisedToInteger: placesDesired.
			integerPart := numerator abs quo: denominator.
			roundedFractionPart := (numerator abs - (integerPart * denominator)) * scaling * 2 + denominator quo: denominator * 2.
			roundedFractionPart = scaling
				ifTrue:
					[integerPart := integerPart + 1.
					roundedFractionPart := 0].
			"Don't print minus sign if result is rounded to zero"
			(numerator negative and: [integerPart > 0 or: [roundedFractionPart > 0]]) ifTrue: [aStream nextPut: $-].
			integerPart printOn: aStream.
			aStream nextPut: $..
			roundedFractionPart printOn: aStream base: 10 length: placesDesired padded: true].! !


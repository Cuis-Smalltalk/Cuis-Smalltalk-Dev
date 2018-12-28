'From Cuis 5.0 of 7 November 2016 [latest update: #3546] on 26 December 2018 at 2:34:46 pm'!

!Float methodsFor: 'converting' stamp: 'jmv 12/26/2018 14:05:07'!
asTrueFraction
	" Answer a fraction that EXACTLY represents self,
	  a double precision IEEE floating point number.
	  By David N. Smith with significant performance
	  improvements by Luciano Esteban Notarfrancesco.
	  (Version of 11April97).
	Refactoring and simplification by jmv"
	
	^self
		partValues: [ :sign :exponent :mantissa | | zeroBitsCount |
			" Prepare result. If exponent is greater than mantissa size, result is an integer"
			(exponent >= 52 or: [
					zeroBitsCount _ mantissa lowBit - 1.
					exponent + zeroBitsCount >= 52 ])
				ifTrue: [
					"result is an integer number"
					 sign * mantissa bitShift: exponent - 52 ]
				ifFalse: [
					" This is the 'obvious' way. Better do Luciano's trick below:"
					"result := Fraction
						numerator: sign * mantissa
						denominator: (1 bitShift: 52 - exponent)."
					" Form the result. When exp>52, the exponent is adjusted by
					  the number of trailing zero bits in the mantissa to minimize
					  the (huge) time that could be spent in #gcd:. "
					Fraction
						numerator: (sign * (mantissa bitShift: 0 - zeroBitsCount))
						denominator: (1 bitShift: 52 - exponent - zeroBitsCount) ]
		]
		ifInfinite: [ self error: 'Cannot represent infinity as a fraction' ]
		ifNaN: [ self error: 'Cannot represent Not-a-Number as a fraction' ].! !

!Float methodsFor: 'converting' stamp: 'jmv 12/26/2018 14:33:41'!
partBits: aThreeArgumentBlock
	"
	Extract the bits for Sign, Mantissa and Exponent parts of the floating point representation.
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

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/26/2018 14:32:49'!
exponentBits
	"
	Actual bits for the exponent part of the floating point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f exponentBits. f exponentPart. f exponent } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | exponentBits ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/26/2018 14:33:27'!
exponentPart
	"
	Exponent part of the floating point representation.
	Valid for any floating point number (except zeros, infinities and NaNs).
	Includes correction of stored exponent bits for denormals (where it acts as a label, not a real exponent).
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | exponent ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/26/2018 14:34:33'!
mantissaBits
	"
	Actual bits for the mantissa part of the floating point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{ f. f mantissaBits. f mantissaPart. f significand. } print ].
	"
	^ self partBits: [ :signBit :exponentBits :mantissaBits | mantissaBits ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/26/2018 14:28:48'!
mantissaPart
	"Equivalent to #significandAsInteger."
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	Does not include the sign.
	See #exponentPart and #signPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f mantissaPart * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self partValues: [ :sign :exponent :mantissa | mantissa ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/26/2018 14:34:07'!
signBit
	"
	Actual bits for the exponent part of the floating point representation.
	Just extract the bits. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0. 0.0. -0.0} do: [ :f |
		{ f. f signBit. f signPart. f sign } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | signBit ]! !


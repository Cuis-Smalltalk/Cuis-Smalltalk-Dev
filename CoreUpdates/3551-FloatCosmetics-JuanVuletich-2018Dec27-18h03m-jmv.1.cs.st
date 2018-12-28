'From Cuis 5.0 of 7 November 2016 [latest update: #3550] on 27 December 2018 at 6:03:32 pm'!

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/26/2018 14:59:25'!
signBit
	"
	Actual sigh bit part of the floating point representation.
	Just extract the bit. Do not correct denormals. Do not subtract bias. Do nothing with infinites and NaN.
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0. 0.0. -0.0} do: [ :f |
		{ f. f signBit. f signPart. f sign } print ].
	"

	^ self partBits: [ :signBit :exponentBits :mantissaBits | signBit ]! !

!Float methodsFor: 'truncation and round off' stamp: 'jmv 12/26/2018 15:00:07'!
significandAsInteger
	"The actual bits in the mantissa of the receiver, as an integer, including the implicit leading 1 if appropriate.
	See #mantissaPart
	"
	"
	{Float pi. Float fminNormalized. Float fminDenormalized. 2.0. -2.0} do: [ :f |
		{f. (f signPart * f significandAsInteger * (2 raisedToInteger: f exponentPart-52)) asFloat = f } print ].
	"
	^self mantissaPart! !

!Float methodsFor: 'printing' stamp: 'jmv 12/26/2018 14:57:20'!
printOn: aStream fractionDigits: placesDesired
	"This implementation avoids rounding errors doue to #rounded or #roundTo:
	Round to a suitable integer and insert the decimal point in the appropriately between the digits."

	| i s scaled |
	self isFinite ifFalse: [ ^self printOn: aStream ].

	placesDesired > 0 ifFalse: [
		^self rounded printOn: aStream ].

	scaled _ self * (10 raisedTo: placesDesired).
	"If rounding could possibly print a sequence that is read back as a different float, then go the more expensive Fraction way.
	If the following line is commented, #testPrintShowingDecimalPlaces4 will fail!!"
	scaled ulp > 1 ifTrue: [
		^ self asTrueFraction printOn: aStream fractionDigits: placesDesired ].
	i _ scaled rounded.
	i negative ifTrue: [
		aStream nextPut: $-.
		i _ i negated ].
	s _ i printString.
	placesDesired + 1 > s size
		ifTrue: [
			aStream nextPutAll: '0.'.
			placesDesired - s size timesRepeat: [ aStream nextPut: $0 ].
			aStream nextPutAll: s ]
		ifFalse: [
			aStream
				nextPutAll: (s copyFrom: 1 to: s size-placesDesired);
				nextPut: $.;
				nextPutAll: (s copyFrom: s size-placesDesired+1 to: s size) ]! !


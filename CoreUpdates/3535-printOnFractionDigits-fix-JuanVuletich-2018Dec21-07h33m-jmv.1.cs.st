'From Cuis 5.0 of 7 November 2016 [latest update: #3533] on 21 December 2018 at 7:39:11 am'!

!Number methodsFor: 'printing' stamp: 'jmv 12/20/2018 17:54:18'!
printOn: aStream fractionDigits: placesDesired
	"Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator."

	| rounder rounded roundedFractionPart |
	placesDesired > 0 ifFalse: [ ^ self rounded printOn: aStream ].
	rounder _ 10 raisedToInteger: placesDesired.
	rounded _ self roundTo: rounder reciprocal.
	rounded negative ifTrue: [ aStream nextPut: $- ].
	rounded _ rounded abs.
	rounded integerPart truncated printOn: aStream.
	aStream nextPut: $..
	roundedFractionPart _ (rounded fractionPart * rounder) truncated.
	roundedFractionPart
		printOn: aStream
		base: 10
		length: placesDesired
		padded: true! !

!Number methodsFor: 'printing' stamp: 'jmv 12/20/2018 16:30:54'!
printOn: aStream integerDigits: placesLeftOfFractionPoint fractionDigits: placesRightOfFractionPoint
	"placesLeftOfFractionPoint is the minimum to be used (use more if required)
	placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate."
	"
	String streamContents: [ :strm | 23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | 123456.23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float pi printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float nan printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float infinity printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float negativeInfinity printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float zero printOn: strm integerDigits: 3 fractionDigits: 5 ]
	"

	^self printOn: aStream integerDigits: placesLeftOfFractionPoint padWith: nil fractionDigits: placesRightOfFractionPoint positiveIndicator: nil! !


!Float methodsFor: 'printing' stamp: 'jmv 12/21/2018 07:38:12'!
printOn: aStream fractionDigits: placesDesired
	"This implementation avoids any rounding error caused by rounded or roundTo:
	The approach is simple. Round to appropriate integer, take the digits, and just 
	add the decimal point in the appropriate place."

	| i s scaled |
	self isFinite ifFalse: [ ^self printOn: aStream ].

	placesDesired > 0 ifFalse: [
		^self rounded printOn: aStream ].

	scaled _ self * (10 raisedTo: placesDesired).
	"If rounding could possible print a sequence that is read back as a different float, then go the more expensive Fraction way.
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


!Fraction methodsFor: 'printing' stamp: 'jmv 12/20/2018 17:54:02'!
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
			"Don't print minus sign if result is rouded to zero"
			(numerator negative and: [integerPart > 0 or: [roundedFractionPart > 0]]) ifTrue: [aStream nextPut: $-].
			integerPart printOn: aStream.
			aStream nextPut: $..
			roundedFractionPart printOn: aStream base: 10 length: placesDesired padded: true].! !


!Integer methodsFor: 'printing' stamp: 'jmv 12/20/2018 17:54:47'!
printOn: aStream fractionDigits: placesDesired
	"Same as super, but provides a faster implementation because fraction part and rounding are trivial."
	
	self printOn: aStream base: 10.

	placesDesired > 0
		ifTrue: [
			aStream nextPut: $..
			placesDesired timesRepeat: [
				aStream nextPut: $0 ]]! !


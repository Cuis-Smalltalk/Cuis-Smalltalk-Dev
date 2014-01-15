'From Cuis 4.2 of 25 July 2013 [latest update: #1919] on 5 January 2014 at 7:45:16.039915 pm'!

!Number methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:39'!
printOn: aStream fractionDigits: placesDesired
	"Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator."

	| rounder rounded roundedFractionPart |
	placesDesired <= 0 ifTrue: [ ^ self rounded printOn: aStream ].
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

!Number methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:19'!
printOn: aStream integerDigits: placesLeftOfFractionPoint fractionDigits: placesRightOfFractionPoint
	"placesLeftOfFractionPoint is the minimum to be used (use more if required)
	placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate."
	"
	String streamContents: [ :strm | 23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float nan printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float infinity printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float negativeInfinity printOn: strm integerDigits: 3 fractionDigits: 5 ]
	String streamContents: [ :strm | Float zero printOn: strm integerDigits: 3 fractionDigits: 5 ]
	"

	^self printOn: aStream integerDigits: placesLeftOfFractionPoint padWith: nil fractionDigits: placesRightOfFractionPoint positiveIndicator: nil! !

!Number methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:37'!
printOn: aStream integerDigits: placesLeftOfFractionPoint padWith: leftPaddingCharOrNil fractionDigits: placesRightOfFractionPoint positiveIndicator: aCharacterOrNil
	"placesLeftOfFractionPoint is the minimum to be used (use more if required)
	placesRightOfFractionPoint is strict. Add extra zeros or round as appropriate."
	"
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+ ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: nil ]
	String streamContents: [ :strm | 1.23 printOn: strm integerDigits: 3 padWith: $  fractionDigits: 5 positiveIndicator: Character space ]
	String streamContents: [ :strm | Float nan printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+ ]
	String streamContents: [ :strm | Float infinity printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+  ]
	String streamContents: [ :strm | Float negativeInfinity printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: nil ]
	String streamContents: [ :strm | Float zero printOn: strm integerDigits: 3 padWith: nil fractionDigits: 5 positiveIndicator: $+  ]
	"

	| withoutSignAndPad zeroPad |
	withoutSignAndPad _ String streamContents: [ :strm |
		self abs printOn: strm fractionDigits: placesRightOfFractionPoint ].
	self < 0
		ifTrue: [ aStream nextPut: $- ]
		ifFalse: [ aCharacterOrNil ifNotNil: [ aStream nextPut: aCharacterOrNil ]].
	zeroPad _ placesLeftOfFractionPoint - ((withoutSignAndPad indexOf: $. startingAt: 1 ifAbsent: [withoutSignAndPad size + 1]) - 1).
	zeroPad > 0 ifTrue: [
		zeroPad timesRepeat: [ aStream nextPut: (leftPaddingCharOrNil ifNil: [$0]) ]].
	aStream nextPutAll: withoutSignAndPad! !


!Float methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:38'!
printOn: aStream fractionDigits: placesDesired
	"This implementation avoids any rounding error caused by rounded or roundTo:
	The approach is simple. Round to appropriate integer, take the digits, and just 
	add the decimal point in the appropriate place."

	| i s |
	self isFinite ifFalse: [ ^self printOn: aStream ].

	placesDesired <= 0 ifTrue: [
		^self rounded printOn: aStream ].

	i _ (self * (10 raisedTo: placesDesired)) rounded.
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


!Fraction methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:38'!
printOn: aStream fractionDigits: placesDesired
	"Same as super, but provides a faster implementation by inlining some Fraction protocol thus avoiding intermediate Fraction creation."
	
	| roundedFractionPart integerPart scaling |
	placesDesired <= 0
		ifTrue: [self rounded printOn: aStream]
		ifFalse:
			[scaling := 10 raisedToInteger: placesDesired.
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


!Integer methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:38'!
printOn: aStream fractionDigits: placesDesired
	"Same as super, but provides a faster implementation because fraction part and rounding are trivial."
	
	self printOn: aStream base: 10.
	placesDesired <= 0
		ifFalse:
			[aStream nextPut: $..
			0 printOn: aStream base: 10 length: placesDesired padded: true].! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 1/5/2014 19:37'!
summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count _ 0.
		selection classesDo: [ :cls | count _ count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		methods := selection methods size.
		methods printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		linesOfCode := selection linesOfCode.
		linesOfCode printOn: strm.
		strm nextPutAll: ' ('.
		linesOfCode / methods asFloat printOn: strm fractionDigits: 2.
		strm nextPutAll: ' per method).' ]! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 1/5/2014 19:37'!
printOn: aStream total: total totalTime: totalTime tallyExact: isExact

	isExact
		ifTrue: [ 
			| myTally |
			myTally := tally.
			receivers
				ifNotNil: [ receivers asArray do: [ :r | myTally := myTally - r tally ] ].
			aStream
				print: myTally;
				space ]
		ifFalse: [ 
			| percentage |
			percentage := tally asFloat / total * 100.0.
			percentage printOn: aStream fractionDigits: 1.
			aStream
				nextPutAll: '% (';
				nextPutAll: (percentage * totalTime / 100) rounded asStringWithCommas;
				nextPutAll: ' ms) '].
	self isPrimitives
		ifTrue: [ 
			aStream
				nextPutAll: 'primitives';
				newLine ]
		ifFalse: [ 
			| className aSelector aClass |
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			className := aClass name contractTo: self maxClassNameSize.
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: (aSelector contractTo: self maxClassPlusSelectorSize - className size);
				newLine ]! !


!MessageTally methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:37'!
printOn: aStream total: total totalTime: totalTime tallyExact: isExact 

	isExact 
		ifTrue: [
			| myTally |
			myTally := tally.
			receivers
				ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
			aStream
				print: myTally;
				space]
		ifFalse: [
			| percentage |
			percentage := tally asFloat / total * 100.0.
			percentage printOn: aStream fractionDigits: 1.
			aStream
				nextPutAll: '% (';
				nextPutAll: (percentage * totalTime / 100) rounded asStringWithCommas;
				nextPutAll: 'ms) '].
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			| className aSelector aClass |
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			className := aClass name contractTo: self maxClassNameSize.
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: (aSelector 
							contractTo: self maxClassPlusSelectorSize - className size);
				newLine]! !


!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:02'!
matrixPrintString
	"
	(MatrixTransform2x3 withRadians: -3 scale: 12 position: 4.5@3) matrixPrintString
	"
	^String streamContents: [ :strm | self printMatrixOn: strm ]! !

!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:35'!
printMatrixOn: aStream
	aStream
		newLine;
		nextPutAll: '| '.
	self a11 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a12 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a13 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream nextPutAll: ' |'.

	aStream
		newLine;
		nextPutAll: '| '.
	self a21 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a22 printOn: aStream integerDigits: 1 fractionDigits: 3.
	aStream space; space.
	self a23 printOn: aStream integerDigits: 1 fractionDigits: 3.

	aStream
		nextPutAll: ' |';
		newLine! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 1/5/2014 18:55'!
withRadians: radians scale: aNumber position: aPoint
	"Translation is added at the end. This means that aPoint is in the outer coordinate space.
	MatrixTransform2x3 withRadians: -3 scale: 12 position: 4.5@3
	"
	^self new setRadians: radians scale: aNumber position: aPoint! !


!SmallInteger reorganize!
('arithmetic' * + - / // gcd: quo: \\)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed hashMultiply highBit highBitOfMagnitude lowBit)
('comparing' < <= = > >= identityHash ~=)
('converting' asCharacter asFloat)
('copying' clone shallowCopy)
('mathematical functions' sqrt)
('printing' decimalDigitLength destinationBuffer: numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength instVarAt: nextInstance nextObject)
('testing' even odd)
('private' fromString:radix: highBitOfPositiveReceiver)
!


!LargeNegativeInteger reorganize!
('arithmetic' abs negated)
('bit manipulation' bitAt: highBit)
('converting' asFloat normalize)
('mathematical functions' sqrt)
('printing' printOn:base:)
('testing' negative positive sign strictlyPositive)
!


!LargePositiveInteger reorganize!
('arithmetic' * + - / // abs negated quo: \\ \\\)
('bit manipulation' bitAt: bitReverse: hashMultiply highBit highBitOfMagnitude)
('comparing' < <= > >= hash)
('converting' asFloat normalize withAtLeastNDigits:)
('mathematical functions' mightBeASquare sqrt sqrtFloor)
('printing' printOn:base: printOn:base:nDigits: printStringBase:)
('system primitives' digitAt: digitAt:put: digitLength replaceFrom:to:with:startingAt:)
('testing' isPrime negative positive sign strictlyPositive)
!

!methodRemoval: Integer #printOn:showingDecimalPlaces:!
Integer removeSelector: #printOn:showingDecimalPlaces:!
!methodRemoval: Integer #printPaddedWith:to:!
Integer removeSelector: #printPaddedWith:to:!
!methodRemoval: Integer #printPaddedWith:to:base:!
Integer removeSelector: #printPaddedWith:to:base:!
!methodRemoval: Integer #printStringLength:!
Integer removeSelector: #printStringLength:!
!methodRemoval: Integer #printStringLength:padded:!
Integer removeSelector: #printStringLength:padded:!
!methodRemoval: Integer #printStringPadded:!
Integer removeSelector: #printStringPadded:!
!methodRemoval: Integer #storeOn:base:length:padded:!
Integer removeSelector: #storeOn:base:length:padded:!
!methodRemoval: Integer #storeStringBase:length:padded:!
Integer removeSelector: #storeStringBase:length:padded:!

!Integer reorganize!
('arithmetic' * + - / // alignedTo: crossSumBase: quo: reciprocalModulo: \\\)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: highBit highBitOfMagnitude lowBit noMask:)
('comparing' < <= = > >= hash)
('converting' adaptToFraction:andSend: asCharacter asColorOfDepth: asFloat asFraction asHexDigit asInteger asIntegerOrFloat)
('enumerating' timesRepeat:)
('inspecting' explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial gcd: lcm: ln log nthRoot: nthRootTruncated: productTo: raisedTo:modulo: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' asBytesDescription asStringThousandSeparator:includePlusSign: asStringWithCommas asStringWithCommasSigned asTwoCharacterString asWords destinationBuffer: digitBuffer: hex hex8 isLiteral numberOfDigitsInBase: printOn: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printOn:base:showRadix: printOn:fractionDigits: printString printStringBase:length:padded: printStringHex printStringRadix: printStringRoman radix: storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even isInteger isPowerOfTwo isPrime isProbablyPrime nextPrime)
('tiles')
('truncation and round off' asLargerPowerOfTwo asPowerOfTwo asSmallerPowerOfTwo atRandom atRandom: ceiling floor normalize rounded truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
!

!methodRemoval: Fraction #printOn:showingDecimalPlaces:!
Fraction removeSelector: #printOn:showingDecimalPlaces:!
!methodRemoval: Fraction #printTruncatedOn:showingDecimalPlaces:!
Fraction removeSelector: #printTruncatedOn:showingDecimalPlaces:!

!Fraction reorganize!
('arithmetic' * + - / negated)
('comparing' < <= = hash)
('converting' adaptToInteger:andSend: asFloat asFraction isFraction)
('mathematical' reciprocal)
('mathematical functions' ln log nthRoot: raisedToInteger: sqrt squared)
('printing' printOn: printOn:base: printOn:fractionDigits: storeOn:base:)
('testing' negative)
('truncation and round off' truncated)
('private' denominator numerator reduced setNumerator:denominator:)
!

!methodRemoval: Float #printOn:integerDigits:fractionDigits:positiveIndicator:!
Float removeSelector: #printOn:integerDigits:fractionDigits:positiveIndicator:!
!methodRemoval: Float #printOn:showingDecimalPlaces:!
Float removeSelector: #printOn:showingDecimalPlaces:!
!methodRemoval: Float #printPaddedLeft:decimalPlaces:!
Float removeSelector: #printPaddedLeft:decimalPlaces:!
!methodRemoval: Float #printPaddedWith:to:!
Float removeSelector: #printPaddedWith:to:!

!Float reorganize!
('accessing' at: at:put: basicAt: basicAt:put:)
('arithmetic' * + - / abs negated)
('comparing' < <= = > >= closeTo: hash ~=)
('converting' adaptToFraction:andSend: adaptToInteger:andSend: asFloat asFraction asIEEE32BitWord asTrueFraction degreesToRadians partValues: partValues:ifInfinite:ifNaN: radiansToDegrees withNegativeSign)
('intervals' to:count:do: to:hops:do:)
('mathematical' reciprocal)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh degreeCos degreeSin exp floorLog: ln log nthRoot: reciprocalFloorLog: reciprocalLogBase2 safeArcCos sign: sin sinh sqrt tan tanh timesTwoPower:)
('printing' absByteEncode:base: absPrintExactlyOn:base: absPrintOn:base: hex printOn:base: printOn:fractionDigits: storeOn: storeOn:base:)
('testing' hasContentsInExplorer isFinite isFloat isInfinite isLiteral isNaN isPowerOfTwo isZero sign)
('truncation and round off' exponent exponentPart fractionPart integerPart mantissaPart predecessor reduce rounded signPart significand significandAsInteger successor truncated ulp)
('private' absPrintOn:base:digitCount:)
!

!methodRemoval: Number #printOn:integerDigits:fractionDigits:positiveIndicator:!
Number removeSelector: #printOn:integerDigits:fractionDigits:positiveIndicator:!
!methodRemoval: Number #printOn:showingDecimalPlaces:!
Number removeSelector: #printOn:showingDecimalPlaces:!
!methodRemoval: Number #printShowingDecimalPlaces:!
Number removeSelector: #printShowingDecimalPlaces:!

!Number reorganize!
('arithmetic' * + - / // abs arg negated quo: reciprocal rem: \\)
('comparing' closeTo:)
('converting' adaptToCollection:andSend: adaptToComplex:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asB3DVector3 asComplex asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians hours i milliSeconds minutes nanoSeconds radiansToDegrees seconds weeks withNegativeSign @)
('intervals' to: to:by: to:by:do: to:count: to:do:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh degreeCos degreeSin exp floorLog: interpolateTo:at: ln log log: magnitude nthRoot: raisedTo: raisedToInteger: sign: sin sinh sqrt squared tan tanh)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: storeOn: storeOn:base: storeStringBase:)
('testing' even isDivisibleBy: isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart reduce roundDownTo: roundTo: roundUpTo: rounded truncateTo: truncated)
!


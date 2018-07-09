'From Cuis 5.0 of 7 November 2016 [latest update: #3359] on 5 July 2018 at 4:03:31 pm'!

!Number methodsFor: 'accessing' stamp: 'jmv 7/5/2018 15:47:19'!
imaginary
	"Compatibility with Complex numbers"
	^ 0! !

!Number methodsFor: 'accessing' stamp: 'jmv 7/5/2018 15:47:55'!
real
	"Compatibility with Complex numbers"
	^ self! !


!Number methodsFor: 'accessing' stamp: 'jmv 7/5/2018 15:48:09'!
argument
	"Compatibility with Complex numbers.
	Answer the argument of the receiver (see Complex | argument).
	Answer is in (-Pi .. +Pi]
	Note: Assume the convention of 0+0i having argument=0"

	^self < 0
		ifFalse: [0]
		ifTrue: [Float pi]! !

!Number methodsFor: 'accessing' stamp: 'jmv 7/5/2018 15:48:20'!
magnitude
	"Compatibility with Complex numbers"
	^self abs! !

!Number methodsFor: 'accessing' stamp: 'jmv 7/5/2018 15:48:29'!
phase
	"Compatibility with Complex numbers.
	Answer the argument of the receiver.
	Answer is in (-Pi .. +Pi]
	Note: Assume the convention of 0+0i having argument=0"

	^ self argument! !


!Number reorganize!
('accessing' argument imaginary magnitude phase real)
('arithmetic' * + - / // \\ abs arg div: mod: negated quo: reciprocal rem:)
('comparing' closeTo: isWithin:ulpsFrom:)
('converting' @ adaptToCollection:andSend: adaptToComplex:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asComplex asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours i milliSeconds minutes nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log2 log: nthRoot: raisedTo: raisedToFraction: raisedToInteger: sign: sin sinh sqrt squared tan tanh)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
('testing' even isDivisibleBy: isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart reduce roundDownTo: roundTo: roundUpTo: rounded truncateTo: truncated)
('*JSON-writing' jsonWriteOn:)
!


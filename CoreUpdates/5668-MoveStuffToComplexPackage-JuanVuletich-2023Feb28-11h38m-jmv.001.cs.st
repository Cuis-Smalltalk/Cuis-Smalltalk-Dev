'From Cuis 6.0 [latest update: #5667] on 28 February 2023 at 11:41:58 am'!
!methodRemoval: BoxedFloat64 #arg stamp: 'jmv 2/28/2023 11:38:59'!
BoxedFloat64 removeSelector: #arg!
!methodRemoval: BoxedFloat64 #argument stamp: 'jmv 2/28/2023 11:39:30'!
BoxedFloat64 removeSelector: #argument!
!methodRemoval: Number #arg stamp: 'jmv 2/28/2023 11:39:04'!
Number removeSelector: #arg!
!methodRemoval: Number #phase stamp: 'jmv 2/28/2023 11:39:19'!
Number removeSelector: #phase!
!methodRemoval: Number #argument stamp: 'jmv 2/28/2023 11:39:35'!
Number removeSelector: #argument!
!methodRemoval: Number #imaginary stamp: 'jmv 2/28/2023 11:39:53'!
Number removeSelector: #imaginary!
!methodRemoval: Number #magnitude stamp: 'jmv 2/28/2023 11:40:02'!
Number removeSelector: #magnitude!
!methodRemoval: Number #real stamp: 'jmv 2/28/2023 11:40:47'!
Number removeSelector: #real!

!Number reorganize!
('arithmetic' * + - / // \\ ^ abs div: mod: negated quo: reciprocal rem:)
('comparing' % = hash)
('converting' @ adaptToCollection:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asFloat asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log2 log: nthRoot: raisedTo: raisedToFraction: raisedToInteger: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
('testing' even ifNotZero: is: isDivisibleBy: isFinite isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ truncateTo: truncated)
!


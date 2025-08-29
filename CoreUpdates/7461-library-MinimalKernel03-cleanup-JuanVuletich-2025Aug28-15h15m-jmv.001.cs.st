'From Cuis7.5 [latest update: #7456] on 28 August 2025 at 3:23:14 pm'!

!Number reorganize!
('arithmetic' * + - / // \\ ^ abs div: mod: negated quo: reciprocal rem:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToInteger: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('comparing' % = hash)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
('converting' @ adaptToCollection:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asFloat asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
('testing' even ifNotZero: is: isDivisibleBy: isFinite isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
!


!Float reorganize!
('arithmetic' abs negated)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan: arcTanNonPrimitive copySignTo: cos cosh degreeCos degreeSin expNonPrimitive floorLog: lnNonPrimitive log log: log2 nthRoot: reciprocal reciprocalFloorLog: reciprocalLogBase2 safeArcCos sinNonPrimitive sinh smoothIsAbsBelow: smoothStep sqrt sqrtNonPrimitive tan tanh)
('comparing' hash isWithin:floatsFrom: literalEqual:)
('converting' adaptToFraction:andSend: adaptToInteger:andSend: asFloat asFraction asIEEE32BitPrecisionFloat asIEEE32BitWord asMatching32BitSingleDecimalRepresentation asTrueFraction degreesToRadians inMinusPiToPiRange partBits: partValues: partValues:ifInfinite:ifNaN: radiansToDegrees withNegativeSign)
('intervals' to:count:do: to:hops:do:)
('accessing' at: at:put: basicAt: basicAt:put: byteSize exponent exponentBits exponentPart floatsAwayFrom: integerPart mantissaBits mantissaPart nextAwayFromZero nextTowardsZero predecessor roundedHAFZ roundedHTE sign signBit signPart significand significandAsInteger successor ulp)
('printing' absPrintExactlyOn:base: absPrintOn:base: absPrintOn:base:mantissaSignificantBits: hex printAsIEEE32BitPrecisionFloatOn:base: printAsLiteralOn: printOn:base: printOn:fractionDigits: storeOn:base:)
('testing' isDenormalized isFinite isFloat isFloatOrFloatComplex isInfinite isLiteral isNaN isZero)
('private' absPrintOn:base:digitCount: replaceWordsFrom:to:with:startingAt:)
('inspecting' hasContentsInExplorer)
!


!BoxedFloat64 reorganize!
('arithmetic' * + - /)
('mathematical functions' arcTan cos exp ln primSqrt raisedTo: raisedToInteger: sin timesTwoPower:)
('comparing' < <= = > >= ~=)
('truncation and round off' truncated)
('accessing' exponent fractionPart sign)
('printing' storeOn:base:)
('testing' isDenormalized isFinite isInfinite isNaN)
!


!SmallFloat64 reorganize!
('arithmetic' * + - /)
('mathematical functions' arcTan cos exp ln primSqrt profilerFriendlyPrimTimesTwoPower: profilerFriendlyTimesTwoPower: sin timesTwoPower:)
('comparing' < <= = > >= ~= identityHash)
('truncation and round off' truncated)
('accessing' exponent fractionPart)
('copying' shallowCopy)
!


'From Cuis7.3 [latest update: #7122] on 30 April 2025 at 9:53:24 am'!

!Number reorganize!
('minimalKernel - Bootstrap' // = \\ adaptToInteger:andSend: asInteger ceiling floor is: isNumber isZero negated negative printString raisedToInteger:)
('minimalKernel - Float support' adaptToFloat:andSend:)
('arithmetic' * + - / ^ abs div: mod: quo: reciprocal rem:)
('comparing' % hash)
('converting' @ adaptToCollection:andSend: adaptToFraction:andSend: adaptToPoint:andSend: asFloat asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
('testing' even ifNotZero: isDivisibleBy: isFinite isInfinite isNaN odd positive sign strictlyPositive)
('truncation and round off' detentBy:atMultiplesOf:snap: fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
!


!Float reorganize!
('minimalKernel - Float support' absPrintExactlyOn:base: absPrintOn:base: adaptToInteger:andSend: basicAt: exponentPart isInfinite isNaN isZero mantissaPart negated nextTowardsZero partBits: partValues: partValues:ifInfinite:ifNaN: printOn:base: reciprocalLogBase2 signPart significandAsInteger)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan: arcTanNonPrimitive copySignTo: cos cosh degreeCos degreeSin expNonPrimitive floorLog: lnNonPrimitive log log2 log: nthRoot: reciprocal reciprocalFloorLog: safeArcCos sinNonPrimitive sinh smoothIsAbsBelow: smoothStep sqrt sqrtNonPrimitive tan tanh)
('converting' adaptToFraction:andSend: asFloat asFraction asIEEE32BitPrecisionFloat asIEEE32BitWord asMatching32BitSingleDecimalRepresentation asTrueFraction degreesToRadians inMinusPiToPiRange radiansToDegrees withNegativeSign)
('truncation and round off' exponent exponentBits floatsAwayFrom: integerPart mantissaBits nextAwayFromZero predecessor roundedHAFZ roundedHTE signBit significand successor ulp)
('testing' isDenormalized isFinite isFloat isFloatOrFloatComplex isLiteral sign)
('arithmetic' abs)
('accessing' at: at:put: basicAt:put: byteSize)
('printing' absPrintOn:base:mantissaSignificantBits: hex printAsIEEE32BitPrecisionFloatOn:base: printAsLiteralOn: printOn:fractionDigits: storeOn:base:)
('intervals' to:count:do: to:hops:do:)
('private' absPrintOn:base:digitCount: replaceWordsFrom:to:with:startingAt:)
('comparing' hash isWithin:floatsFrom: literalEqual:)
('inspecting' hasContentsInExplorer)
!


!Float class reorganize!
('minimalKernel - Float support' denormalized fmin fminDenormalized initClassCachedState initialize one pi signPart:mantissaPart:exponentPart:)
('instance creation' basicNew basicNew: fromIEEE32Bit: new new: readFrom: signBit:mantissaBits:exponentBits:)
('constants' e emax emin epsilon fmax fminNormalized halfPi infinity ln10 ln2 maxExactInteger nan negativeInfinity negativeZero precision radix twoPi zero)
('class initialization' releaseClassCachedState)
('testing' nativeWordOrdering)
!


!BoxedFloat64 reorganize!
('minimalKernel - Float support' * + - / < <= = > >= ~= exponent ln timesTwoPower: truncated)
('arithmetic')
('comparing')
('mathematical functions' arcTan cos exp primSqrt raisedTo: raisedToInteger: sin)
('truncation and round off' fractionPart)
('testing' isDenormalized isFinite isInfinite isNaN sign)
('printing' storeOn:base:)
!


!SmallFloat64 reorganize!
('minimalKernel - Float support' * + - / < <= = > >= ~= exponent identityHash ln timesTwoPower: truncated)
('arithmetic')
('comparing')
('mathematical functions' arcTan cos exp primSqrt profilerFriendlyPrimTimesTwoPower: profilerFriendlyTimesTwoPower: sin)
('truncation and round off' fractionPart)
('copying' shallowCopy)
!


!SmallInteger reorganize!
('minimalKernel - Bootstrap' * + - // < <= = > >= \\ ~= bitAnd: bitOr: bitShift: bitXor: decimalDigitLength even identityHash lowBit printString quo:)
('minimalKernel - Float support' asFloat)
('arithmetic' / gcd:)
('bit manipulation' byteReversed hashMultiply)
('testing' isLarge odd)
('copying' shallowCopy)
('printing' numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
!


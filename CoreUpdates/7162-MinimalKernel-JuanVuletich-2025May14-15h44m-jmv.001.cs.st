'From Cuis7.3 [latest update: #7160] on 14 May 2025 at 3:57:28 pm'!

!Float reorganize!
('[mkFloat] arithmetic' abs negated)
('[mkFloat] comparing' hash)
('[mkFloat] mathematical functions' reciprocalLogBase2)
('[mkFloat] converting' adaptToInteger:andSend: partBits: partValues: partValues:ifInfinite:ifNaN:)
('[mkFloat] testing' isDenormalized isFinite isInfinite isNaN isZero)
('[mkFloat] accessing' at: at:put: basicAt: basicAt:put: exponentPart mantissaPart nextTowardsZero sign signPart significandAsInteger)
('[mkFloat] printing' absPrintExactlyOn:base: absPrintOn:base: printOn:base:)
('comparing' isWithin:floatsFrom: literalEqual:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan: arcTanNonPrimitive copySignTo: cos cosh degreeCos degreeSin expNonPrimitive floorLog: lnNonPrimitive log log: log2 nthRoot: reciprocal reciprocalFloorLog: safeArcCos sinNonPrimitive sinh smoothIsAbsBelow: smoothStep sqrt sqrtNonPrimitive tan tanh)
('converting' adaptToFraction:andSend: asFloat asFraction asIEEE32BitPrecisionFloat asIEEE32BitWord asMatching32BitSingleDecimalRepresentation asTrueFraction degreesToRadians inMinusPiToPiRange radiansToDegrees withNegativeSign)
('testing' isFloat isFloatOrFloatComplex isLiteral)
('accessing' byteSize exponent exponentBits floatsAwayFrom: integerPart mantissaBits nextAwayFromZero predecessor roundedHAFZ roundedHTE signBit significand successor ulp)
('printing' absPrintOn:base:mantissaSignificantBits: hex printAsIEEE32BitPrecisionFloatOn:base: printAsLiteralOn: printOn:fractionDigits: storeOn:base:)
('intervals' to:count:do: to:hops:do:)
('private' absPrintOn:base:digitCount: replaceWordsFrom:to:with:startingAt:)
('inspecting' hasContentsInExplorer)
!


!Float class reorganize!
('[mkFloat] instance creation' basicNew basicNew: new new: signPart:mantissaPart:exponentPart:)
('[mkFloat] constants' fmin fminDenormalized maxExactInteger one pi precision)
('[mkFloat] class initialization' initClassCachedState initialize)
('[mkFloat] testing' denormalized)
('instance creation' fromIEEE32Bit: readFrom: signBit:mantissaBits:exponentBits:)
('constants' e emax emin epsilon fmax fminNormalized halfPi infinity ln10 ln2 nan negativeInfinity negativeZero radix twoPi zero)
('class initialization' releaseClassCachedState)
('testing' nativeWordOrdering)
!


!BoxedFloat64 reorganize!
('[mkFloat] arithmetic' * + - /)
('[mkFloat] comparing' < <= = > >= ~=)
('[mkFloat] mathematical functions' ln raisedToInteger: timesTwoPower:)
('[mkFloat] testing' isDenormalized isFinite isInfinite isNaN)
('[mkFloat] truncation and round off' truncated)
('[mkFloat] accessing' exponent sign)
('mathematical functions' arcTan cos exp primSqrt raisedTo: sin)
('accessing and round off' fractionPart)
('printing' storeOn:base:)
!


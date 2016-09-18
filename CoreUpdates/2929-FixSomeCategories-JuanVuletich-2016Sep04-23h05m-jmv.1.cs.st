'From Cuis 4.2 of 25 July 2013 [latest update: #2928] on 4 September 2016 at 11:11:14 pm'!

!Number reorganize!
('arithmetic' * + - / // \\ abs arg div: mod: negated quo: reciprocal rem:)
('comparing' closeTo: isWithin:ulpsFrom:)
('converting' @ adaptToCollection:andSend: adaptToComplex:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asComplex asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours i milliSeconds minutes nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: ln log log2 log: magnitude nthRoot: raisedTo: raisedToInteger: sign: sin sinh sqrt squared tan tanh)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase:)
('testing' even isDivisibleBy: isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart reduce roundDownTo: roundTo: roundUpTo: rounded truncateTo: truncated)
('squeak compatibility')
!


!Integer reorganize!
('arithmetic' * + - / // \\\ alignedTo: crossSumBase: quo: reciprocalModulo:)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: highBit highBitOfMagnitude lowBit noMask:)
('comparing' < <= = > >= hash)
('converting' adaptToFraction:andSend: asCharacter asColorOfDepth: asFloat asFraction asHexDigit asInteger asIntegerOrFloat)
('enumerating' timesRepeat:)
('inspecting' explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial floatLog gcd: ifMultipleOf2And5Do:otherwise: lcm: ln log log2 log: nthRoot: nthRootTruncated: productTo: raisedTo:modulo: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even isInteger isPowerOfTwo isPrime isProbablyPrime nextPrime)
('tiles')
('truncation and round off' atRandom atRandom: ceiling floor normalize rounded truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
('printing-numerative')
!


!LargePositiveInteger reorganize!
('arithmetic' * + - / // \\ \\\ abs negated quo:)
('bit manipulation' bitAt: bitReverse: hashMultiply highBit highBitOfMagnitude)
('comparing' < <= > >= hash)
('converting' asFloat normalize withAtLeastNDigits:)
('mathematical functions' mightBeASquare sqrt)
('printing' printOn:base: printOn:base:nDigits:)
('system primitives' digitAt: digitAt:put: digitLength replaceFrom:to:with:startingAt:)
('testing' negative positive sign strictlyPositive)
('enumerating' timesRepeat:)
!


!SmallInteger reorganize!
('arithmetic' * + - / // \\ gcd: quo:)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed hashMultiply lowBit)
('testing' even odd)
('comparing' < <= = > >= ~= identityHash)
('copying' shallowCopy)
('converting' asFloat)
('printing' decimalDigitLength numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength instVarAt: nextInstance nextObject)
('private')
('mathematical functions' sqrt)
!


!Duration reorganize!
('public protocol' totalMilliSeconds totalMilliseconds totalMinutes totalNanoSeconds totalSeconds)
('ansi protocol' * + - / < = abs days hash hours minutes negated negative positive seconds)
('squeak protocol' // \\ asDelay isZero nanoSeconds printOn: roundTo: truncateTo:)
('private' seconds:nanoSeconds: storeOn:)
('initialization' initialize)
!


!Collection reorganize!
('accessing' anyOne atRandom size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = hash)
('converting' asArray asBag asByteArray asCharacterSet asFloatArray asIdentitySet asIntegerArray asNewArray asOrderedCollection asSet asSortedArray asSortedCollection asSortedCollection: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: detectSum: difference: do: do:separatedBy: do:without: explorerContents explorerContentsWithIndexCollect: fold: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionToSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species toBraceStack:)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted:)
('statistics' average max max: mean median min min: product product: range sampleStandardDeviation sampleVariance standardDeviation sum sum: variance)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

SystemOrganization  removeEmptyCategories!


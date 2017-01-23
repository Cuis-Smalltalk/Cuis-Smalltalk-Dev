'From Cuis 5.0 of 7 November 2016 [latest update: #3042] on 22 January 2017 at 9:49:56 pm'!

!Integer methodsFor: 'arithmetic' stamp: 'len 12/4/2016 13:11:44'!
reciprocalModulo2: n
	"Answer an integer x such that self * x \\ n = 1, with 0 < x < n, or nil if it doesn't exist."
	| xgcd |
	self == 0 ifTrue: [^ nil].
	self == 1 ifTrue: [^ 1].
	xgcd _ self xgcd: n.
	^ (xgcd at: 1) == 1 ifTrue: [^ (xgcd at: 2) \\ n]! !

!Integer methodsFor: 'arithmetic' stamp: 'len 12/4/2016 13:07:08'!
xgcd: anInteger
	"Extended Euclidean algorithm.
	Answer an array {x. u. v} where self * u + (anInteger * v) = x, and x = (self gcd: anInteger)."
	| a b s t sp tp r rp |
	a _ self. b _ anInteger.
	s _ 0. sp _ 1.
	t _ 1. tp _ 0.
	r _ a abs. rp _ b abs.
	[r == 0]
		whileFalse:
			[ | q temp |
			q _ rp // r.
			temp _ r. r _ rp - (q * r). rp _ temp.
			temp _ s. s _ sp - (q * s). sp _ temp.
			temp _ t. t _ tp - (q * t). tp _ temp].
	sp _ sp * b sign. tp _ tp * a sign.
	^ {rp. tp. sp}! !


!Integer reorganize!
('arithmetic' * + - / // \\\ alignedTo: crossSumBase: gcd: quo: reciprocalModulo2: reciprocalModulo: xgcd:)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: highBit highBitOfMagnitude lowBit noMask:)
('comparing' < <= = > >= hash)
('converting' adaptToFraction:andSend: asCharacter asColorOfDepth: asFloat asFraction asHexDigit asInteger asIntegerOrFloat)
('enumerating' timesRepeat:)
('inspecting' explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial floatLog ifMultipleOf2And5Do:otherwise: lcm: ln log log2 log: nthRoot: nthRootRounded: nthRootTruncated: productTo: raisedTo:modulo: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even isInteger isPowerOfTwo isPrime isProbablyPrime nextPrime)
('tiles')
('truncation and round off' atRandom atRandom: ceiling floor normalize rounded truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryDigitBase montgomeryDigitLength montgomeryDigitMax montgomeryNumberOfDigits montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
('printing-numerative')
!


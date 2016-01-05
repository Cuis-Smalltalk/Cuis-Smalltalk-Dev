'From Cuis 4.2 of 25 July 2013 [latest update: #2652] on 5 January 2016 at 11:50:53.346971 am'!

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 1/5/2016 11:37'!
new: size from: aSequenceableCollection
	"Create a new collection containing all the elements from aSequenceableCollection.
	FloatArray new: 4 from: { 1. 2 }
	FloatArray new: 2 from: { 1. 2. 3. 4 }
	"

	^ (self new: size)
		replaceFrom: 1
		to: (aSequenceableCollection size min: size)
		with: aSequenceableCollection
		startingAt: 1! !


!Float methodsFor: 'private' stamp: 'jmv 1/5/2016 11:10'!
replaceWordsFrom: start to: stop with: replacement startingAt: repStart
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	self primitiveFailed! !


!Float reorganize!
('accessing' at: at:put: basicAt: basicAt:put: byteSize)
('arithmetic' * + - / abs negated)
('comparing' < <= = > >= closeTo: hash ~=)
('converting' adaptToFraction:andSend: adaptToInteger:andSend: asFloat asFraction asIEEE32BitPrecisionFloat asIEEE32BitWord asTrueFraction degreesToRadians partValues: partValues:ifInfinite:ifNaN: radiansToDegrees withNegativeSign)
('intervals' to:count:do: to:hops:do:)
('mathematical' reciprocal)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh degreeCos degreeSin exp floorLog: ln log nthRoot: reciprocalFloorLog: reciprocalLogBase2 safeArcCos sign: sin sinh sqrt tan tanh timesTwoPower:)
('printing' absByteEncode:base: absPrintExactlyOn:base: absPrintOn:base: hex printOn:base: printOn:fractionDigits: storeOn:base:)
('testing' hasContentsInExplorer isFinite isFloat isInfinite isLiteral isNaN isZero sign)
('truncation and round off' exponent exponentPart fractionPart integerPart mantissaPart predecessor reduce rounded signPart significand significandAsInteger successor truncated ulp)
('private' absPrintOn:base:digitCount: replaceWordsFrom:to:with:startingAt:)
('*OpenCL' asOpenCLArg)
!


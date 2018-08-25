'From Cuis 5.0 of 7 November 2016 [latest update: #3429] on 23 August 2018 at 4:37:09 pm'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/23/2018 09:45:40'!
smoothStep
	"A Function that is
		x <= -1 		-> 		1
		-1 < x < 1 	->		smooth differentiable transition from 1 to 0
		1 <= x 		-> 		0
	
	It is a 'smooth' version of an #negative
	Has countless applications. For example in image and signal processing, but also in other fields."
	"
	| g |
	Feature require: 'Morphic-Widgets-Extras'.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < 1 ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothStep ] color: Color red.
	(g embeddedInMorphicWindowLabeled: 'graph') openInWorld
	"
	self > -1 ifFalse: [ ^ 1 ].
	1 > self ifFalse: [ ^ 0 ].
	^ (self +2 * Float halfPi) sin * 0.5 + 0.5! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/23/2018 09:30:56'!
reciprocal
	^ 1.0 / self! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 8/23/2018 09:32:01'!
smoothIsAbsBelow: threshold
	"A Function that is 1 at zero, 0 outside a bounded interval of size 4*threshold, and is continuous and differentiable.
	
	It is a 'smooth' version of an #isAbsBelow: function: x abs < threshold.
	Useful when we need to 'count' stuff, but still want continuous and differentiable stuff."
	"
	| g |
	Feature require: 'Morphic-Widgets-Extras'.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < Float halfPi ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothIsAbsBelow: Float halfPi ] color: Color red.
	(g embeddedInMorphicWindowLabeled: 'graph') openInWorld
	"
	| scaled |
	scaled _ self / threshold.
	^ scaled abs < 2 ifTrue: [ (scaled * Float halfPi) cos + 1.0 * 0.5 ] ifFalse: [ 0 ]! !


!Float reorganize!
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan: copySignTo: cos cosh degreeCos degreeSin floorLog: log log2 log: nthRoot: reciprocal reciprocalFloorLog: reciprocalLogBase2 safeArcCos sign: sinh smoothIsAbsBelow: smoothStep sqrt tan tanh)
('converting' adaptToFraction:andSend: adaptToInteger:andSend: asFloat asFraction asIEEE32BitPrecisionFloat asIEEE32BitWord asTrueFraction degreesToRadians inMinusPiToPiRange partValues: partValues:ifInfinite:ifNaN: radiansToDegrees withNegativeSign)
('truncation and round off' exponentPart integerPart mantissaPart predecessor reduce rounded signPart significand significandAsInteger successor ulp)
('testing' hasContentsInExplorer isFinite isFloat isInfinite isLiteral isNaN isZero sign)
('arithmetic' abs negated)
('accessing' at: at:put: basicAt: basicAt:put: byteSize)
('printing' absByteEncode:base: absPrintExactlyOn:base: absPrintOn:base: absPrintOn:base:mantissaSignificantBits: hex printAsIEEE32BitPrecisionFloatOn:base: printOn:base: printOn:fractionDigits: storeOn:base:)
('intervals' to:count:do: to:hops:do:)
('private' absPrintOn:base:digitCount: replaceWordsFrom:to:with:startingAt:)
('comparing' closeTo: hash isWithin:ulpsFrom:)
!


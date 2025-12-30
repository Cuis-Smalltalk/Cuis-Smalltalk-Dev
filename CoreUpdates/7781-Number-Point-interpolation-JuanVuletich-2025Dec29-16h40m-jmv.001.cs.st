'From Cuis7.7 [latest update: #7781] on 29 December 2025 at 4:48:01 pm'!

!Point methodsFor: 'interpolating' stamp: 'jmv 29/Dec/2025 16:46:21'!
advanceFractionOf: partial towards: end
	"The inverse of #interpolateTo:at:
	Answer 0 means partial = self
	Answer 1 means partial = end
	Answer between 0 and 1 means partial is between self and end"
	
	^partial - self / (end - self)! !


!Number methodsFor: 'interpolation' stamp: 'jmv 29/Dec/2025 16:46:15'!
advanceFractionOf: partial towards: end
	"The inverse of #interpolateTo:at:
	Answer 0 means partial = self
	Answer 1 means partial = end
	Answer between 0 and 1 means partial is between self and end"
	
	^partial - self / (end - self)! !

!Number methodsFor: 'interpolation' stamp: 'ar 31/Aug/2000 20:05:00'!
interpolateTo: end at: fraction
	^self + ((end - self) * fraction)! !


!Point methodsFor: 'interpolating' stamp: 'jsp 22/Mar/1999 16:31:00'!
interpolateTo: end at: fraction
	"Interpolate between the instance and end after the specified amount has been done (0 - 1)."

	^ self + ((end - self) * fraction).! !


!Number reorganize!
('arithmetic' * + - / // \\ ^ abs div: mod: negated quo: reciprocal rem:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToInteger: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('interpolation' advanceFractionOf:towards: interpolateTo:at:)
('comparing' % = hash)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: floor fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
('converting' @ adaptToCollection:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asFloat asInteger asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
('testing' even ifNotZero: is: isDivisibleBy: isFinite isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
!


!Point reorganize!
('accessing' x y)
('arithmetic' * + - / // \\ abs)
('comparing' < <= = > >= between:and: hash max: min: min:max:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: asFloatPoint asIntegerPoint asPoint corner: extent: isPoint rect:)
('geometry' barycentricCoordinatesInTriangle:with:with:into:ifDegenerate: isInsideCircle:with:with: sideOf: to:intersects:to: to:sideOf: triangleArea:with:)
('interpolating' advanceFractionOf:towards: interpolateTo:at:)
('point functions' crossProduct: dist: dotProduct: eightNeighbors flipBy:centerAt: fourNeighbors insideTriangle:with:with: nearestPointAlongLineFrom:to: nearestPointOnLineFrom:to: normal normalized onLineFrom:to: onLineFrom:to:within: quadrantOf: squaredDistanceTo: transposed)
('polar coordinates' degrees magnitude r rho theta)
('printing' printOn: printStringFractionDigits: storeOn:)
('private - initialization' initializePvtX:y:)
('transforming' adhereTo: inverseRotatedBy:about: negated rotatedBy: scaledBy: translatedBy:)
('truncation and round off' ceiling floor rounded roundedHAFZ roundedToEven truncateTo: truncated)
('testing' isIntegerPoint isZero)
('copying' shallowCopy)
!


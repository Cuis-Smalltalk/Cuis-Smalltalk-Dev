'From Cuis 5.0 of 7 November 2016 [latest update: #3799] on 12 June 2019 at 10:02:22 am'!

!Point methodsFor: 'private - initialization' stamp: 'jmv 6/12/2019 09:55:41'!
pvtInitializeX: xValue y: yValue
	"Points are immutable."
	x _ xValue.
	y _ yValue! !


!Point class methodsFor: 'instance creation' stamp: 'jmv 6/12/2019 09:55:51'!
x: anX y: anY
	"Answer an instance of me with supplied coordinates."

	^self new pvtInitializeX: anX y: anY! !

!methodRemoval: Point #privateSetX:setY:!
Point removeSelector: #privateSetX:setY:!

!Point reorganize!
('accessing' x y)
('arithmetic' * + - / // \\ abs)
('comparing' < <= = > >= between:and: hash max: min: min:max:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: asFloatPoint asIntegerPoint asPoint corner: enclosingRectangleWith: extent: isPoint rect:)
('geometry' barycentricCoordinatesInTriangle:with:with:into:ifDegenerate: isInsideCircle:with:with: sideOf: to:intersects:to: to:sideOf: triangleArea:with:)
('interpolating' interpolateTo:at:)
('point functions' crossProduct: dist: dotProduct: eightNeighbors flipBy:centerAt: fourNeighbors insideTriangle:with:with: nearestPointAlongLineFrom:to: nearestPointOnLineFrom:to: normal normalized onLineFrom:to: onLineFrom:to:within: quadrantOf: squaredDistanceTo: transposed)
('polar coordinates' degrees magnitude r rho theta)
('printing' printOn: printStringFractionDigits: storeOn:)
('private - initialization' pvtInitializeX:y:)
('transforming' adhereTo: inverseRotatedBy:about: negated rotatedBy: scaledBy: translatedBy:)
('truncation and round off' ceiling floor rounded truncateTo: truncated)
('testing' isZero)
('copying' shallowCopy)
!


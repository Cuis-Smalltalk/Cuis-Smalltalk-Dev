'From Cuis 4.2 of 25 July 2013 [latest update: #2940] on 29 October 2016 at 5:48:41 pm'!
!classDefinition: #Point category: #'Graphics-Primitives'!
Object subclass: #Point
	instanceVariableNames: 'x y '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Point methodsFor: 'copying' stamp: 'pb 10/29/2016 18:17:53'!
copy
	"Immutable"
	^ self.! !

!Point methodsFor: 'copying' stamp: 'pb 10/29/2016 18:17:59'!
copyFrom: anotherObject
	"Immutable"
	self shouldNotImplement.! !

!Point methodsFor: 'copying' stamp: 'pb 10/29/2016 18:18:03'!
copySameFrom: otherObject
	"Immutable"
	self shouldNotImplement.! !

!Point methodsFor: 'copying' stamp: 'pb 10/29/2016 18:18:07'!
shallowCopy
	"Immutable"
	^ self.! !

!Point methodsFor: 'copying' stamp: 'pb 10/29/2016 18:18:14'!
veryDeepCopy
	"Immutable"
	^ self.! !

!Point class methodsFor: 'instance creation' stamp: 'pb 10/29/2016 17:14:00'!
r: rho degrees: degrees
	"Answer an instance of me with polar coordinates rho and theta."
	^ self
		rho: rho
		theta: degrees asFloat degreesToRadians.! !

!Point class methodsFor: 'instance creation' stamp: 'pb 10/29/2016 17:12:53'!
rho: rho theta: radians
	"Answer an instance of me with polar coordinates rho and theta."
	^ self
		x: rho asFloat * radians cos
		y: rho asFloat * radians sin.! !

!Point class methodsFor: 'instance creation' stamp: 'pb 10/29/2016 16:34:28'!
x: anX y: anY
	"Answer an instance of me with supplied coordinates."
	^ self new
		
			instVarNamed: 'x'
			put: anX;
		
			instVarNamed: 'y'
			put: anY;
		yourself.! !

!methodRemoval: Point #setR:degrees:!
Point removeSelector: #setR:degrees:!
!methodRemoval: Point #setRho:theta:!
Point removeSelector: #setRho:theta:!
!methodRemoval: Point #setX:setY:!
Point removeSelector: #setX:setY:!
!classDefinition: #Point category: #'Graphics-Primitives'!
Object subclass: #Point
	instanceVariableNames: 'x y'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Point reorganize!
('accessing' x y)
('arithmetic' * + - / // \\ abs)
('comparing' < <= = > >= between:and: hash max: min: min:max:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: asFloatPoint asIntegerPoint asPoint corner: enclosingRectangleWith: extent: isPoint rect:)
('geometry' barycentricCoordinatesInTriangle:with:with:into:ifDegenerate: isInsideCircle:with:with: sideOf: to:intersects:to: to:sideOf: triangleArea:with:)
('interpolating' interpolateTo:at:)
('point functions' crossProduct: dist: dotProduct: eightNeighbors flipBy:centerAt: fourNeighbors insideTriangle:with:with: nearestPointAlongLineFrom:to: nearestPointOnLineFrom:to: normal normalized onLineFrom:to: onLineFrom:to:within: quadrantOf: squaredDistanceTo: transposed)
('polar coordinates' degrees magnitude r rho theta)
('printing' printOn: storeOn:)
('private')
('transforming' adhereTo: inverseRotatedBy:about: negated rotatedBy: scaledBy: translatedBy:)
('truncation and round off' ceiling floor rounded truncateTo: truncated)
('testing' isZero)
('copying' copy copyFrom: copySameFrom: shallowCopy veryDeepCopy)
!

!Point class reorganize!
('instance creation' fromUser fromUserWithCursor: r:degrees: rho:theta: x:y:)
!


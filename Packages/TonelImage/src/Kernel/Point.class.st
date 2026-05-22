"
I represent an x-y pair of numbers usually designating a location on the screen.

When dealing with display coordinates, the y axis is usually considered to increase downwards. However, the standard math convention is to consider it increasing upwards. 
Points don't need to know about this. In the first case, theta increases clockwise. In the second case, it increases counter-clockwise, also the standard math convention.

Any method that doesn't follow this (because it assumes one specific convention) include this fact in the selector and in a comment.

My instances are immutable. See #privateSetX:setY:
"
Class {
	#name : 'Point',
	#superclass : 'Object',
	#instVars : [
		'x',
		'y'
	],
	#category : 'Kernel-Support',
	#package : 'Kernel',
	#tag : 'Support'
}

{ #category : 'instance creation' }
Point class >> fromUser [
	Sensor waitNoButton.
	(Cursor cursorAt: #crossHairCursor) activateCursor.
	Sensor waitButton.
	Cursor defaultCursor activateCursor.
	^ Sensor mousePoint"Point fromUser".
]

{ #category : 'instance creation' }
Point class >> fromUserWithCursor: aCursor [
	Sensor waitNoButton .
	aCursor showWhile: [ Sensor waitButton ] .
	^ Sensor mousePoint

"Point fromUserWithCursor: Cursor target"
"Point fromUserWithCursor: Cursor crossHair"

]

{ #category : 'instance creation' }
Point class >> r: rho degrees: degrees [
	"Answer an instance of me with polar coordinates rho and theta."
	^ self
		rho: rho
		theta: degrees asFloat degreesToRadians.
]

{ #category : 'instance creation' }
Point class >> rho: rho theta: radians [
	"Answer an instance of me with polar coordinates rho and theta."
	^ self
		x: rho asFloat * radians cos
		y: rho asFloat * radians sin.
]

{ #category : 'instance creation' }
Point class >> x: anX y: anY [
	"Answer an instance of me with supplied coordinates."

	^self new initializePvtX: anX y: anY
]

{ #category : 'arithmetic' }
Point >> * arg  [
	"Answer a Point that is the product of the receiver and arg."

	arg isPoint ifTrue: [^ (x * arg x) @ (y * arg y)].
	^ arg adaptToPoint: self andSend: #*
]

{ #category : 'arithmetic' }
Point >> + arg  [
	"Answer a Point that is the sum of the receiver and arg."

	arg isPoint ifTrue: [^ (x + arg x) @ (y + arg y)].
	^ arg adaptToPoint: self andSend: #+
]

{ #category : 'arithmetic' }
Point >> - arg  [
	"Answer a Point that is the difference of the receiver and arg."

	arg isPoint ifTrue: [^ (x - arg x) @ (y - arg y)].
	^ arg adaptToPoint: self andSend: #-
]

{ #category : 'arithmetic' }
Point >> / arg  [
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint ifTrue: [^ (x / arg x) @ (y / arg y)].
	^ arg adaptToPoint: self andSend: #/
]

{ #category : 'arithmetic' }
Point >> // arg  [
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint ifTrue: [^ (x // arg x) @ (y // arg y)].
	^ arg adaptToPoint: self andSend: #//
]

{ #category : 'comparing' }
Point >> < aPoint  [
	"Answer whether the receiver is 'above and to the left' of aPoint."
	^x < aPoint x and: [y < aPoint y]
]

{ #category : 'comparing' }
Point >> <= aPoint  [
	"Answer whether the receiver is 'neither below nor to the right' of aPoint."

	^x <= aPoint x and: [y <= aPoint y]
]

{ #category : 'comparing' }
Point >> = aPoint [

	self == aPoint ifTrue: [ ^ true ].
	self species == aPoint species ifFalse: [ ^ false ].

	^ x = aPoint x and: [ y = aPoint y ]
]

{ #category : 'comparing' }
Point >> > aPoint  [
	"Answer whether the receiver is 'below and to the right' of aPoint."

	^x > aPoint x and: [y > aPoint y]
]

{ #category : 'comparing' }
Point >> >= aPoint  [
	"Answer whether the receiver is 'neither above nor to the left' of aPoint."

	^x >= aPoint x and: [y >= aPoint y]
]

{ #category : 'arithmetic' }
Point >> \\ arg  [
	"Answer a Point that is the mod of the receiver and arg."

	arg isPoint ifTrue: [^ (x \\ arg x) @ (y \\ arg y)].
	^ arg adaptToPoint: self andSend: #\\
]

{ #category : 'arithmetic' }
Point >> abs [
	"Answer a Point whose x and y are the absolute values of the receiver's x 
	and y."

	^ x abs @ y abs
]

{ #category : 'converting' }
Point >> adaptToCollection: rcvr andSend: selector [
	"If I am involved in arithmetic with a Collection, return a Collection of
	the results of each element combined with me in that expression."

	^ rcvr collect: [:element | element perform: selector with: self]
]

{ #category : 'converting' }
Point >> adaptToNumber: rcvr andSend: selector [
	"If I am involved in arithmetic with an Integer, convert it to a Point."
	^ rcvr@rcvr perform: selector with: self
]

{ #category : 'transforming' }
Point >> adhereTo: aRectangle [
	"If the receiver lies outside aRectangle, return the nearest point on the boundary of the rectangle, otherwise return self."

	(aRectangle containsPoint: self) ifTrue: [^ self].
	^ ((x max: aRectangle left) min: aRectangle right)
		@ ((y max: aRectangle top) min: aRectangle bottom)
]

{ #category : 'converting' }
Point >> asFloatPoint [
	^ x asFloat @ y asFloat
]

{ #category : 'converting' }
Point >> asIntegerPoint [
	^ x asInteger @ y asInteger
]

{ #category : 'converting' }
Point >> asPoint [
	"Answer the receiver itself."
	^self
]

{ #category : 'geometry' }
Point >> barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: aBlock ifDegenerate: degeneratBlock [
	"Return  the barycentric coordinates for the receiver within the triangle defined by the three arguments."
	| p0 b0 b1 b2 b3 |
	p0 := self.
	b0 := ((p2 x - p1 x) * (p3 y - p1 y)) - ((p3 x - p1 x) * (p2 y - p1 y)).
	b0 isZero ifTrue: [ ^ degeneratBlock value ]. "degenerate"
	b0 := 1.0 / b0.
	b1 := (((p2 x - p0 x) * (p3 y - p0 y)) - ((p3 x - p0 x) * (p2 y - p0 y))) * b0.
	b2 := (((p3 x - p0 x) * (p1 y - p0 y)) - ((p1 x - p0 x) * (p3 y - p0 y))) * b0.
	b3 := (((p1 x - p0 x) * (p2 y - p0 y)) - ((p2 x - p0 x) * (p1 y - p0 y))) * b0.
	^aBlock value: b1 value: b2 value: b3
]

{ #category : 'comparing' }
Point >> between: min and: max  [
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]
]

{ #category : 'truncation and round off' }
Point >> ceiling [
	"Answer a Point that is the receiver's x and y ceiling. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x ceiling @ y ceiling

]

{ #category : 'converting' }
Point >> corner: aPoint  [
	"Answer a new Rectangle whose origin is the receiver and whose corner is aPoint.
	This is one of the infix ways of expressing the creation of a rectangle."

	^Rectangle origin: self corner: aPoint
]

{ #category : 'point functions' }
Point >> crossProduct: aPoint  [
	"Answer a number that is the cross product of the receiver and the 
	argument, aPoint."

	^ (x * aPoint y) - (y * aPoint x)
]

{ #category : 'polar coordinates' }
Point >> degrees [
	"Answer the angle the receiver makes with origin in radians.
	If coordinates are in Display convention (y increases downwards), angles increase clockwise.
	If coordinates are in math convention (y increases upwards), angles increase counter-clockwise "
	^self theta radiansToDegrees
]

{ #category : 'point functions' }
Point >> dist: aPoint  [
	"Answer the distance between aPoint and the receiver."

	^(aPoint - self) r
]

{ #category : 'point functions' }
Point >> dotProduct: aPoint  [
	"Answer a number that is the dot product of the receiver and the 
	argument, aPoint. That is, the two points are multipled and the 
	coordinates of the result summed."

	^ (x * aPoint x) + (y * aPoint y)
]

{ #category : 'point functions' }
Point >> eightNeighbors [
	^ (Array with: self + `1@0`
		with: self + `1@1`
		with: self + `0@1`
		with: self + `-1@1`) ,
	(Array with: self + `-1@0`
		with: self + `-1@ -1`
		with: self + `0@ -1`
		with: self + `1@ -1`)
]

{ #category : 'converting' }
Point >> extent: aPoint  [
	"Answer a new Rectangle whose origin is the receiver and whose extent is aPoint. 
	This is one of the infix ways of expressing the creation of a rectangle."

	^Rectangle origin: self extent: aPoint
]

{ #category : 'point functions' }
Point >> flipBy: direction centerAt: c [
	"Answer a Point which is flipped according to the direction about the point c.
	Direction must be #vertical or #horizontal."
	direction == #vertical ifTrue: [^ x @ (c y * 2 - y)].
	direction == #horizontal ifTrue: [^ (c x * 2 - x) @ y].
	self error: 'unrecognizable direction'
]

{ #category : 'truncation and round off' }
Point >> floor [
	"Answer a Point that is the receiver's x and y floor. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x floor @ y floor

]

{ #category : 'point functions' }
Point >> fourNeighbors [
	^ Array with: self + `1@0`
		with: self + `0@1`
		with: self + `-1@0`
		with: self + `0@ -1`
]

{ #category : 'comparing' }
Point >> hash [
	"Hash is reimplemented because = is implemented."

	^(x hash hashMultiply + y hash) hashMultiply
]

{ #category : 'private - initialization' }
Point >> initializePvtX: xValue y: yValue [
	"Points are immutable."
	x := xValue.
	y := yValue
]

{ #category : 'point functions' }
Point >> insideTriangle: p1 with: p2 with: p3 [
	"Return true if the receiver is within the triangle defined by the three coordinates.
	Note: This method computes the barycentric coordinates for the receiver and tests those coordinates."
	self
		barycentricCoordinatesInTriangle: p1 with: p2 with: p3 into: [ :b1 :b2 :b3 | 
			b1 < 0.0 ifTrue: [ ^false ].
			b2 < 0.0 ifTrue: [ ^false ].
			b3 < 0.0 ifTrue: [ ^false ].
			^true ]
		ifDegenerate: [ ^false ]
]

{ #category : 'interpolating' }
Point >> interpolateTo: end at: amountDone [
	"Interpolate between the instance and end after the specified amount has been done (0 - 1)."

	^ self + ((end - self) * amountDone).
]

{ #category : 'transforming' }
Point >> inverseRotatedBy: angle about: center [
	"This method considers angle to be in standard math convention (counter clock wise) while at the same time it considers the points to be in display convention (y axis increases downwards)"

	| p r theta |
	p := self - center.
	r := p r.
	theta := angle asFloat - p theta.
	^ (center x asFloat + (r * theta cos)) @
	  (center y asFloat - (r * theta sin))
]

{ #category : 'geometry' }
Point >> isInsideCircle: a with: b with: c  [
	"Returns TRUE if self is inside the circle defined by the     
	points a, b, c. See Guibas and Stolfi (1985) p.107"
	^ (a dotProduct: a)
		* (b triangleArea: c with: self) - ((b dotProduct: b)
			* (a triangleArea: c with: self)) + ((c dotProduct: c)
			* (a triangleArea: b with: self)) - ((self dotProduct: self)
			* (a triangleArea: b with: c)) > 0.0
]

{ #category : 'testing' }
Point >> isIntegerPoint [
	^x isInteger and:[y isInteger]
]

{ #category : 'converting' }
Point >> isPoint [
	^ true
]

{ #category : 'testing' }
Point >> isZero [
	^x isZero and:[y isZero]
]

{ #category : 'polar coordinates' }
Point >> magnitude [
	^self r
]

{ #category : 'comparing' }
Point >> max: aPoint  [
	"Answer the lower right corner of the rectangle uniquely defined by the 
	receiver and the argument, aPoint."

	^ (x max: aPoint x) @ (y max: aPoint y)
]

{ #category : 'comparing' }
Point >> min: aPoint  [
	"Answer the upper left corner of the rectangle uniquely defined by the 
	receiver and the argument, aPoint."

	^ (x min: aPoint x) @ (y min: aPoint y)
]

{ #category : 'comparing' }
Point >> min: aMin max: aMax  [

	^ (self min: aMin) max: aMax
]

{ #category : 'point functions' }
Point >> nearestPointAlongLineFrom: p1 to: p2 [
	"Note this will give points beyond the endpoints.
	Streamlined by Gerardo Richarte 11/3/97"
	| x21 y21 t x1 y1 |
	p1 x = p2 x ifTrue: [^ p1 x @ y].
	p1 y = p2 y ifTrue: [^ x @ p1 y].
	x1 := p1 x asFloat.
	y1 := p1 y asFloat.
	x21 := p2 x asFloat - x1.
	y21 := p2 y asFloat - y1.
	t := ((y asFloat - y1 / x21) + (x asFloat - x1 / y21))
			/ ((x21 / y21) + (y21 / x21)).
	^ (x1 + (t * x21)) @ (y1 + (t * y21))
"
	| old new |
	Pen new place: 200@100; goto: (old := 500@300).
	Display reverse: (old extent: 10@10).
	[Sensor isAnyButtonPressed] whileFalse:
		[(new := (Sensor mousePoint nearestPointAlongLineFrom: 200@100 to: 500@300) )
			= old ifFalse:
				[Display reverse: (old extent: 10@10).
				Display reverse: ((old := new) extent: 10@10)]]
"

]

{ #category : 'point functions' }
Point >> nearestPointOnLineFrom: p1 to: p2 [
	"This will not give points beyond the endpoints"
	^ (self nearestPointAlongLineFrom: p1 to: p2)
		adhereTo: (p1 rect: p2)
]

{ #category : 'transforming' }
Point >> negated [
	"Answer a point whose x and y coordinates are the negatives of those of the receiver.  6/6/96 sw"
	"Optimized for speed -- ar 8/26/2001"
	^ (0 - x) @ (0 - y)
]

{ #category : 'point functions' }
Point >> normal [
	"Answer a Point representing the unit vector rotated 90 deg clockwise."

	| n |
	n := y negated @ x.
	^n / (n x * n x + (n y * n y)) sqrt
]

{ #category : 'point functions' }
Point >> normalized [
	"Optimized for speed -- ar 8/26/2001"
	| r |
	r := ((x*x) + (y * y)) sqrt.
	^(x / r) @ (y / r)
]

{ #category : 'point functions' }
Point >> onLineFrom: p1 to: p2 [
	^ self onLineFrom: p1 to: p2 within: 2
]

{ #category : 'point functions' }
Point >> onLineFrom: p1 to: p2 within: epsilon [
	"Answer true if the receiver lies on the given line segment between p1 and p2 within a small epsilon."

	"is this point within the box spanning p1 and p2 expanded by epsilon? (optimized)"
	p1 x < p2 x
		ifTrue: [
			((x < (p1 x - epsilon)) or: [x > (p2 x + epsilon)]) ifTrue: [^ false]]
		ifFalse: [
			((x < (p2 x - epsilon)) or: [x > (p1 x + epsilon)]) ifTrue: [^ false]].
	p1 y < p2 y
		ifTrue: [
			((y < (p1 y - epsilon)) or: [y > (p2 y + epsilon)]) ifTrue: [^ false]]
		ifFalse: [
			((y < (p2 y - epsilon)) or: [y > (p1 y + epsilon)]) ifTrue: [^ false]].

	"it's in the box; is it on the line?"
	^ (self dist: (self nearestPointAlongLineFrom: p1 to: p2)) <= epsilon
]

{ #category : 'printing' }
Point >> printOn: aStream  [
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@.
	y negative ifTrue: [ aStream space ].
	y printOn: aStream
]

{ #category : 'printing' }
Point >> printStringFractionDigits: placesDesired [
	^(x printStringFractionDigits: placesDesired), '@', (y printStringFractionDigits: placesDesired)
]

{ #category : 'point functions' }
Point >> quadrantOf: otherPoint [
	"Return 1..4 indicating relative direction to otherPoint.
	1 is downRight, 2=downLeft, 3=upLeft, 4=upRight"
	^ x <= otherPoint x
		ifTrue: [y < otherPoint y ifTrue: [1] ifFalse: [4]]
		ifFalse: [y <= otherPoint y ifTrue: [2] ifFalse: [3]]
"
[Sensor isAnyButtonPressed] whileFalse: [
	Display fill: (0@0 extent: 100@20) fillColor: Color white.
	(Display boundingBox center quadrantOf: Sensor mousePoint) printString displayAt: 0@0]
"
]

{ #category : 'polar coordinates' }
Point >> r [
	"Answer the receiver's radius in polar coordinate system."

	^self rho
]

{ #category : 'converting' }
Point >> rect: aPoint  [
	"Answer a Rectangle that encompasses the receiver and aPoint.
	This is the most general infix way to create a rectangle."

	^ Rectangle 
		origin: (self min: aPoint)
		corner: (self max: aPoint)
]

{ #category : 'polar coordinates' }
Point >> rho [
	"Answer the receiver's radius in polar coordinate system."

	^(self dotProduct: self) sqrt
]

{ #category : 'transforming' }
Point >> rotatedBy: radians [

	| r theta |
	r := self r.
	theta := self theta +  radians asFloat.
	^ (r * theta cos) @ (r * theta sin)
]

{ #category : 'truncation and round off' }
Point >> rounded [
	"Answer a Point that is the receiver's x and y rounded. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x rounded @ y rounded

]

{ #category : 'truncation and round off' }
Point >> roundedHAFZ [
	"Answer a Point that is the receiver's x and y rounded. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x roundedHAFZ @ y roundedHAFZ
]

{ #category : 'truncation and round off' }
Point >> roundedToEven [
	^ x roundedToEven @ y roundedToEven

]

{ #category : 'transforming' }
Point >> scaledBy: factor  [
	"Answer a Point scaled by factor (an instance of Point)."

	^(factor x * x) @ (factor y * y)
]

{ #category : 'copying' }
Point >> shallowCopy [
	"Immutable"
	^ self.
]

{ #category : 'geometry' }
Point >> sideOf: otherPoint  [
	"Returns #left, #right or #center if the otherPoint lies to the left, right 
	or on the line given by the vector from 0@0 to self"
	| side |
	side := (self crossProduct: otherPoint) sign.
	^ {#right. #center. #left} at: side + 2

]

{ #category : 'point functions' }
Point >> squaredDistanceTo: aPoint [
	"Answer the distance between aPoint and the receiver."
	| delta |
	delta := aPoint - self.
	^delta dotProduct: delta
]

{ #category : 'printing' }
Point >> storeOn: aStream  [
	"x@y printed form is good for storing too"
	self printOn: aStream
]

{ #category : 'polar coordinates' }
Point >> theta [
	"Answer the angle the receiver makes with origin in radians.
	Answer is between Zero and TwoPi
	If coordinates are in Display convention (y increases downwards), angles increase clockwise.
	If coordinates are in math convention (y increases upwards), angles increase counter-clockwise 
	"

	| tan theta |
	^x = 0
		ifTrue: [
			y >= 0
				ifTrue: [ 1.570796326794897 	"90.0 degreesToRadians"]
				ifFalse: [ 4.71238898038469	"270.0 degreesToRadians"]]
		ifFalse: [
			tan := y asFloat / x asFloat.
			theta := tan arcTan.
			x >= 0
				ifTrue: [
					y >= 0
						ifTrue: [ theta ]
						ifFalse: [
							6.283185307179586 "360.0 degreesToRadians"
							+ theta ]]
				ifFalse: [
					3.141592653589793 "180.0 degreesToRadians"
					+ theta ]]
]

{ #category : 'geometry' }
Point >> to: end1 intersects: start2 to: end2  [
	"Returns true if the linesegment from start1 (=self) to end1 intersects      
	    with the segment from start2 to end2, otherwise false."
	| start1 sideStart sideEnd |
	start1 := self.
	(((start1 = start2 or: [end1 = end2])
		or: [start1 = end2])
		or: [start2 = end1])
		ifTrue: [^ true].
	sideStart := start1 to: end1 sideOf: start2.
	sideEnd := start1 to: end1 sideOf: end2.
	sideStart = sideEnd ifTrue: [^ false].
	sideStart := start2 to: end2 sideOf: start1.
	sideEnd := start2 to: end2 sideOf: end1.
	sideStart = sideEnd ifTrue: [^ false].
	^ true
]

{ #category : 'geometry' }
Point >> to: end sideOf: otherPoint  [
	"Returns #left, #right, #center if the otherPoint lies to the left, right or on the line given by the vector from self to end"
	^ end - self sideOf: otherPoint - self
]

{ #category : 'transforming' }
Point >> translatedBy: delta  [
	"Answer a Point translated by delta (an instance of Point)."

	^(delta x + x) @ (delta y + y)
]

{ #category : 'point functions' }
Point >> transposed [
	^y@x
]

{ #category : 'geometry' }
Point >> triangleArea: b with: c [
	"Returns twice the area of the oriented triangle (a, b, c), i.e., the   
	area is positive if the triangle is oriented counterclockwise"
	^ b x - self x * (c y - self y) - (b y - self y * (c x - self x))
]

{ #category : 'truncation and round off' }
Point >> truncateTo: grid [
	"Answer a Point that is the receiver's x and y truncated to grid x and 
	grid y."
	| gridPoint |
	gridPoint := grid asPoint.
	^(x truncateTo: gridPoint x) @ (y truncateTo: gridPoint y)
]

{ #category : 'truncation and round off' }
Point >> truncated [
	"Answer a Point whose x and y coordinates are integers. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x truncated @ y truncated

]

{ #category : 'accessing' }
Point >> x [
	"Answer the x coordinate."
	^x
]

{ #category : 'accessing' }
Point >> y [
	"Answer the y coordinate."
	^y
]

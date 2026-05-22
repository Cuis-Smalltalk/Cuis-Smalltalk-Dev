"
This class represents a temporary contour structure during the construction of a TTGlyph from a TrueType file.

Instance variables:
	points				<ShortPointArray>	The points defining this contour
	controlPointFlags	<BitArray (ByteArray with bit content)> Flags for each point
"
Class {
	#name : 'TTContourConstruction',
	#superclass : 'Object',
	#instVars : [
		'points',
		'controlPointFlags'
	],
	#category : 'Graphics-TrueType',
	#package : 'Graphics',
	#tag : 'TrueType'
}

{ #category : 'instance creation' }
TTContourConstruction class >> on: points [

	^self new points: points
]

{ #category : 'primitive format' }
TTContourConstruction >> addContourDataTo: stream tx: aGeometryTransformationOrNil [
	| bezierCount firstPoint prevEndPoint |
	self isEmpty ifTrue: [
		^self ].

	"This bogus iteration could perhaps be avoided at the cost of extra memory."
	bezierCount := 0.
	self
		startPointDo: [ :firstBezierStart | ]
		quadraticBeziersDo: [ :controlPoint :endPoint | bezierCount := bezierCount + 1 ].
	stream nextPut: bezierCount.

	self
		startPointDo: [ :firstBezierStart |
			firstPoint := firstBezierStart.
			aGeometryTransformationOrNil notNil ifTrue: [
				firstPoint := aGeometryTransformationOrNil transform: firstPoint ].
			stream nextPut: firstPoint x; nextPut: firstPoint y.
			prevEndPoint := firstPoint ]
		quadraticBeziersDo: [ :controlPoint :endPoint |
			| cp ep cpr epr |
			cp := controlPoint.
			ep := endPoint.
			aGeometryTransformationOrNil notNil ifTrue: [
				cp := aGeometryTransformationOrNil transform: cp.
				ep := aGeometryTransformationOrNil transform: ep ].
			cpr := cp - prevEndPoint.
			epr := ep - prevEndPoint.
			"Make control point distict from start and end points for the benefit of VectorEnginePlugin"
			(cpr = `0.0@0.0` or: [ cpr = epr ]) ifTrue: [
				cpr := epr * 0.5 ].
			stream nextPut: epr x; nextPut: epr y.
			stream nextPut: cpr x; nextPut: cpr y.
			prevEndPoint := ep ].
]

{ #category : 'accessing' }
TTContourConstruction >> controlPointFlags [
	^controlPointFlags
]

{ #category : 'enumerating' }
TTContourConstruction >> initialMoveToAndPathCommands: anAffineTransformationOrNil [
	"TrueType reasonably assumes Y coordinates increase upwards."

	| initialMoveToCommand pathCommand firstPoint prevEndPoint endPoints controlPoints cp ep |
	endPoints := OrderedCollection new.
	controlPoints := OrderedCollection new.
	self
		startPointDo: [ :firstBezierStart |
			firstPoint := firstBezierStart.
			anAffineTransformationOrNil notNil ifTrue: [
				firstPoint := anAffineTransformationOrNil transform: firstPoint ].
			prevEndPoint := firstPoint ]
		quadraticBeziersDo: [ :controlPoint :endPoint |
			cp := controlPoint.
			ep := endPoint.
			anAffineTransformationOrNil notNil ifTrue: [
				cp := anAffineTransformationOrNil transform: cp.
				ep := anAffineTransformationOrNil transform: ep ].
			controlPoints add: cp - prevEndPoint.
			endPoints add: ep - prevEndPoint.
			prevEndPoint := ep ].
	initialMoveToCommand := PathMoveToCommand new toRelativePoint: firstPoint.
	pathCommand := PathQuadraticCurveToCommand new toRelativePoints: endPoints controlPoints: controlPoints.
	^{initialMoveToCommand. pathCommand}
]

{ #category : 'enumerating' }
TTContourConstruction >> isEmpty [
	^points size < 2
]

{ #category : 'converting' }
TTContourConstruction >> makePointsAbsolute: aPoint [
	| t |
	t := aPoint.
	1 to: points size do: [ :i |
		t := t + (points at: i).
		points at: i put: t ].
	^ t
]

{ #category : 'accessing' }
TTContourConstruction >> points [
	^points
]

{ #category : 'accessing' }
TTContourConstruction >> points: aShortPointArray [
	points := aShortPointArray.
	controlPointFlags := ByteArray newBits: aShortPointArray size
]

{ #category : 'printing' }
TTContourConstruction >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPut:$(;
		print: points size;
		"space;
		print: self type;"
		nextPut:$)
]

{ #category : 'enumerating' }
TTContourConstruction >> startPointDo: initialMoveBlock quadraticBeziersDo: bezierBlock [
	"Iterate over Bezier curves of the receiver.
	initialMoveBlock has a single argument: the startPoint of the first bezier
	bezierBlock has two arguments: controlPoint and endPoint.
	First StartPoint may be points first, or (if this is a control point), the mean between first and last.
		(this assumes that last point is also a control point, and a tacit first/end point was ommited. TTF can do this).
	Other StartPoints are assumed to be previous endPoint.
	Contour parts may either be straight line segments or quadratic bezier curves.
	The decision is made upon the type flags in controlPointFlags as follows:
	a) Two subsequent #isEndPoint points define a straight segment
	b) An #isEndPoint point followed by an #isControlPoint point followed 
		by an #isEndPoint point defines a quadratic bezier segment
	c)Two subsequent #isControlPoint points have an implicitely defined 
		#isEndPoint point at half the distance between them."

	| pointsSize firstPointIsControlPoint lastPointIsControlPoint firstBezierStart i nextI lastBezierEnd thisPoint thisPointIsControlPoint nextPoint nextPointIsControlPoint thisBezierControlPoint thisBezierEnd |
	pointsSize := points size.
	firstPointIsControlPoint := controlPointFlags bitBooleanAt: 1.
	firstPointIsControlPoint
		ifFalse: [
			firstBezierStart := points first.
			i := 2 ]
		ifTrue: [
			lastPointIsControlPoint := controlPointFlags bitBooleanAt: pointsSize.
			firstBezierStart := lastPointIsControlPoint
				ifTrue: [points first + points last / 2.0]
				ifFalse: [points last].
			i := 1 ].
	initialMoveBlock value: firstBezierStart.
	lastBezierEnd := firstBezierStart.
	[ i <= pointsSize ] whileTrue: [
		thisPoint := points at: i.
		thisPointIsControlPoint := controlPointFlags bitBooleanAt: i.
		thisPointIsControlPoint
			ifTrue: [
				thisBezierControlPoint := thisPoint.
				nextI := i = pointsSize ifFalse: [ i+1 ] ifTrue: [ 1 ].
				nextPoint := points at: nextI.
				nextPointIsControlPoint := controlPointFlags bitBooleanAt: nextI.
				nextPointIsControlPoint
					ifFalse: [ "Quadratic Bezier with explicit control and end points."
						thisBezierEnd := nextPoint.
						i := i + 2 ]
					ifTrue: [ "Quadratic Bezier with implicit end point."
						thisBezierEnd := thisPoint + nextPoint / 2.0.
						i := i + 1 ].
				]
			ifFalse: ["Line segment requested. Consider it as a bezier with an appropriate control point."
				thisBezierControlPoint := thisPoint + lastBezierEnd / 2.0.
				thisBezierEnd := thisPoint.
				i := i + 1 ].
		"Avoid null beziers. This, besides being being useless, is dangerous.
		Rasterization code uses curve length for computing hop, and this case would result in a division by zero."
		thisBezierEnd = lastBezierEnd ifFalse: [
			bezierBlock value: 	thisBezierControlPoint value: 	thisBezierEnd.
			lastBezierEnd := thisBezierEnd ].
		].
	"Close with a line if needed. TrueType contours must be closed shapes, because of fill."
	lastBezierEnd = firstBezierStart ifFalse: [
		bezierBlock value: lastBezierEnd + firstBezierStart / 2.0 value:  firstBezierStart ].
]

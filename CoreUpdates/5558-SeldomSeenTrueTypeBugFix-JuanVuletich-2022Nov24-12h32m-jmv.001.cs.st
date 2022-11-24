'From Cuis 6.0 [latest update: #5557] on 24 November 2022 at 12:36:31 pm'!

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 11/24/2022 11:55:35'!
initialMoveToAndPathCommands: anAffineTransformationOrNil
	"TrueType reasonably assumes Y coordinates increase upwards."

	| initialMoveToCommand pathCommand firstPoint prevEndPoint endPoints controlPoints cp ep |
	endPoints _ OrderedCollection new.
	controlPoints _ OrderedCollection new.
	self
		startPointDo: [ :firstBezierStart |
			firstPoint _ firstBezierStart.
			anAffineTransformationOrNil notNil ifTrue: [
				firstPoint _ anAffineTransformationOrNil transform: firstPoint ].
			prevEndPoint _ firstPoint ]
		quadraticBeziersDo: [ :controlPoint :endPoint |
			cp _ controlPoint.
			ep _ endPoint.
			anAffineTransformationOrNil notNil ifTrue: [
				cp _ anAffineTransformationOrNil transform: cp.
				ep _ anAffineTransformationOrNil transform: ep ].
			controlPoints add: cp - prevEndPoint.
			endPoints add: ep - prevEndPoint.
			prevEndPoint _ ep ].
	initialMoveToCommand _ PathMoveToCommand new toRelativePoint: firstPoint.
	pathCommand _ PathQuadraticCurveToCommand new toRelativePoints: endPoints controlPoints: controlPoints.
	^{initialMoveToCommand. pathCommand}! !

!TTContourConstruction methodsFor: 'enumerating' stamp: 'jmv 11/24/2022 12:33:31'!
startPointDo: initialMoveBlock quadraticBeziersDo: bezierBlock
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
	pointsSize _ points size.
	firstPointIsControlPoint _ controlPointFlags bitBooleanAt: 1.
	firstPointIsControlPoint
		ifFalse: [
			firstBezierStart _ points first.
			i _ 2 ]
		ifTrue: [
			lastPointIsControlPoint _ controlPointFlags bitBooleanAt: pointsSize.
			firstBezierStart _ lastPointIsControlPoint
				ifTrue: [points first + points last / 2.0]
				ifFalse: [points last].
			i _ 1 ].
	initialMoveBlock value: firstBezierStart.
	lastBezierEnd _ firstBezierStart.
	[ i <= pointsSize ] whileTrue: [
		thisPoint _ points at: i.
		thisPointIsControlPoint _ controlPointFlags bitBooleanAt: i.
		thisPointIsControlPoint
			ifTrue: [
				thisBezierControlPoint _ thisPoint.
				nextI _ i = pointsSize ifFalse: [ i+1 ] ifTrue: [ 1 ].
				nextPoint _ points at: nextI.
				nextPointIsControlPoint _ controlPointFlags bitBooleanAt: nextI.
				nextPointIsControlPoint
					ifFalse: [ "Quadratic Bezier with explicit control and end points."
						thisBezierEnd _ nextPoint.
						i _ i + 2 ]
					ifTrue: [ "Quadratic Bezier with implicit end point."
						thisBezierEnd _ thisPoint + nextPoint / 2.0.
						i _ i + 1 ].
				]
			ifFalse: ["Line segment requested. Consider it as a bezier with an appropriate control point."
				thisBezierControlPoint _ thisPoint + lastBezierEnd / 2.0.
				thisBezierEnd _ thisPoint.
				i _ i + 1 ].
		"Avoid null beziers. This, besides being being useless, is dangerous.
		Rasterization code uses curve length for computing hop, and this case would result in a division by zero."
		thisBezierEnd = lastBezierEnd ifFalse: [
			bezierBlock value: 	thisBezierControlPoint value: 	thisBezierEnd.
			lastBezierEnd _ thisBezierEnd ].
		].
	"Close with a line if needed. TrueType contours must be closed shapes, because of fill."
	lastBezierEnd = firstBezierStart ifFalse: [
		bezierBlock value: lastBezierEnd + firstBezierStart / 2.0 value:  firstBezierStart ].! !


!TTContourConstruction methodsFor: 'primitive format' stamp: 'jmv 11/24/2022 11:58:30'!
addContourDataTo: stream tx: aGeometryTransformationOrNil
	| bezierCount firstPoint cp ep prevEndPoint cpr epr |
	self isEmpty ifTrue: [
		^self ].

	"This bogus iteration could perhaps be avoided at the cost of extra memory."
	bezierCount _ 0.
	self
		startPointDo: [ :firstBezierStart | ]
		quadraticBeziersDo: [ :controlPoint :endPoint | bezierCount _ bezierCount + 1 ].
	stream nextPut: bezierCount.

	self
		startPointDo: [ :firstBezierStart |
			firstPoint _ firstBezierStart.
			aGeometryTransformationOrNil notNil ifTrue: [
				firstPoint _ aGeometryTransformationOrNil transform: firstPoint ].
			stream nextPut: firstPoint x; nextPut: firstPoint y.
			prevEndPoint _ firstPoint ]
		quadraticBeziersDo: [ :controlPoint :endPoint |
			cp _ controlPoint.
			ep _ endPoint.
			aGeometryTransformationOrNil notNil ifTrue: [
				cp _ aGeometryTransformationOrNil transform: cp.
				ep _ aGeometryTransformationOrNil transform: ep ].
			cpr _ cp - prevEndPoint.
			epr _ ep - prevEndPoint.
			"Make control point distict from start and end points for the benefit of VectorEnginePlugin"
			(cpr = `0.0@0.0` or: [ cpr = epr ]) ifTrue: [
				cpr _ epr * 0.5 ].
			stream nextPut: epr x; nextPut: epr y.
			stream nextPut: cpr x; nextPut: cpr y.
			prevEndPoint _ ep ].! !


!TTGlyph methodsFor: 'drawing' stamp: 'jmv 11/24/2022 11:56:35'!
drawGlyphOn: aVectorCanvas geometryTransformation: txOrNil
	| initialPathAndMove |
	contours do: [ :contour |
		contour isEmpty ifFalse: [
			aVectorCanvas moveTo: `0.0@0.0`.
			initialPathAndMove _ contour initialMoveToAndPathCommands: txOrNil.
			initialPathAndMove first runOn: aVectorCanvas .
			initialPathAndMove second runOn: aVectorCanvas ]].! !

!methodRemoval: TTContourConstruction #quadraticBeziersDo: stamp: 'jmv 11/24/2022 12:34:33'!
TTContourConstruction removeSelector: #quadraticBeziersDo:!
!methodRemoval: TTContourConstruction #pathCommand: stamp: 'jmv 11/24/2022 12:34:27'!
TTContourConstruction removeSelector: #pathCommand:!
!methodRemoval: TTContourConstruction #initialMoveToCommand: stamp: 'jmv 11/24/2022 12:34:27'!
TTContourConstruction removeSelector: #initialMoveToCommand:!

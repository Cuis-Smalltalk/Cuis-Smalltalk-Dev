'From Cuis 6.0 [latest update: #5546] on 1 November 2022 at 5:36:35 pm'!

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 11/1/2022 17:35:27'!
fontToUse: fontOrNil
	"Answer a suitable font, aFont if possible."

	^fontOrNil ifNil: [ FontFamily defaultFamilyAndPointSize ].! !

!methodRemoval: BitBltCanvas #fontToUse: stamp: 'jmv 11/1/2022 17:35:34'!
BitBltCanvas removeSelector: #fontToUse:!
!methodRemoval: AbstractVectorCanvas #fontToUse: stamp: 'jmv 11/1/2022 17:35:32'!
AbstractVectorCanvas removeSelector: #fontToUse:!
!methodRemoval: FontFamily class #defaultOrTrueTypeFamilyAndPointSize stamp: 'jmv 11/1/2022 17:36:10'!
FontFamily class removeSelector: #defaultOrTrueTypeFamilyAndPointSize!

!AbstractVectorCanvas reorganize!
('accessing' engine geometryTransformation: morphIds)
('stroke & fill' fillColor:do: initializePath strokeWidth:color:do: strokeWidth:color:fillColor:do:)
('paths' arcTo:radius:angleOfXAxis:largeFlag:sweepFlag: circleCenter:radius: closePath closePathAndMoveBack cubicBezier: cubicBezierSmoothTo:control2: cubicBezierTo:control1:control2: curveTo:control1:control2: ellipseCenter:radius: ellipseCenter:radius:rotationAngle: ellipseCenter:radius:rotationAngle:start:sweep: lineTo: lineToX: lineToY: moveTo: polyLine: quadraticBezier: quadraticBezierSmoothTo: quadraticBezierTo:control: rectangleMinX:maxX:minY:maxY: roundBottomRectangleMinX:maxX:minY:maxY:rx:ry: roundRectangleMinX:maxX:minY:maxY:rx:ry: roundTopRectangleMinX:maxX:minY:maxY:rx:ry:)
('paths - relative coordinates' rel_ArcTo:radius:angleOfXAxis:largeFlag:sweepFlag: rel_CubicBezierSmoothTo:control2: rel_CubicBezierTo:control1:control2: rel_LineTo: rel_MoveTo: rel_QuadraticBezierSmoothTo: rel_QuadraticBezierTo:control:)
('paths - sequences' multiArc:rs:xAxisRotations:toPoints:doLargerArcs:useIncreasingAngles: multiCurveTo:toPoints:control1Points:control2Points: multiCurveToSmooth:toPoints:control2Points: multiLineTo:points: multiLineTo:xs: multiLineTo:ys: multiMoveTo:points: multiQuadraticCurveTo:toPoints:controlPoints: multiQuadraticCurveToSmooth:toPoints: pathSequence:)
('drawing - text' drawString:from:to:atBaseline:font:color:)
('drawing - icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawExpandedAt: drawHandLarge drawHandSmall drawLeftIcon drawMenuIcon drawNotExpandedAt: drawPushPinIcon drawRightIcon drawUpIcon)
('drawing - windows' drawRoundedFrame:border:color:insideColor:labelHeight: roundBottomLeftCornerX:y:length:border:color: roundBottomRightCornerX:y:length:border:color: roundEdge:border:color: roundTopLeftCornerX:y:length:border:color: roundTopRightCornerX:y:length:border:color:)
('drawing - MorphicCanvas protocol' ellipseCenter:radius:borderWidth:borderColor:fillColor: fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:topLeftColor:bottomRightColor:borderWidth: line:to:width:color: roundRect:color:radius:)
('morphic' drawHand drawsHandLarge into: outOfMorph)
('morphic world' boundingRectOfCurrentMorphAfterDraw)
!


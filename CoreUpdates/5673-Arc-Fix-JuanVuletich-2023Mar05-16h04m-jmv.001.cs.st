'From Cuis 6.0 [latest update: #5672] on 5 March 2023 at 4:18:33 pm'!

!VectorEngine methodsFor: 'paths' stamp: 'jmv 3/5/2023 16:18:21'!
arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color black fillColor: Color red do: [ :engine |
		engine arcCenter: 100@100 radius: 80@40 rotationAngle: -0.2 start: 0 sweep: Float twoPi ].
Display forceToScreen.
"
	| angle thetaCos thetaSin xp yp x0 y0 ttheta tthetaCos tthetaSin |
	thetaCos _ theta cos.
	thetaSin _ theta sin.
	angle _ startAngle.
	xp _ angle cos * radiusPoint x.
	yp _ angle sin * radiusPoint y.
	x0 _ thetaCos * xp - (thetaSin * yp) + aPoint x.
	y0 _ thetaSin * xp + (thetaCos * yp) + aPoint y.
	(sweepAngle ~= Float twoPi and: [firstX isNaN not])
		ifTrue: [
			"If an arc (not whole circle/ellipse), join with a line, to ensure closed path.
			For example, for rounded rectangles."
			self lineFromX: currentX y: currentY toX: x0 y: y0.
			currentX _ x0.
			currentY _ y0 ].

	ttheta _ theta + geometryTransformation radians.
	tthetaCos _ ttheta cos.
	tthetaSin _ ttheta sin.
	geometryTransformation doesMirror
		ifTrue: [
			self arcCenterX: aPoint x centerY: aPoint y radiusX: radiusPoint x radiusY: radiusPoint y
				start: startAngle negated sweep: sweepAngle negated rotationCos: tthetaCos rotationSin: tthetaSin ]
		ifFalse: [
			self arcCenterX: aPoint x centerY: aPoint y radiusX: radiusPoint x radiusY: radiusPoint y
				start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin ].

	angle _ startAngle + sweepAngle.
	xp _ angle cos * radiusPoint x.
	yp _ angle sin * radiusPoint y.
	currentX _ thetaCos * xp - (thetaSin * yp) + aPoint x.
	currentY _ thetaSin * xp + (thetaCos * yp) + aPoint y.

	sweepAngle = Float twoPi
		ifTrue: [
			"If we were asked for a full circle/ellipse, ensure we end exactly at the same point we started,
			even if there is some rounding error."
			self lineFromX: currentX y: currentY toX: x0 y: y0.
			currentX _ x0.
			currentY _ y0 ].! !

!VectorEngine methodsFor: 'paths' stamp: 'jmv 3/5/2023 16:17:31'!
arcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAnglesArg
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color red do: [ :engine |
		engine
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.2 position: 200@100);
	strokeWidth: 1.0 color: Color red do: [ :engine |
		engine
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: false ].
Display forceToScreen
"
	| x1 y1 x2 y2 radiusPoint rx ry cosPhi sinPhi x12hd y12hd x1p y1p rxSq rySq x1pSq y1pSq bigSqR bigSqRNumerator bigSqRDenominator sign cxp cyp cx cy theta1 theta2 deltaTheta scaleSq scale newRx newRy useIncreasingAngles |
	x1 _ currentX.
	y1 _ currentY.
	x2 _ aPoint x.
	y2 _ aPoint y.
	radiusPoint _ radiusPointOrNumber asPoint.
	rx _ radiusPoint x.
	ry _ radiusPoint y.
	"Handle out of range parameters as requested by the w3 implementation notes"
	(x2 = x1 and: [ y2 = y1 ]) ifTrue: [ ^self ].
	(rx = 0.0 or: [ ry = 0.0 ]) ifTrue: [
		self lineFromX: currentX y: currentY toX: x2 y: y2.
		currentX _ x2.
		currentY _ y2.
		^self ].
	rx < 0 ifTrue: [
		^self arcTo: aPoint radius: 0.0 -rx @ ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAnglesArg ].
	ry < 0 ifTrue: [
		^self arcTo: aPoint radius: rx @ (0.0-ry) angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAnglesArg ].

	cosPhi _ phi cos.
	sinPhi _ phi sin.
	"step 1 in `B.2. Elliptical arc parameter conversion` at http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes"
	x12hd _ (x1-x2) * 0.5.
	y12hd _ (y1-y2) * 0.5.
	x1p _ (cosPhi * x12hd) + (sinPhi * y12hd).
	y1p _ (cosPhi * y12hd) - (sinPhi * x12hd).

	"step 2"
	rxSq _ rx * rx.
	rySq _ ry * ry.
	x1pSq _ x1p * x1p.
	y1pSq _ y1p * y1p.
	"ensure radii are large enough"
	scaleSq _ (x1pSq / rxSq) + (y1pSq / rySq).
	scaleSq > 1 ifTrue: [
		scale _ scaleSq sqrt.
		newRx _ scale * rx.
		newRy _ scale * ry.
		(newRx = rx or: [ newRy = ry ]) ifTrue: [		"This happens when scaleSq is so close to 1, that the multiplication rounds to the same numbers"
			newRx _ scaleSq * rx.
			newRy _ scaleSq * ry ].
		^self arcTo: aPoint radius: newRx @ newRy angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAnglesArg
		].
	bigSqRNumerator _ (rxSq * rySq) - (rxSq * y1pSq)-(rySq * x1pSq).
	bigSqRDenominator _ (rxSq * y1pSq) + (rySq * x1pSq).
	useIncreasingAngles _ useIncreasingAnglesArg xor: geometryTransformation doesMirror.
	sign _ doLargerArc = useIncreasingAngles ifTrue: [ -1 ] ifFalse: [ 1 ].
	bigSqR _ (bigSqRNumerator / bigSqRDenominator max: 0.0) sqrt * sign.		"This only can be < 0.0 because of rounding errors. Take 0.0 in such cases."
	cxp _ bigSqR * rx * y1p / ry.
	cyp _ 0.0 - (bigSqR * ry * x1p / rx).

	"step 3"
	cx _ (cosPhi * cxp) - (sinPhi * cyp) + ((x1+x2) * 0.5).
	cy _ (sinPhi * cxp) + (cosPhi * cyp) + ((y1+y2) * 0.5).

	"step 4"
	theta1 _ (((x1p - cxp) / rx) @ ((y1p - cyp) / ry)) theta.
	theta2 _ (((0 - x1p - cxp) / rx) @ (( 0 -y1p - cyp) / ry)) theta.
	deltaTheta _ theta2 - theta1.
	useIncreasingAngles = (deltaTheta > 0) ifFalse: [
		deltaTheta > 0
			ifTrue: [ deltaTheta _ deltaTheta - Float pi - Float pi ]
			ifFalse: [ deltaTheta _ deltaTheta + Float pi + Float pi ]
		].

	self arcCenter: cx@cy radius: rx@ry rotationAngle: phi start: theta1 sweep: deltaTheta! !


"
A Vector Graphics rasterizer for Morphic.
Supports most SVG primitives.
Gives exceptional quality.

Based on https://www.researchgate.net/publication/267152327_Prefiltering_Antialiasing_for_General_Vector_Graphics
"
Class {
	#name : 'VectorEngine',
	#superclass : 'Object',
	#instVars : [
		'canvas',
		'antiAliasingWidth',
		'geometryTransformation',
		'strokeWidth',
		'morphBoundsLeft',
		'morphBoundsTop',
		'morphBoundsRight',
		'morphBoundsBottom',
		'hop',
		'firstX',
		'firstY',
		'currentX',
		'currentY'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'experiments' }
VectorEngine class >> circleApproximationExamples [
"
VectorEngine circleApproximationExamples
https://spencermortensen.com/articles/bezier-circle/"
| ve c cx cy d e r |
cx := 250.
cy := 250.
r := 200.
c := 0.085 * r.
d := 0.5 * r.
e := 0.552 * r.
ve := VectorEngine onForm: Display subPixelAntiAliasing: false.
ve
	strokeWidth: 5.0 color: Color red do: [ :engine |
		engine
			circleCenter: cx @ cy radius: r].
ve												"Quadratic, coarse and better approximations"
	strokeWidth: 1.0 color: Color white do: [ :engine |
		engine
			moveTo: cx @ (cy + r);
			quadraticBezierTo: cx - r @ cy control: (cx - r) @ (cy + r);
			moveTo: cx @ (cy + r);
			quadraticBezierTo: cx - r @ cy control: (cx - r + c) @ (cy + r - c) ].
ve												"Cubic, coarse approximation"
	strokeWidth: 1.0 color: Color green do: [ :engine |
		engine
			moveTo: cx @ (cy + r);
			cubicBezierTo: cx - r @ cy control1: (cx - d) @ (cy + r) control2: (cx - r) @ (cy + d) ].
ve												"Cubic, good approximation"
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: cx @ (cy + r);
			cubicBezierTo: cx - r @ cy control1: (cx - e) @ (cy + r) control2: (cx - r) @ (cy + e) ].
Display forceToScreen

]

{ #category : 'instance creation' }
VectorEngine class >> defaultAntiAliasingWidth [
	self subclassResponsibility
]

{ #category : 'experiments' }
VectorEngine class >> experLinesAndShapes [
	"
Display fill: (0@0 corner: 120@1200) fillColor: Color white.
	VectorEngine experLinesAndShapes
	"
	| engine y1 yy2 dy y2 aaw |
	engine := self onForm: Display subPixelAntiAliasing: true.
	aaw := engine class defaultAntiAliasingWidth.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	Display forceToScreen.
	engine strokeWidth: aaw color: Color black fillColor: Color black do: [
		engine
			moveTo: 220@10;
			lineTo: 410@15; lineTo: 220@20;
			lineTo: 215@410; lineTo: 210@20;
			lineToFirstPoint ].
	Display forceToScreen.

	y1 := 10.
	dy := 3.
	y2 := aaw * 2.
	15 timesRepeat: [
		"stroke with w > aaw"
		yy2 := y2 >= 1 ifTrue: [ aaw * 0.5 ] ifFalse: [0].
		engine strokeWidth: aaw color: Color black fillColor: Color black do: [
			engine
				moveTo: 431@(y1+yy2); lineTo: 530@(y1+yy2+dy);
				lineTo: 530@(y1+y2-yy2+dy); lineTo: 431@(y1+y2-yy2);
				lineToFirstPoint.
			engine
				moveTo: y1+yy2@431; lineTo: y1+yy2+dy@530;
				lineTo: y1+y2-yy2+dy@530; lineTo: y1+y2-yy2@431;
				lineToFirstPoint ].
		"Stroke too narrow to be visible"
		engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
			engine
				moveTo: 531@(y1+dy); lineTo: 630@(y1+dy+dy);
				lineTo: 630@(y1+y2+dy+dy); lineTo: 531@(y1+y2+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy@531; lineTo: y1+dy+dy@630;
				lineTo: y1+y2+dy+dy@630; lineTo: y1+y2+dy@531;
				lineToFirstPoint ].
		"Only fill."
		engine fillColor: Color black do: [
			engine
				moveTo: 631@(y1+dy+dy); lineTo: 730@(y1+dy+dy+dy);
				lineTo: 730@(y1+y2+dy+dy+dy); lineTo: 631@(y1+y2+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy@631; lineTo: y1+dy+dy+dy@730;
				lineTo: y1+y2+dy+dy+dy@730; lineTo: y1+y2+dy+dy@631;
				lineToFirstPoint ].
		"Only stroke, no fill"
		engine strokeWidth: y2 color: Color black do: [
			engine
				moveTo: y2*0.5+731@(y2*0.5 + y1+dy+dy+dy); lineTo: 830@(y2 * 0.5 + y1 + dy + dy+dy+dy).
			engine
				moveTo: y2*0.5 + y1+dy+dy+dy@(y2*0.5+731); lineTo: y2 * 0.5 + y1 + dy + dy+dy+dy @ 830 ].
		y1 := y1 + y2 + aaw + 3.
		Display forceToScreen.
		y2 print.
		y2 := y2 * 0.8.
	].
	
	Display forceToScreen
]

{ #category : 'experiments' }
VectorEngine class >> experLinesAndShapes2 [
	"
	VectorEngine experLinesAndShapes2
	"
	| engine y1 dy y2 aaw |
	engine := self onForm: Display subPixelAntiAliasing: true.
	aaw := engine class defaultAntiAliasingWidth.

	y1 := 10.
	dy := 3.
	y2 := aaw*4*2.
	15 timesRepeat: [
		"stroke is narrow but visible"
		engine strokeWidth: 1.0 color: Color green fillColor: Color red do: [
			engine
				moveTo: 131@y1; lineTo: 230@(y1+dy);
				lineTo: 230@(y1+y2+dy); lineTo: 131@(y1+y2);
				lineToFirstPoint.
			engine
				moveTo: y1@131; lineTo: y1+dy@230;
				lineTo: y1+y2+dy@230; lineTo: y1+y2@131;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.8 color: Color green fillColor: Color red do: [
			engine
				moveTo: 231@(y1+dy); lineTo: 330@(y1+dy+dy);
				lineTo: 330@(y1+y2+dy+dy); lineTo: 231@(y1+y2+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy@231; lineTo: y1+dy+dy@330;
				lineTo: y1+y2+dy+dy@330; lineTo: y1+y2+dy@231;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.6 color: Color green fillColor: Color red do: [
			engine
				moveTo: 331@(y1+dy+dy); lineTo: 430@(y1+dy+dy+dy);
				lineTo: 430@(y1+y2+dy+dy+dy); lineTo: 331@(y1+y2+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy@331; lineTo: y1+dy+dy+dy@430;
				lineTo: y1+y2+dy+dy+dy@430; lineTo: y1+y2+dy+dy@331;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.4 color: Color green fillColor: Color red do: [
			engine
				moveTo: 431@(y1+dy+dy+dy); lineTo: 530@(y1+dy+dy+dy+dy);
				lineTo: 530@(y1+y2+dy+dy+dy+dy); lineTo: 431@(y1+y2+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy@431; lineTo: y1+dy+dy+dy+dy@530;
				lineTo: y1+y2+dy+dy+dy+dy@530; lineTo: y1+y2+dy+dy+dy@431;
				lineToFirstPoint ].
		"stroke is narrow but visible"
		engine strokeWidth: 0.2 color: Color green fillColor: Color red do: [
			engine
				moveTo: 531@(y1+dy+dy+dy+dy); lineTo: 630@(y1+dy+dy+dy+dy+dy);
				lineTo: 630@(y1+y2+dy+dy+dy+dy+dy); lineTo: 531@(y1+y2+dy+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy+dy@531; lineTo: y1+dy+dy+dy+dy+dy@630;
				lineTo: y1+y2+dy+dy+dy+dy+dy@630; lineTo: y1+y2+dy+dy+dy+dy@531;
				lineToFirstPoint ].
		"stroke zero means no stroke"
		engine strokeWidth: 0.0 color: Color green fillColor: Color red do: [
			engine
				moveTo: 631@(y1+dy+dy+dy+dy+dy); lineTo: 730@(y1+dy+dy+dy+dy+dy+dy);
				lineTo: 730@(y1+y2+dy+dy+dy+dy+dy+dy); lineTo: 631@(y1+y2+dy+dy+dy+dy+dy);
				lineToFirstPoint.
			engine
				moveTo: y1+dy+dy+dy+dy+dy@631; lineTo: y1+dy+dy+dy+dy+dy+dy@730;
				lineTo: y1+y2+dy+dy+dy+dy+dy+dy@730; lineTo: y1+y2+dy+dy+dy+dy+dy@631;
				lineToFirstPoint ].
		y1 := y1 + y2 + aaw + 3.
		Display forceToScreen.
		y2 print.
		y2 := y2 * 0.8.
	].
	
	Display forceToScreen
]

{ #category : 'experiments' }
VectorEngine class >> experimentHopsPerPixel1 [
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 1: Very thin stroke, the same color as fill
	VectorEngine experimentHopsPerPixel1
	"
	| d engine f1 f2 hops1 hops2 |
	hops1 := 200.
	hops2 := 3.
	f1 := Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine := self onForm: f1 subPixelAntiAliasing: true.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops1.
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	f1 displayAt: 0@0.

	f2 := Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine := self onForm: f2 subPixelAntiAliasing: true.
	engine strokeWidth: 0.001 color: Color black fillColor: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops2.
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	f2 displayAt: 220@0.

	Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d := (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d := (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) *10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]
]

{ #category : 'experiments' }
VectorEngine class >> experimentHopsPerPixel2 [
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 2: Stroke of various widths, no fill
	VectorEngine experimentHopsPerPixel2
	"
	"In the following script, err is the error admitted. It is consistent with a hop of 0.5 for fills."
	"
	| err r rads hop hopx |
	'------' print.
	err := 0.25.
	#(0.001 0.01 0.1 0.2 0.25 0.3 0.5 1 1.4 10 100 200 265 400 800 1600 3200 6400 12800) do: [ :w |
		r := w / 2.
		rads := (1-(err/r)) arcCos.
		hop := rads sin * r. 'theoretical'.
		hopx := w sqrt / 2.0 max: 0.5. 'approximated'.
		{r. hop. hopx} print ].
	"
	| d engine f1 f2 hops1 hops2 strokeWidth |
	hops1 := 200.
	hops1 := 20.
	hops2 := 3.
	strokeWidth := 1.4.
	f1 := Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine := self onForm: f1 subPixelAntiAliasing: true.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops1.
		engine
			moveTo: 50@30;
			lineTo: 190@135; lineTo: 50 @50;
			lineTo: 135@390; lineTo: 30@50;
			lineToFirstPoint ].
	f1 displayAt: 0@0.

	f2 := Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine := self onForm: f2 subPixelAntiAliasing: true.
	engine strokeWidth: strokeWidth color: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops2.
		engine
			moveTo: 50@30;
			lineTo: 190@135; lineTo: 50@50;
			lineTo: 135@390; lineTo: 30@50;
			lineToFirstPoint ].
	f2 displayAt: 220@0.

Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d := (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d := (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) * 10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]
]

{ #category : 'experiments' }
VectorEngine class >> experimentHopsPerPixel3 [
	"Play with this to learn about the effect of hopsPerPixel
	Experiment 3: No stroke, just fill
	VectorEngine experimentHopsPerPixel3
	"
	| d engine f1 f2 hops1 hops2 |
	hops1 := 200.
	hops2 := 3.
	f1 := Form extent: 220@420 depth: 32.
	f1 fillColor: Color white.
	engine := self onForm: f1 subPixelAntiAliasing: true.
	engine fillColor: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops1.
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	f1 displayAt: 0@0.

	f2 := Form extent: 220@420 depth: 32.
	f2 fillColor: Color white.
	engine := self onForm: f2 subPixelAntiAliasing: true.
	engine fillColor: Color black do: [
		engine instVarNamed: 'hop' put: 1.0 / hops2.
		engine
			moveTo: 20@10;
			lineTo: 210@15; lineTo: 20@20;
			lineTo: 15@410; lineTo: 10@20;
			lineToFirstPoint ].
	f2 displayAt: 220@0.

	Feature require: 'LinearAlgebra'.
	Smalltalk at: #FloatMatrix ifPresent: [ :matrixClass |
		d := (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 440@0.
		d := (matrixClass form: f1 color: #red) - (matrixClass form: f2 color: #red) * 10 + 0.5.
		(d asFormMin: 0 max: 1) displayAt: 660@0.
		{d elements min * 255. d elements max * 255. 'diff entre'. hops1. hops2} print ]
]

{ #category : 'instance creation' }
VectorEngine class >> onForm: aForm [
	"To be called on specific subclasses."

	^self onForm: aForm subPixelAntiAliasing: nil
]

{ #category : 'instance creation' }
VectorEngine class >> onForm: aForm subPixelAntiAliasing: aBoolean [
	"nil means use default kind of anti aliasing
	Use the VectorEnginePlugin if available."

	| engineClass doSubPixelAntiAliasing |
	self hasSubclasses
		ifFalse: [
			engineClass := self ]
		ifTrue: [
			doSubPixelAntiAliasing := aBoolean ifNil: [
				"Assume high resolution implies high DPI"
				Display width <= 1920 ].
			engineClass := (VectorEngineWithPlugin isPluginAvailable and: [
					"VectorEnginePlugin calls can not be reliably simulated in the Debugger because the plugin holds static state."
					Processor activeProcess isSimulatingAnotherProcess not ])
				ifTrue: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineWithPluginSubPixel]
					ifFalse: [VectorEngineWithPluginWholePixel]]
				ifFalse: [ doSubPixelAntiAliasing
					ifTrue: [VectorEngineSubPixel]
					ifFalse: [VectorEngineWholePixel]]].
		^engineClass new initializeOnForm: aForm.
]

{ #category : 'accessing' }
VectorEngine >> antiAliasingWidth [

	^antiAliasingWidth
]

{ #category : 'accessing' }
VectorEngine >> antiAliasingWidth: aNumber [

	antiAliasingWidth := aNumber.
]

{ #category : 'paths' }
VectorEngine >> arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle [
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color black fillColor: Color red do: [ :engine |
		engine arcCenter: 100@100 radius: 80@40 rotationAngle: -0.2 start: 0 sweep: Float twoPi ].
Display forceToScreen.
"
	| angle thetaCos thetaSin xp yp x0 y0 ttheta tthetaCos tthetaSin |
	thetaCos := theta cos.
	thetaSin := theta sin.
	angle := startAngle.
	xp := angle cos * radiusPoint x.
	yp := angle sin * radiusPoint y.
	x0 := thetaCos * xp - (thetaSin * yp) + aPoint x.
	y0 := thetaSin * xp + (thetaCos * yp) + aPoint y.
	(sweepAngle ~= Float twoPi and: [firstX isNaN not])
		ifTrue: [
			"If an arc (not whole circle/ellipse), join with a line, to ensure closed path.
			For example, for rounded rectangles."
			self lineFromX: currentX y: currentY toX: x0 y: y0.
			currentX := x0.
			currentY := y0 ].

	ttheta := theta + geometryTransformation radians.
	tthetaCos := ttheta cos.
	tthetaSin := ttheta sin.
	geometryTransformation doesMirror
		ifTrue: [
			self arcCenterX: aPoint x centerY: aPoint y radiusX: radiusPoint x radiusY: radiusPoint y
				start: startAngle negated sweep: sweepAngle negated rotationCos: tthetaCos rotationSin: tthetaSin ]
		ifFalse: [
			self arcCenterX: aPoint x centerY: aPoint y radiusX: radiusPoint x radiusY: radiusPoint y
				start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin ].

	angle := startAngle + sweepAngle.
	xp := angle cos * radiusPoint x.
	yp := angle sin * radiusPoint y.
	currentX := thetaCos * xp - (thetaSin * yp) + aPoint x.
	currentY := thetaSin * xp + (thetaCos * yp) + aPoint y.

	sweepAngle = Float twoPi
		ifTrue: [
			"If we were asked for a full circle/ellipse, ensure we end exactly at the same point we started,
			even if there is some rounding error."
			self lineFromX: currentX y: currentY toX: x0 y: y0.
			currentX := x0.
			currentY := y0 ].
]

{ #category : 'paths' }
VectorEngine >> arcTo: aPoint radius: radiusPointOrNumber angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAnglesArg [
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
	x1 := currentX.
	y1 := currentY.
	x2 := aPoint x.
	y2 := aPoint y.
	radiusPoint := radiusPointOrNumber asPoint.
	rx := radiusPoint x.
	ry := radiusPoint y.
	"Handle out of range parameters as requested by the w3 implementation notes"
	(x2 = x1 and: [ y2 = y1 ]) ifTrue: [ ^self ].
	(rx = 0.0 or: [ ry = 0.0 ]) ifTrue: [
		self lineFromX: currentX y: currentY toX: x2 y: y2.
		currentX := x2.
		currentY := y2.
		^self ].
	rx < 0 ifTrue: [
		^self arcTo: aPoint radius: 0.0 -rx @ ry angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAnglesArg ].
	ry < 0 ifTrue: [
		^self arcTo: aPoint radius: rx @ (0.0-ry) angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAnglesArg ].

	cosPhi := phi cos.
	sinPhi := phi sin.
	"step 1 in `B.2. Elliptical arc parameter conversion` at http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes"
	x12hd := (x1-x2) * 0.5.
	y12hd := (y1-y2) * 0.5.
	x1p := (cosPhi * x12hd) + (sinPhi * y12hd).
	y1p := (cosPhi * y12hd) - (sinPhi * x12hd).

	"step 2"
	rxSq := rx * rx.
	rySq := ry * ry.
	x1pSq := x1p * x1p.
	y1pSq := y1p * y1p.
	"ensure radii are large enough"
	scaleSq := (x1pSq / rxSq) + (y1pSq / rySq).
	scaleSq > 1 ifTrue: [
		scale := scaleSq sqrt.
		newRx := scale * rx.
		newRy := scale * ry.
		(newRx = rx or: [ newRy = ry ]) ifTrue: [		"This happens when scaleSq is so close to 1, that the multiplication rounds to the same numbers"
			newRx := scaleSq * rx.
			newRy := scaleSq * ry ].
		^self arcTo: aPoint radius: newRx @ newRy angleOfXAxis: phi largeFlag: doLargerArc sweepFlag: useIncreasingAnglesArg
		].
	bigSqRNumerator := (rxSq * rySq) - (rxSq * y1pSq)-(rySq * x1pSq).
	bigSqRDenominator := (rxSq * y1pSq) + (rySq * x1pSq).
	useIncreasingAngles := useIncreasingAnglesArg xor: geometryTransformation doesMirror.
	sign := doLargerArc = useIncreasingAngles ifTrue: [ -1 ] ifFalse: [ 1 ].
	bigSqR := (bigSqRNumerator / bigSqRDenominator max: 0.0) sqrt * sign.		"This only can be < 0.0 because of rounding errors. Take 0.0 in such cases."
	cxp := bigSqR * rx * y1p / ry.
	cyp := 0.0 - (bigSqR * ry * x1p / rx).

	"step 3"
	cx := (cosPhi * cxp) - (sinPhi * cyp) + ((x1+x2) * 0.5).
	cy := (sinPhi * cxp) + (cosPhi * cyp) + ((y1+y2) * 0.5).

	"step 4"
	theta1 := (((x1p - cxp) / rx) @ ((y1p - cyp) / ry)) theta.
	theta2 := (((0 - x1p - cxp) / rx) @ (( 0 -y1p - cyp) / ry)) theta.
	deltaTheta := theta2 - theta1.
	useIncreasingAngles = (deltaTheta > 0) ifFalse: [
		deltaTheta > 0
			ifTrue: [ deltaTheta := deltaTheta - Float pi - Float pi ]
			ifFalse: [ deltaTheta := deltaTheta + Float pi + Float pi ]
		].

	self arcCenter: cx@cy radius: rx@ry rotationAngle: phi start: theta1 sweep: deltaTheta
]

{ #category : 'morphic' }
VectorEngine >> canvas: aMorphicCanvas [
	"Only needed if we are being called from a Canvas"

	canvas := aMorphicCanvas
]

{ #category : 'paths' }
VectorEngine >> circleCenter: aPoint radius: mr [
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			circleCenter: 200 @ 150 radius: 80].
Display forceToScreen
"

	self arcCenter: aPoint radius: mr@mr rotationAngle: 0.0 start: 0.0 sweep: Float twoPi.
]

{ #category : 'private' }
VectorEngine >> contourDataIndexForUtf32: codePoint contourDataIndexes: contourDataIndexes [
	"contourDataIndexes is designed to be directly accessible via UTF-8 bytes of a CodePoint.
	This allows for a compact and efficient packing of TrueType contour data.
	In order to be used for a plain Code Point (as expressed in UTF-32), the UTF-8 bytes of
	the code point need to be extracted and used to access contourDataIndexes."

	| i utf8Byte1 utf8Byte2 utf8Byte3 utf8Byte4 |
	codePoint <= 16r7F
		ifTrue: [
			utf8Byte1 := codePoint.
			i := contourDataIndexes at: utf8Byte1 + 1 ]
		ifFalse: [
			codePoint <= 16r7FF
				ifTrue: [
					utf8Byte1 := (codePoint bitShift: -6) bitOr: 2r11000000.
					utf8Byte2 := (codePoint bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte1 + 1.
					i < 0 ifFalse: [ ^0 ].
					i := contourDataIndexes at: i negated + utf8Byte2 + 1 ]
				ifFalse: [
					codePoint <= 16rFFFF
						ifTrue: [
							utf8Byte1 := (codePoint bitShift: -12) bitOr: 2r11100000.
							utf8Byte2 := ((codePoint bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
							utf8Byte3 := (codePoint bitAnd: 2r00111111) bitOr: 2r10000000.
							i := contourDataIndexes at: utf8Byte1 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte2 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte3 + 1 ]
						ifFalse: [
							utf8Byte1 := (codePoint bitShift: -18) bitOr: 2r11110000.
							utf8Byte2 := ((codePoint bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
							utf8Byte3 := ((codePoint bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
							utf8Byte4 := (codePoint bitAnd: 2r00111111) bitOr: 2r10000000.
							i := contourDataIndexes at: utf8Byte1 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte2 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte3 + 1.
							i < 0 ifFalse: [ ^0 ].
							i := contourDataIndexes at: i negated + utf8Byte4 + 1 ]]].
	^i
]

{ #category : 'paths' }
VectorEngine >> cubicBezierTo: toPoint control1: controlPoint1 control2: controlPoint2 [
	"Cubic Bezier
	http://en.wikipedia.org/wiki/Bezier_curve"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			cubicBezierTo: 300@200 control1: 110@110 control2: 200@200 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@50.
		engine
			cubicBezierTo: 600@100
			control1: 150@300
			control2: 600@400 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@100.
		engine
			cubicBezierTo: 150@400
			control1: 500@400
			control2: -150@300 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@100.
		engine
			cubicBezierTo: 150@400
			control1: 600@400
			control2: -150@300 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 10 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@100.
		engine
			cubicBezierTo: 400@150
			control1: 400@4600
			control2: 300 @ -15].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 3 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.8) do: [ :engine |
		engine moveTo: 100@100.
		engine
			cubicBezierTo: 120@1000
			control1: 140@200
			control2: 130@1400].
Display forceToScreen
	"

	self cubicBezierFromX: currentX y: currentY toX: toPoint x y: toPoint y control1X: controlPoint1 x y: controlPoint1 y control2X: controlPoint2 x y: controlPoint2 y.
	currentX := toPoint x.
	currentY := toPoint y.
]

{ #category : 'private' }
VectorEngine >> displayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	
	| nextGlyphX advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours |
	nextGlyphX := destX.
	startIndex to: stopIndex do: [ :charIndex | | latin1Code i |
		latin1Code := (aString at: charIndex) codePoint.
		i := contourDataIndexes at: latin1Code + 1.
		i < 1 ifTrue: [ i := 1 ].
		advanceWidth := contourData at: i. i := i + 1.
		boundsLeft := contourData at: i. i := i + 1.
		boundsRight := contourData at: i. i := i + 1.
		boundsBottom := contourData at: i. i := i + 1.
		boundsTop := contourData at: i. i := i + 1.
		numContours := (contourData at: i) asInteger. i := i + 1.
		self glyphContours: numContours atx: nextGlyphX y: destY scalex: sx y: sy contourData: contourData i: i.
		nextGlyphX := nextGlyphX + (advanceWidth * sx) ].
	^nextGlyphX
]

{ #category : 'private' }
VectorEngine >> displayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	
	| nextGlyphX advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours deltaXToCenter deltaXForDiacriticalMark deltaYForDiacriticalMark deltaXForBaseGlyph baseBoundsTop |
	deltaXForBaseGlyph := 0.0.
	baseBoundsTop := 0.0.
	nextGlyphX := destX.
	startIndex to: stopIndex do: [ :utf32Index | | utf32 i |
		utf32 := aWordArray at: utf32Index.
		
		"$i followed by a Combining Diacritical Mark is replaced by CodePoint 16r131 Dotless i
		This should also be done for VectorEngineWithPlugin"
		(utf32 = 16r69 and: [ utf32Index < stopIndex ] and: [
			(aWordArray at: utf32Index+1) between: 16r300 and: 16r36F ])
				ifTrue: [ utf32 := 16r131 ].
		i :=  self contourDataIndexForUtf32: utf32 contourDataIndexes: contourDataIndexes.
		advanceWidth := contourData at: i. i := i + 1.
		boundsLeft := contourData at: i. i := i + 1.
		boundsRight := contourData at: i. i := i + 1.
		boundsBottom := contourData at: i. i := i + 1.
		boundsTop := contourData at: i. i := i + 1.
		numContours := (contourData at: i) asInteger. i := i + 1.
		
		"TrueType doesn't speciy how to position diacritics wrt base character glyph.
		Horizontally center diacriticals wrt base glyph.
		See
			basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:
			formGlyphForUtf8Byte1:byte2:byte3:byte4:pixelSize:
			displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring:
		This should also be done for VectorEngineWithPlugin
		Apparently OpenType can do better than TrueType, as it includes the GPOS table..."
		deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.51).
		utf32 = 16r300 ifTrue: [	"Combining Grave Accent"
			deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.43) ].
		utf32 = 16r301 ifTrue: [ 	"Combining Acute Accent"
			deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.65) ].
		"DoubleWidth code points that combine above previous and following characters.
		Codepoints between 16r035C and 16r0363.
		Example: o͢o
		Don't center them"
		(utf32 between: 16r035C and: 16r0363) ifTrue: [
			deltaXToCenter := 0.0 ].
				
		deltaXForDiacriticalMark := 0.0.
		deltaYForDiacriticalMark := 0.
		advanceWidth = 0.0
			ifFalse: [ "Regular glyph"
				deltaXForBaseGlyph := deltaXToCenter.
				baseBoundsTop := boundsTop ]
			ifTrue: [ "Diacritical mark"
				deltaXForBaseGlyph = 0.0  "For instance, if the first code point to display is a diacritic, like when building a FormGlyph for it"
					ifFalse: [
						deltaXForDiacriticalMark := deltaXToCenter - deltaXForBaseGlyph * sx.
						baseBoundsTop > boundsBottom ifTrue: [
							deltaYForDiacriticalMark :=  baseBoundsTop - boundsBottom + 100 * sy ]]].
		
		self
			glyphContours: numContours
			atx: nextGlyphX + deltaXForDiacriticalMark y: destY + deltaYForDiacriticalMark
			scalex: sx y: sy
			contourData: contourData i: i.
		
		nextGlyphX := nextGlyphX + (advanceWidth * sx) ].
	^nextGlyphX
]

{ #category : 'private' }
VectorEngine >> displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	
	| nextGlyphX byte1 byte2OrNil baseIndex advanceWidth boundsLeft boundsRight boundsBottom boundsTop numContours deltaXToCenter deltaXForDiacriticalMark deltaYForDiacriticalMark deltaXForBaseGlyph baseBoundsTop |
	deltaXForBaseGlyph := 0.0.
	baseBoundsTop := 0.0.
	nextGlyphX := destX.
	byte1 := nil.
	byte2OrNil := nil.
	baseIndex := 0.
	byteStartIndex to: byteStopIndex do: [ :byteIndex | | byte i |
		byte := aByteArray at: byteIndex.
		byte1 isNil ifTrue: [byte1 := byte ] ifFalse: [ byte2OrNil isNil ifTrue: [ byte2OrNil := byte ]].
		i := contourDataIndexes at: baseIndex + byte + 1.
		
		"$i followed by a Combining Diacritical Mark is replaced by CodePoint 305 Dotless i, UTF-8 #[196 177]
		(UnicodeString fromUtf8Bytes: #[105 105 204 129 32 105 204 128 32 105 204 130 32 105 204 131 32
			105 204 132 32 105 204 133 32 105 204 134 32 105 204 135 32 105 204 136 32 105 204 137 ]) edit
		This should also be done for VectorEngineWithPlugin"
		(byte1 = 105 and: [ byteIndex < (byteStopIndex-1) ] and: [
			(aByteArray at: byteIndex+1) = 204 or: [ (aByteArray at: byteIndex+1) = 205 and: [ (aByteArray at: byteIndex+2) < 176 ]]])
				ifTrue: [
					i := contourDataIndexes at: 196 + 1.
					i := contourDataIndexes at: i negated + 177 + 1 ].
		
		i < 0
			ifTrue: [ baseIndex := i negated ]
			ifFalse: [
				advanceWidth := contourData at: i. i := i + 1.
				boundsLeft := contourData at: i. i := i + 1.
				boundsRight := contourData at: i. i := i + 1.
				boundsBottom := contourData at: i. i := i + 1.
				boundsTop := contourData at: i. i := i + 1.
				numContours := (contourData at: i) asInteger. i := i + 1.
				"TrueType doesn't speciy how to position diacritics wrt base character glyph.
				Horizontally center diacriticals wrt base glyph.
				See
					basicDisplayUtf8:fromByte:toByte:at:trueTypeFont:
					formGlyphForUtf8Byte1:byte2:byte3:byte4:pixelSize:
					displayUtf32:from:to:at:trueTypeFont:color:scale:preventMirroring:
				This should also be done for VectorEngineWithPlugin
				Apparently OpenType can do better than TrueType, as it includes the GPOS table..."
				deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.51).
				byte1 = 204 ifTrue: [
					byte2OrNil = 128 ifTrue: [ "Combining Grave Accent"
						deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.43) ].
					byte2OrNil = 129 ifTrue: [ "Combining Acute Accent"
						deltaXToCenter := advanceWidth - (boundsRight+boundsLeft * 0.65) ]].
				"DoubleWidth code points that combine above previous and following characters.
				Codepoints between 16r035C and 16r0363.
				Example: o͢o
				Don't center them"
				(byte1 = 205 and: [ byte2OrNil between: 156 and: 163 ]) ifTrue: [
					deltaXToCenter := 0.0 ].
				
				deltaXForDiacriticalMark := 0.0.
				deltaYForDiacriticalMark := 0.
				advanceWidth = 0.0
					ifFalse: [ "Regular glyph"
						deltaXForBaseGlyph := deltaXToCenter.
						baseBoundsTop := boundsTop ]
					ifTrue: [ "Diacritical mark"
						deltaXForBaseGlyph = 0.0  "For instance, if the first code point to display is a diacritic, like when building a FormGlyph for it"
							ifFalse: [
								deltaXForDiacriticalMark := deltaXToCenter - deltaXForBaseGlyph * sx.
								baseBoundsTop > boundsBottom ifTrue: [
									deltaYForDiacriticalMark :=  baseBoundsTop - boundsBottom + 100 * sy ]]].
				
				self
					glyphContours: numContours
					atx: nextGlyphX + deltaXForDiacriticalMark y: destY + deltaYForDiacriticalMark
					scalex: sx y: sy
					contourData: contourData i: i.
				
				nextGlyphX := nextGlyphX + (advanceWidth * sx).
				baseIndex := 0.
				byte1 := nil.
				byte2OrNil := nil ]].
	^nextGlyphX
]

{ #category : 'private' }
VectorEngine >> doPath: pathCommandsBlock ensureClosePath: ensureClosePath [
	"Evaluate a sequence of path commands. Path parameters already set."

	self initializePath.
	pathCommandsBlock valueWithPossibleArgument: (canvas ifNil:[self]).
	self finishPath: ensureClosePath
]

{ #category : 'text and multi paths' }
VectorEngine >> drawByteString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring [
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f s |
'Try all FOUR of the,!'.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
s := 'Hello World! (Ascii Only in this example. Latin-1 requires an additional single byte table in the Font, that Cuis does not include.' asByteString.
e drawByteString: s from: 1 to: s size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale asFloat.
	sy := scale * trueTypeScale asFloat.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.	
	contourDataIndexes := aTTFontDescription contourDataIndexesByLatin1.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			displayByteString: aString from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.
]

{ #category : 'text and multi paths' }
VectorEngine >> drawUtf32String: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring [
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f utf32 |
'Try All four!'.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
utf32 _ #(4314 4317 4320 4308 4315 32 4312 4324 4321 4323 4315 32 4307 4317 4314 4317 4320 32 4321 4312 4311 32 4304 4315 4308 4311 44 32 4327 4323 4317 32 4323 4311 32 4307 4312 4330 4304 4311 32 4304 4314 4312 4327 4323 4312 4307 44 32 4304 4307 32 4314 4312 4305 4320 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4320 4308 4324 4320 4308 4336 4308 4316 4307 4323 4316 4311 32 4308 4321 4311 46 32 4314 4308 4306 4308 4320 4308 32 4330 4317 4320 4324 4317 4320 4304 32 4308 4324 4324 4312 4330 4312 4304 4316 4311 4323 4320 32 4304 4311 32 4308 4304 4315 44 32 4316 4317 32 4327 4323 4304 4321 32 4315 4308 4316 4304 4316 4307 4320 4312 32 4320 4308 4324 4308 4320 4320 4308 4316 4311 4323 4320 32 4327 4323 4312 46 32 4311 4308 32 4324 4323 4311 4308 4316 4311 32 4315 4317 4314 4314 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4309 4312 4321 46 32 4309 4312 4315 32 4307 4308 4316 4312 4327 4323 4308 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4311 4336 4308 4317 4324 4336 4320 4304 4321 4311 4323 4321 32 4308 4334 44 32 4308 4304 32 4336 4304 4321 32 4309 4317 4314 4323 4315 4323 4321 32 4324 4304 4321 4311 4312 4307 4312 4312 32 4307 4308 4321 4308 4320 4323 4316 4311 46 32 4308 4334 32 4317 4324 4324 4312 4330 4312 4312 4321 32 4311 4317 4320 4327 4323 4304 4311 4317 4321 32 4315 4308 4312 44 32 4307 4317 4330 4311 4323 4321 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4312 4316 32 4327 4323 4317 46 32 4321 4323 4315 4315 4317 32 4311 4320 4304 4330 4311 4304 4311 4317 4321 32 4308 4312 32 4312 4323 4321 44 32 4323 4311 32 4317 4315 4316 4308 4321 32 4309 4312 4320 4311 4323 4311 4308 32 4307 4308 4311 4320 4304 4334 4312 4311 46) asWordArray.
e drawUtf32String: utf32 from: 1 to: utf32 size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"
"
| e f utf8a utf8b utf8c utf32a utf32b utf32c |
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
utf8a := #[105 105 204 129 32 105 204 128 32 105 204 130 32 105 204 131 32 105 204 132 32 105 204 133 32 105 204 134 32 105 204 135 32 105 204 136 32 105 204 137 ].
utf8b := #[97 	195 169 97 195 168 	97 	195 177 97 	195 145 195 188 64 97 	101 204 129 97 101 204 128 97 	110 204 131 97 	78 204 131 117 204 136 97	].
utf8c := #[97 	195 169 97 195 168 	97 	195 177 97 	195 145 195 188 105 195 173 64 97 	101 204 129 97 101 204 128 97 	110 204 131 97 	78 204 131 117 204 136 97 105 105 204 129 	].
utf32a := #(105 105 769 32 105 768 32 105 770 32 105 771 32 105 772 32 105 773 32 105 774 32 105 775 32 105 776 32 105 777) asWordArray.
utf32b :=  #(97 233 97 232 97 241 97 209 252 64 97 101 769 97 101 768 97 110 771 97 78 771 117 776 97) asWordArray.
utf32c :=  #(97 233 97 232 97 241 97 209 252 105 237 64 97 101 769 97 101 768 97 110 771 97 78 771 117 776 97 105 105 769) asWordArray.
e drawUtf8String: utf8a fromByte: 1 toByte: utf8a size at: 10@120 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32a from: 1 to: utf32a size at: 10@220 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf8String: utf8b fromByte: 1 toByte: utf8b size at: 10@320 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32b from: 1 to: utf32b size at: 10@420 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf8String: utf8c fromByte: 1 toByte: utf8c size at: 10@520 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32c from: 1 to: utf32c size at: 10@620 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			displayUtf32: aWordArray from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.
]

{ #category : 'text and multi paths' }
VectorEngine >> drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring [
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
	"
	| e f s byteStartIndex byteStopIndex |
	'-----Try all FOUR of them!' print.
	Display fillColor: Color veryLightGray.
	e := VectorEngineWholePixel onForm: Display.
	e := VectorEngineSubPixel onForm: Display.
	e := VectorEngineWithPluginWholePixel onForm: Display.
	e := VectorEngineWithPluginSubPixel onForm: Display.
	f := 	FontFamily defaultFamilyAndPointSize fontDescription.
	s := 'Hello World! This is TrueType text rasterization in VectorEngine.' asUnicodeString.
	byteStartIndex := s byteIndexAt: 1.
	byteStopIndex := (s byteIndexAt: s size+1)-1.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@150 trueTypeFont: f color: Color random scale: 130 preventMirroring: true] timeToRun print.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@300 trueTypeFont: f color: Color random strokeWidth: 2 strokeColor: Color black scale: 130 preventMirroring: true] timeToRun print.
	Display forceToScreen.
	"
	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := aPoint x asFloat. "If #fillColor:do: decides to evaluate this block more than once, set correct start x each time."
		nextGlyphX := self
			displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.
]

{ #category : 'text and multi paths' }
VectorEngine >> drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor strokeWidth: sw strokeColor: sc scale: scale preventMirroring: doPreventMirroring [
	"Allows adding a colored border to text display.
	TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
	"
	| e f s byteStartIndex byteStopIndex |
	'-----Try all FOUR of them!' print.
	Display fillColor: Color veryLightGray.
	e := VectorEngineWholePixel onForm: Display.
	e := VectorEngineSubPixel onForm: Display.
	e := VectorEngineWithPluginWholePixel onForm: Display.
	e := VectorEngineWithPluginSubPixel onForm: Display.
	f := 	FontFamily defaultFamilyAndPointSize fontDescription.
	s := 'Hello World! This is TrueType text rasterization in VectorEngine.' asUnicodeString.
	byteStartIndex := s byteIndexAt: 1.
	byteStopIndex := (s byteIndexAt: s size+1)-1.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@150 trueTypeFont: f color: Color random scale: 130 preventMirroring: true] timeToRun print.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@300 trueTypeFont: f color: Color random strokeWidth: 2 strokeColor: Color black scale: 130 preventMirroring: true] timeToRun print.
	Display forceToScreen.
	"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self strokeWidth: sw color: sc fillColor: aColor do: [
		nextGlyphX := aPoint x asFloat. "If #strokeWidth:color:fillColor:do: decides to evaluate this block more than once, set correct start x each time."
		nextGlyphX := self
			displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.
]

{ #category : 'paths' }
VectorEngine >> ellipseCenter: aPoint radius: radiusPoint [
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50].
Display forceToScreen
"

	self arcCenter: aPoint radius: radiusPoint rotationAngle: 0.0 start: 0.0 sweep: Float twoPi.
]

{ #category : 'paths' }
VectorEngine >> ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta [
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: -0.5 ].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	geometryTransformation: (AffineTransformation withRadians: 1 scale: 1.2 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: -0.5 ].
Display forceToScreen
"

	self arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: 0.0 sweep: Float twoPi.
]

{ #category : 'paths' }
VectorEngine >> ellipseCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle [
	"Currently draws a line from pathCurrentX@pathCurrentY to start (if startAngle ~= 0.0 or sweepAngle ~= Float twoPi).
	This could be useful for proper fills. But might not be what is desired in some cases.
	Implement desired behavior when clear."
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: 0.0 start: 0.1 sweep: 6 ].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	geometryTransformation: (AffineTransformation withRadians: 0.5 scale: 1.0 position: 200@100);
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@150 radius: 100@50 rotationAngle: 0.0 start: 0.1 sweep: 6].
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2.0 color: Color red do: [ :engine |
		engine
			ellipseCenter: 200@350 radius: 100@50 rotationAngle: 0.5 start: 0.1 sweep: 6 ].
Display forceToScreen.
"

	self arcCenter: aPoint radius: radiusPoint rotationAngle: theta start: startAngle sweep: sweepAngle
]

{ #category : 'accessing' }
VectorEngine >> geometryTransformation: aGeometryTransformation [

	geometryTransformation := aGeometryTransformation.
]

{ #category : 'private' }
VectorEngine >> glyphContours: numContours atx: nextGlyphX y: nextGlyphY scalex: sx y: sy contourData: contourData i: contourDataIndex [

		| i contourStart numBeziers ttMoveToX ttMoveToY x y |
		i := contourDataIndex.
		numContours timesRepeat: [
			numBeziers := (contourData at: i) asInteger. i := i + 1.
			ttMoveToX := contourData at: i. i := i + 1.
			ttMoveToY := contourData at: i. i := i + 1.
			x := ttMoveToX * sx + nextGlyphX.
			y := ttMoveToY * sy + nextGlyphY.
			contourStart := x@y.
			self moveTo: contourStart.
			numBeziers timesRepeat: [ | cx cy ttControlX ttControlY ttEndX ttEndY |
				ttEndX := contourData at: i. i := i + 1.
				ttEndY := contourData at: i. i := i + 1.
				ttControlX := contourData at: i. i := i + 1.
				ttControlY := contourData at: i. i := i + 1.
				cx := ttControlX * sx + x.
				cy := ttControlY * sy + y.
				x := ttEndX * sx + x.
				y := ttEndY * sy + y.
				self quadraticBezierTo: x@y control: cx@cy ].
			self lineTo: contourStart ].
]

{ #category : 'private' }
VectorEngine >> initializePath [

	firstX := firstY := Float nan.
	currentX := currentY := 0.0.
	self newTrajectoryFragment.
]

{ #category : 'paths' }
VectorEngine >> lineTo: aPoint [
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			lineTo: 300@150 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 25.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			lineTo: 500@200 ].
Display forceToScreen
"
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 2 color: (Color black alpha: 0.7) fillColor: (Color orange alpha: 0.4) do: [ :engine |
		engine moveTo: 30@115.
		engine lineTo: 45@190.
		engine lineTo: 180@130 ].
Display forceToScreen
"
	self lineFromX: currentX y: currentY toX: aPoint x y: aPoint y.
	currentX := aPoint x.
	currentY := aPoint y.
]

{ #category : 'paths' }
VectorEngine >> lineToFirstPoint [
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			moveTo: 100@100;
			arcTo: 200@200 radius: 200@100 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			lineToFirstPoint ].
Display forceToScreen
"

	self lineFromX: currentX y: currentY toX: firstX y: firstY.
	currentX := firstX.
	currentY := firstY.
]

{ #category : 'accessing' }
VectorEngine >> morphBoundsAfterDraw [
	"In targetForm coordinates.
	Doesn't account for any clipping rect (neither clip rect parameter to bound drawn area, nor possible clipping to owner shape or bounds).
	Doesn't include any protruding submorphs.
	Only valid _after_ drawing currentMorph."

	^morphBoundsLeft @ morphBoundsTop corner: morphBoundsRight @ morphBoundsBottom + 1
]

{ #category : 'paths' }
VectorEngine >> moveTo: aPoint [
"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			lineTo: 200@150;
			moveTo: 210@150;
			lineTo: 300@200 ].
Display forceToScreen
"
	"A discontinuity in the trajectory"
	"We are starting a new trajectory, or at a discontinuity in trajectory.
	Eventually #closeTrajectory should draw a line to here"

	"Next point will be first in a new trajectory fragment"
	self newTrajectoryFragment.
	firstX := aPoint x.
	firstY := aPoint y.
	currentX := firstX.
	currentY := firstY.
]

{ #category : 'private' }
VectorEngine >> newTrajectoryFragment [

]

{ #category : 'text and multi paths' }
VectorEngine >> pathSequence: aFloat32Array [
	"
	(VectorEngine onForm: Display subPixelAntiAliasing: true)
		geometryTransformation: (AffineTransformation withScale: 14 position: 100@100);
		strokeWidth: 3 color: Color black do: [ :engine |
			engine
				pathSequence: `#[
					0.0 15.0 15.0
					3.0 30.0 30.0 15.0 23.3 21.7 30.0
					3.0 45.0 15.0 38.3 30.0 45.0 23.3
					3.0 30.0 0.0 45.0 6.7 38.3 0.0
					3.0 15.0 15.0 21.7 0.0 15.0 6.7
					0.0 22.0 19.0
					2.0 30.0 24.0 24.5 24.0
					2.0 38.0 19.0 35.5 24.0
					0.0 25.0 10.0
					1.0 25.0 12.0
					0.0 35.0 10.0
					1.0 35.0 12.0
					] asFloat32Array`
				].
	Display forceToScreen
	"
	| size i |
	size := aFloat32Array size.
	i := 1.
	[ i < size ] whileTrue: [
		| commandType p1x p1y p2x p2y p3x p3y |
		commandType := aFloat32Array at: i. i := i + 1.
		commandType caseOf: {
			[0.0] -> [
				i < size ifFalse: [ ^self "failure" ].
				p1x := aFloat32Array at: i. i := i + 1.
				p1y := aFloat32Array at: i. i := i + 1.
				self moveTo: p1x@p1y].
			[1.0] -> [
				i < size ifFalse: [ ^self "failure" ].
				p1x := aFloat32Array at: i. i := i + 1.
				p1y := aFloat32Array at: i. i := i + 1.
				self lineTo: p1x@p1y].
			[2.0] -> [
				i+2 < size ifFalse: [ ^self "failure" ].
				p1x := aFloat32Array at: i. i := i + 1.
				p1y := aFloat32Array at: i. i := i + 1.
				p2x := aFloat32Array at: i. i := i + 1.
				p2y := aFloat32Array at: i. i := i + 1.
				self quadraticBezierTo: p1x@p1y control: p2x@p2y ].
			[3.0] -> [
				i+4 < size ifFalse: [ ^self "failure" ].
				p1x := aFloat32Array at: i. i := i + 1.
				p1y := aFloat32Array at: i. i := i + 1.
				p2x := aFloat32Array at: i. i := i + 1.
				p2y := aFloat32Array at: i. i := i + 1.
				p3x := aFloat32Array at: i. i := i + 1.
				p3y := aFloat32Array at: i. i := i + 1.
				self cubicBezierTo: p1x@p1y control1: p2x@p2y control2: p3x@p3y ]}
		otherwise: [ ^self "failure"]].
]

{ #category : 'private' }
VectorEngine >> pvtStrokeWidth: aNumber hop: aFloat [

	strokeWidth := aNumber.
	hop := aFloat.
]

{ #category : 'paths' }
VectorEngine >> quadraticBezierTo: toPoint control: controlPoint [
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			quadraticBezierTo: 120@130 control: 120@110 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 1.0 color: Color black do: [ :engine |
		engine
			moveTo: 100@100;
			quadraticBezierTo: 120@130 control: 1300@110 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 3.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			moveTo: 100@270;
			quadraticBezierTo: 200@130 control: 110@600 ].
Display forceToScreen
	"
	"
(VectorEngine onForm: Display subPixelAntiAliasing: true)
	strokeWidth: 3.0 color: Color black fillColor: Color red do: [ :engine |
		engine
			moveTo: 100@550;
			quadraticBezierTo: 200@130 control: 110@600 ].
Display forceToScreen
	"
	self quadraticBezierFromX: currentX y: currentY toX: toPoint x y: toPoint y controlX: controlPoint x y: controlPoint y.
	currentX := toPoint x.
	currentY := toPoint y.
]

{ #category : 'private' }
VectorEngine >> updateMorphBoundsLeft: l top: t right: r bottom: b [
	"For internal framework use only"

	morphBoundsLeft := morphBoundsLeft min: l.
	morphBoundsTop := morphBoundsTop min: t.
	morphBoundsRight := morphBoundsRight max: r.
	morphBoundsBottom := morphBoundsBottom max: b.
]

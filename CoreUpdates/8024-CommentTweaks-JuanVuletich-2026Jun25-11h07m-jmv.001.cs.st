'From Cuis7.9 [latest update: #8022] on 25 June 2026 at 11:17:47 am'!

!PathCommand commentStamp: '<historical>' prior: 0!
PathCommand is a reification of the canvas API.
Check also #pathSequence:, which provides an higher performance take on similar services. Check the 'drawing - icons' class category in AbstractVectorCanvas.!


!AbstractVectorCanvas methodsFor: 'paths - sequences' stamp: 'jmv 25/Jun/2026, 2:16:47 pm (UTC)'!
pathSequence: aFloat32Array
	"This method allows high performance drawing of arbitrary shapes. It uses little memory and is very fast.
	It is recommended to use it with literal Float32Arrays as argument.
	See comment at VectorEngine>>#pathSequence:
	See, for example, #drawHandLarge

	'Circle approximation by bezier curves
	https://spencermortensen.com/articles/bezier-circle/'.
	(VectorCanvas onForm: Display)
		geometryTransformation: (AffineTransformation withScale: 14 position: 100@100);
		strokeWidth: 3 color: Color random do: [ :engine |
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
	engine pathSequence: aFloat32Array! !


!VectorEngine methodsFor: 'text and multi paths' stamp: 'jmv 25/Jun/2026, 2:16:17 pm (UTC)'!
pathSequence: aFloat32Array
	"This method allows high performance drawing of arbitrary shapes. It uses little memory and is very fast.
	It is recommended to use it with literal Float32Arrays as argument.

	Commands are:
	0.0: #moveTo: aPoint
	1.0: #lineTo: aPoint
	2.0: #quadraticBezierTo: toPoint control: controlPoint
	3.0: #cubicBezierTo: toPoint control1: controlPoint1 control2: controlPoint2

	Circle, Ellipse and Arc primitives are currently not supported.

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
		otherwise: [ ^self "failure"]].! !


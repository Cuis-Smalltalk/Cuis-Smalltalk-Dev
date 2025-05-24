'From Cuis7.3 [latest update: #7092] on 27 March 2025 at 8:38:44 am'!

!Sample01Star methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:36:27'!
drawOn: aCanvas
	"A 5 point star.
	Feel free to experiment with this code.
	The worst thing that could happen is that your morph breaks. That will only mean extra fun!!
	Sample01Star new openInHand
	"

	aCanvas strokeWidth: 12 color: `Color lightOrange` do: [
	"aCanvas fillColor: Color lightOrange do: ["
	"aCanvas strokeWidth: 12 color: Color lightOrange fillColor: Color yellow do: ["
		aCanvas
				moveTo: `(Point rho: 100 theta: 90 degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*2+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*4+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*6+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: (360/5*8+90) degreesToRadians)`;
				lineTo: `(Point rho: 100 theta: 90 degreesToRadians)`.
		].! !


!Sample02Bezier methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:46:23'!
drawOn: aCanvas
	"Two Bezier curves. A Quadratic one (with start, end and a single control point), and a Cubic one (with start, end, and two control points)
	Sample02Bezier new openInHand
	"

	aCanvas strokeWidth: 2 color: `Color green` strokeDashArray: #(10 10) strokeDashArrayOffset: dashOffset do: [
		aCanvas
				moveTo: aStart;
				quadraticBezierTo: aEnd control: aControlPoint ].
	aCanvas strokeWidth: 1 color: `Color darkGray` strokeDashArray: #(1 3) strokeDashArrayOffset: Random next * 4 do: [
		aCanvas
				moveTo: aStart;
				lineTo: aControlPoint;
				lineTo: aEnd ].

	aCanvas strokeWidth: 3 color: `Color red` strokeDashArray: #(10 35) strokeDashArrayOffset: dashOffset do: [
		aCanvas
				moveTo: bStart;
				cubicBezierTo: bEnd control1: bControlPoint1 control2: bControlPoint2 ].
	aCanvas strokeWidth: 1 color: `Color darkGray` strokeDashArray: #(15 8) strokeDashArrayOffset: dashOffset do: [
		aCanvas
				moveTo: bStart;
				lineTo: bControlPoint1;
				lineTo: bControlPoint2;
				lineTo: bEnd ].! !


!Sample03Smiley methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:35:40'!
drawOn: aCanvas
	"Example of the #pathSequence drawing operation, that can represent many drawing operations
			in one compact literal, and draw them at once.
	0.0		x	y									Move pen to position x@y
	1.0		x	y									Draw a line to position x@y
	2.0		toX	toY	controlX		controlY						Draw a Quadratic Bezier Curve
	3.0		toX	toY	control1X	control1Y			control2X	control2Y			Draw a Cubic Bezier Curve
	See #circleApproximationExamples
	Sample03Smiley new openInHand
	"

	aCanvas strokeWidth: 30 color: `Color gray` fillColor: `Color lightGreen duller` do: [
		aCanvas
				pathSequence: `#[
					0.0  150.0  150.0
					3.0  300.0  300.0  150.0  233.0  217.0  300.0
					3.0  450.0  150.0  380.3  300.0  450.0  233.0
					3.0  300.0  0.0      450.0  67.0    383.0  0.0
					3.0  150.0  150.0  210.7  0.0      150.0  67.0
					] asFloat32Array`
		].

	aCanvas strokeWidth: 30 color: `Color gray` do: [
		aCanvas
				pathSequence: `#[
					0.0 220.0 190.0
					2.0 300.0 240.0 245.0 240.0
					2.0 380.0 190.0 355.0 240.0
					0.0 250.0 100.0
					1.0 250.0 120.0
					0.0 350.0 100.0
					1.0 350.0 120.0
					] asFloat32Array`
		].! !


!Sample04Pacman methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:47:22'!
drawOn: aCanvas

	aCanvas fillColor: color do: [
		aCanvas circleCenter: `30@60` radius: 15.
		aCanvas
			moveTo: `0@0`;
			lineTo: `100@ -50`;
			arcTo: `100@ 50` radius: 110 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			lineTo: `0@0`.
		].! !


!Sample07Clipping methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:48:13'!
drawOn: aCanvas

	aCanvas strokeWidth: 15 color: `Color green darker` fillColor: `Color red darker` do: [
		aCanvas
			roundRectangleMinX: -10 maxX: 220 minY: -10 maxY: 120  rx: 120 ry: 50 ]! !


!Sample09Clock methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:33:11'!
drawOn: aCanvas

	| f |
	aCanvas
		strokeWidth: 10 color: `Color lightCyan`
		fillColor: `Color veryVeryLightGray lighter` do: [ :canvas |
			canvas circleCenter:  0@0 radius: 100 ].
	f := FontFamily familyName: 'DejaVu Sans' pointSize: 14.
	aCanvas drawString: 'XII' at: `-13 @ 58` font: f color: `Color brown`.
	aCanvas drawString: 'III' at: `73 @ -24` font: f color: `Color brown`.
	aCanvas drawString: 'VI' at: `-9 @ -106` font: f color: `Color brown`.
	aCanvas drawString: 'IX' at: `-90 @ -24` font: f color: `Color brown`.

	f := FontFamily familyName: 'Alex Brush' pointSize: 16.
	aCanvas drawString: 'Chaque Heure' atCenterX: `0@ 18` font: f color: `Color black`.
	f := FontFamily familyName: 'Alex Brush' pointSize: 8.
	aCanvas drawString: 'pour la Minorie' atCenterX: `0@ -60` font: f color: `Color black`.! !


!Sample09ClockHourHand methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:34:17'!
drawOn: aCanvas

	aCanvas fillColor: `Color darkGray` do: [
		aCanvas
			moveTo: `0 @ 10`;
			lineTo: `-5 @ 0`;
			lineTo: `0 @ -50`;
			lineTo: `5 @ 0`;
			lineTo: `0 @ 10` ].! !


!Sample09ClockMinuteHand methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:35:01'!
drawOn: aCanvas

	aCanvas fillColor: `Color darkGray` do: [
		aCanvas
			moveTo: `0 @ 8`;
			lineTo: `-4 @ 0`;
			lineTo: `0 @ -82`;
			lineTo: `4 @ 0`;
			lineTo: `0 @ 8` ].! !


!Sample09ClockSecondHand methodsFor: 'drawing' stamp: 'jmv 3/21/2025 17:35:13'!
drawOn: aCanvas

	aCanvas strokeWidth: 3.5 color: Color red do: [
		aCanvas
			moveTo: `0 @ 0`;
			lineTo: `0 @ -85` ].! !


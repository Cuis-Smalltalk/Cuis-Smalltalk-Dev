'From Cuis 6.0 [latest update: #5854] on 14 June 2023 at 2:59:43 pm'!

!Sample02Bezier methodsFor: 'drawing' stamp: 'jmv 6/14/2023 14:56:42'!
drawOn: aCanvas
	"Two Bezier curves. A Quadratic one (with start, end and a single control point), and a Cubic one (with start, end, and two control points)
	Sample02Bezier new openInHand
	"

	aCanvas strokeWidth: 10 color: Color green do: [
		aCanvas
				moveTo: aStart;
				quadraticBezierTo: aEnd control: aControlPoint ].
	aCanvas strokeWidth: 2 color: Color green * 0.4 strokeDashArray: #(10 10) strokeDashArrayOffset: 0.0 do: [
		aCanvas
				moveTo: aStart;
				lineTo: aControlPoint;
				lineTo: aEnd ].

	aCanvas strokeWidth: 10 color: Color red do: [
		aCanvas
				moveTo: bStart;
				cubicBezierTo: bEnd control1: bControlPoint1 control2: bControlPoint2 ].
	aCanvas strokeWidth: 2 color: Color red * 0.1 strokeDashArray: #(1 5) strokeDashArrayOffset: 0.0 do: [
		aCanvas
				moveTo: bStart;
				lineTo: bControlPoint1;
				lineTo: bControlPoint2;
				lineTo: bEnd ].! !


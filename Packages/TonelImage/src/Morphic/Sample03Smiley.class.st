"
This class shows how to use the #pathSequence drawing operation, that can represent many drawing operations in one compact literal, and draw them at once.
See also class comment at Sample01Star.

0.0		x	y									Move pen to position x@y
1.0		x	y									Draw a line to position x@y
2.0		toX	toY	controlX		controlY						Draw a Quadratic Bezier Curve
3.0		toX	toY	control1X	control1Y			control2X	control2Y			Draw a Cubic Bezier Curve

Evaluate:
Sample03Smiley new openInWorld
"
Class {
	#name : 'Sample03Smiley',
	#superclass : 'PlacedMorph',
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample03Smiley >> drawOn: aCanvas [
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
		].
]

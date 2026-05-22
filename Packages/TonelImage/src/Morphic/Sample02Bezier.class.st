"
This class will help you familiarize with Bezier curves.
See also class comment at Sample01Star.

Each instance has random control points. There is very little code, so you can understand it all easily.

Evaluate:
10 timesRepeat: [Sample02Bezier new openInWorld].

Now have a Sample04Pacman at hand, and let it eat all these!
"
Class {
	#name : 'Sample02Bezier',
	#superclass : 'PlacedMorph',
	#instVars : [
		'aStart',
		'aEnd',
		'aControlPoint',
		'bStart',
		'bEnd',
		'bControlPoint1',
		'bControlPoint2',
		'dashOffset'
	],
	#category : 'Morphic-Examples',
	#package : 'Morphic',
	#tag : 'Examples'
}

{ #category : 'drawing' }
Sample02Bezier >> drawOn: aCanvas [
	"Two Bezier curves. A Quadratic one (with start, end and a single control point), and a Cubic one (with start, end, and two control points)
	Sample02Bezier new openInHand
	"

	aCanvas strokeWidth: 4 color: Color green dashedStrokeBits: 2r1111000010000 dashBitLength: 2 dashBitOffset: dashOffset do: [
		aCanvas
				moveTo: aStart;
				quadraticBezierTo: aEnd control: aControlPoint ].

	aCanvas strokeWidth: 1 color: Color darkGray dashedStrokeBits: 16rFFFF0000 dashBitLength: 0.3 dashBitOffset: 0 do: [
		aCanvas
				moveTo: aStart;
				lineTo: aControlPoint;
				lineTo: aEnd ].

	aCanvas strokeWidth: 3 color: Color red dashedStrokeBits: 2r1000000010000000 dashBitLength: 2 dashBitOffset: dashOffset do: [
		aCanvas
				moveTo: bStart;
				cubicBezierTo: bEnd control1: bControlPoint1 control2: bControlPoint2 ].
	aCanvas strokeWidth: 1 color: Color darkGray dashedStrokeBits: 2r01010101010101010101010101010101 dashBitLength: 2 dashBitOffset: 0 do: [
		aCanvas
				moveTo: bStart;
				lineTo: bControlPoint1;
				lineTo: bControlPoint2;
				lineTo: bEnd ].
]

{ #category : 'initialization' }
Sample02Bezier >> initialize [
	super initialize.
	aStart := 500 atRandom @ 500 atRandom.
	aEnd := 500 atRandom @ 500 atRandom.
	aControlPoint := 500 atRandom @ 500 atRandom.
	bStart := 500 atRandom @ 500 atRandom.
	bEnd := 500 atRandom @ 500 atRandom.
	bControlPoint1 := 500 atRandom @ 500 atRandom.
	bControlPoint2 := 500 atRandom @ 500 atRandom.
	dashOffset := 0.
]

{ #category : 'stepping' }
Sample02Bezier >> step [

	dashOffset := dashOffset + 1.
	self redrawNeeded.
]

{ #category : 'stepping' }
Sample02Bezier >> stepTime [
	^200
]

{ #category : 'stepping' }
Sample02Bezier >> wantsSteps [

	^ true
]

{ #category : 'geometry testing' }
Sample02Bezier >> yAxisPointsUp [
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true
]

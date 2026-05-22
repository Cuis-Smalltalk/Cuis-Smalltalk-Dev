Class {
	#name : 'PathArcCommand',
	#superclass : 'PathCommand',
	#instVars : [
		'rs',
		'xAxisRotations',
		'toPoints',
		'doLargerArcs',
		'useIncreasingAngles'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathArcCommand class >> svgName [

	^$A
]

{ #category : 'building' }
PathArcCommand >> buildFrom: commandChar stream: aStream [

	| rx ry xAxisRotation doLargerArc useIncreasingAngle x y |
	super buildFrom: commandChar stream: aStream.
	rs := OrderedCollection new.
	xAxisRotations := OrderedCollection new.
	toPoints := OrderedCollection new.
	doLargerArcs := OrderedCollection new.
	useIncreasingAngles := OrderedCollection new.
	[
		rx := aStream nextNumber.
		ry := aStream nextNumber.
		xAxisRotation := aStream nextNumber.
		doLargerArc := (aStream nextNumber = 0) not.
		useIncreasingAngle := (aStream nextNumber = 0) not.
		x := aStream nextNumber.
		y := aStream nextNumber.
		y notNil ] whileTrue: [
			rs add: rx@ry.
			xAxisRotations add: xAxisRotation degreesToRadians.
			doLargerArcs add: doLargerArc.
			useIncreasingAngles add: useIncreasingAngle.
			toPoints add: x@y ]
]

{ #category : 'drawing' }
PathArcCommand >> runOn: aCanvas [

	aCanvas multiArc: coordinatesAreRelative rs: rs xAxisRotations: xAxisRotations toPoints: toPoints doLargerArcs: doLargerArcs useIncreasingAngles: useIncreasingAngles
]

Class {
	#name : 'PathCurveToSmoothCommand',
	#superclass : 'PathCommand',
	#instVars : [
		'toPoints',
		'control2Points'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathCurveToSmoothCommand class >> svgName [

	^$S
]

{ #category : 'building' }
PathCurveToSmoothCommand >> buildFrom: commandChar stream: aStream [

	| x2 y2 x y |
	super buildFrom: commandChar stream: aStream.
	toPoints := OrderedCollection new.
	control2Points := OrderedCollection new.
	[
		x2 := aStream nextNumber.
		y2 := aStream nextNumber.
		x := aStream nextNumber.
		y := aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y.
			control2Points add: x2@y2 ]
]

{ #category : 'drawing' }
PathCurveToSmoothCommand >> runOn: aCanvas [

	aCanvas multiCurveToSmooth: coordinatesAreRelative toPoints: toPoints control2Points: control2Points
]

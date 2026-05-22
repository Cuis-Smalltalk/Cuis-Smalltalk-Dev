Class {
	#name : 'PathCurveToCommand',
	#superclass : 'PathCommand',
	#instVars : [
		'toPoints',
		'control1Points',
		'control2Points'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathCurveToCommand class >> svgName [

	^$C
]

{ #category : 'building' }
PathCurveToCommand >> buildFrom: commandChar stream: aStream [

	| x1 y1 x2 y2 x y |
	super buildFrom: commandChar stream: aStream.
	toPoints := OrderedCollection new.
	control1Points := OrderedCollection new.
	control2Points := OrderedCollection new.
	[
		x1 := aStream nextNumber.
		y1 := aStream nextNumber.
		x2 := aStream nextNumber.
		y2 := aStream nextNumber.
		x := aStream nextNumber.
		y := aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y.
			control1Points add: x1@y1.
			control2Points add: x2@y2 ]
]

{ #category : 'drawing' }
PathCurveToCommand >> runOn: aCanvas [

	aCanvas multiCurveTo: coordinatesAreRelative toPoints: toPoints control1Points: control1Points control2Points: control2Points
]

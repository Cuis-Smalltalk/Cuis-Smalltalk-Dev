Class {
	#name : 'PathQuadraticCurveToCommand',
	#superclass : 'PathCommand',
	#instVars : [
		'toPoints',
		'controlPoints'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathQuadraticCurveToCommand class >> svgName [

	^$Q
]

{ #category : 'building' }
PathQuadraticCurveToCommand >> buildFrom: commandChar stream: aStream [

	| x1 y1 x y |
	super buildFrom: commandChar stream: aStream.
	toPoints := OrderedCollection new.
	controlPoints := OrderedCollection new.
	[
		x1 := aStream nextNumber.
		y1 := aStream nextNumber.
		x := aStream nextNumber.
		y := aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y.
			controlPoints add: x1@y1 ]
]

{ #category : 'drawing' }
PathQuadraticCurveToCommand >> runOn: aCanvas [

	aCanvas multiQuadraticCurveTo: coordinatesAreRelative toPoints: toPoints controlPoints: controlPoints
]

{ #category : 'building' }
PathQuadraticCurveToCommand >> toPoints: endPoints controlPoints: aControlPoints [
	coordinatesAreRelative := false.
	toPoints := endPoints.
	controlPoints := aControlPoints
]

{ #category : 'building' }
PathQuadraticCurveToCommand >> toRelativePoints: endPoints controlPoints: aControlPoints [
	coordinatesAreRelative := true.
	toPoints := endPoints.
	controlPoints := aControlPoints
]

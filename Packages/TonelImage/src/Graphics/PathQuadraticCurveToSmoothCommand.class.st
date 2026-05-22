Class {
	#name : 'PathQuadraticCurveToSmoothCommand',
	#superclass : 'PathCommand',
	#instVars : [
		'toPoints'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathQuadraticCurveToSmoothCommand class >> svgName [

	^$T
]

{ #category : 'building' }
PathQuadraticCurveToSmoothCommand >> buildFrom: commandChar stream: aStream [

	| x y |
	super buildFrom: commandChar stream: aStream.
	toPoints := OrderedCollection new.
	[
		x := aStream nextNumber.
		y := aStream nextNumber.
		y notNil ] whileTrue: [
			toPoints add: x@y]
]

{ #category : 'drawing' }
PathQuadraticCurveToSmoothCommand >> runOn: aCanvas [

	aCanvas multiQuadraticCurveToSmooth: coordinatesAreRelative toPoints: toPoints
]

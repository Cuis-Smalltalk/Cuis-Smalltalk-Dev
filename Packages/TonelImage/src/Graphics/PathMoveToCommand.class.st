Class {
	#name : 'PathMoveToCommand',
	#superclass : 'PathCommand',
	#instVars : [
		'points'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathMoveToCommand class >> svgName [

	^$M
]

{ #category : 'building' }
PathMoveToCommand >> buildFrom: commandChar stream: aStream [

	super buildFrom: commandChar stream: aStream.
	points := Array streamContents: [ :strm | | px py |
		[px := aStream nextNumber.
		py := aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: px@py ]]
]

{ #category : 'drawing' }
PathMoveToCommand >> runOn: aCanvas [

	aCanvas closePathAndMoveBack.
	aCanvas multiMoveTo: coordinatesAreRelative points: points
]

{ #category : 'building' }
PathMoveToCommand >> toPoint: endPoint [
	coordinatesAreRelative := false.
	points := {endPoint}
]

{ #category : 'building' }
PathMoveToCommand >> toRelativePoint: endPoint [
	coordinatesAreRelative := true.
	points := {endPoint}
]

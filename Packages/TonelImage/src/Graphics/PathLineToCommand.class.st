Class {
	#name : 'PathLineToCommand',
	#superclass : 'PathCommand',
	#instVars : [
		'points'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathLineToCommand class >> svgName [

	^$L
]

{ #category : 'building' }
PathLineToCommand >> buildFrom: commandChar stream: aStream [

	super buildFrom: commandChar stream: aStream.
	points := Array streamContents: [ :strm | | px py |
		[px := aStream nextNumber.
		py := aStream nextNumber.
		py notNil ] whileTrue:
			[ strm nextPut: px@py ]]
]

{ #category : 'drawing' }
PathLineToCommand >> runOn: aCanvas [

	aCanvas multiLineTo: coordinatesAreRelative points: points
]

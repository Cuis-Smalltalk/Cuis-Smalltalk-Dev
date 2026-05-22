Class {
	#name : 'PathLineToHCommand',
	#superclass : 'PathCommand',
	#instVars : [
		'xs'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathLineToHCommand class >> svgName [

	^$H
]

{ #category : 'building' }
PathLineToHCommand >> buildFrom: commandChar stream: aStream [

	super buildFrom: commandChar stream: aStream.
	xs := Array streamContents: [ :strm | | px |
		[px := aStream nextNumber.
		px notNil ] whileTrue:
			[ strm nextPut: px ]]
]

{ #category : 'drawing' }
PathLineToHCommand >> runOn: aCanvas [

	aCanvas multiLineTo: coordinatesAreRelative xs: xs
]

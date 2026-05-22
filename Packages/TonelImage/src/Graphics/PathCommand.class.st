Class {
	#name : 'PathCommand',
	#superclass : 'Object',
	#instVars : [
		'coordinatesAreRelative'
	],
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathCommand class >> classFor: anSVGPathCommandName [

	self allSubclassesDo: [ :cls |
		cls svgName = anSVGPathCommandName asUppercase
			ifTrue: [ ^cls ] ].
	^nil
]

{ #category : 'instance creation' }
PathCommand class >> from: commandChar stream: aStream [
	| cls|
	cls := self classFor: commandChar.
	cls ifNil: [
		PathCommandError signal: ('Unhandled path command: 'copyWith: commandChar).
		^nil ].
	^cls new buildFrom: commandChar stream: aStream
	
]

{ #category : 'instance creation' }
PathCommand class >> svgName [
	"Answer the name of the svg elements we can represent"

	^self subclassResponsibility
]

{ #category : 'building' }
PathCommand >> buildFrom: commandChar stream: aStream [

	coordinatesAreRelative := commandChar isLowercase
]

{ #category : 'building' }
PathCommand >> buildFromStream: aStream in: parentToBe [

	coordinatesAreRelative := aStream next isLowercase
]

{ #category : 'drawing' }
PathCommand >> runOn: aCanvas [

	self subclassResponsibility
]

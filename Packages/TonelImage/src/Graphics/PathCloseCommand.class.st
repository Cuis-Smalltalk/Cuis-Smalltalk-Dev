Class {
	#name : 'PathCloseCommand',
	#superclass : 'PathCommand',
	#category : 'Graphics-PathCommands',
	#package : 'Graphics',
	#tag : 'PathCommands'
}

{ #category : 'instance creation' }
PathCloseCommand class >> svgName [

	^$Z
]

{ #category : 'building' }
PathCloseCommand >> buildFrom: commandChar stream: aStream [

	super buildFrom: commandChar stream: aStream
]

{ #category : 'drawing' }
PathCloseCommand >> runOn: aCanvas [

	aCanvas closePath
]

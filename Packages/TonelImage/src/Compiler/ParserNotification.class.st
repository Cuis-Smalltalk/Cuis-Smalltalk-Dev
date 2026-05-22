Class {
	#name : 'ParserNotification',
	#superclass : 'Notification',
	#instVars : [
		'name'
	],
	#category : 'Compiler-Exceptions',
	#package : 'Compiler',
	#tag : 'Exceptions'
}

{ #category : 'instance creation' }
ParserNotification class >> name: aName [

	^(self new initializeNamed: aName) signal
]

{ #category : 'initialization' }
ParserNotification >> initializeNamed: aName [

	name := aName
]

{ #category : 'name' }
ParserNotification >> name [

	^name
]

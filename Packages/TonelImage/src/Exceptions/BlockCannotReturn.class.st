"
This class is private to the EHS implementation.  Its use allows for ensured execution to survive code such as:

[self doThis.
^nil]
	ensure: [self doThat]

Signaling or handling this exception is not recommended.
"
Class {
	#name : 'BlockCannotReturn',
	#superclass : 'Error',
	#instVars : [
		'result',
		'deadHome',
		'finalContext',
		'pc'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'accessing' }
BlockCannotReturn >> deadHome [

	^ deadHome
]

{ #category : 'accessing' }
BlockCannotReturn >> deadHome: context [

	deadHome := context
]

{ #category : 'exceptionDescription' }
BlockCannotReturn >> defaultAction [

	self messageText: 'Block cannot return'.
	^super defaultAction
]

{ #category : 'accessing' }
BlockCannotReturn >> finalContext [

	^ finalContext
]

{ #category : 'accessing' }
BlockCannotReturn >> finalContext: context [

	finalContext := context
]

{ #category : 'exceptionDescription' }
BlockCannotReturn >> isResumable [

	^true
]

{ #category : 'accessing' }
BlockCannotReturn >> pc [

	^pc
]

{ #category : 'accessing' }
BlockCannotReturn >> pc: pcIndex [

	pc := pcIndex
]

{ #category : 'accessing' }
BlockCannotReturn >> result [

	^result
]

{ #category : 'accessing' }
BlockCannotReturn >> result: r [

	result := r
]

"
Halt is provided to support Object>>halt.
"
Class {
	#name : 'Halt',
	#superclass : 'Exception',
	#category : 'Exceptions-Extensions',
	#package : 'Exceptions',
	#tag : 'Extensions'
}

{ #category : 'priv handling' }
Halt >> defaultAction [

	self noHandler
]

{ #category : 'description' }
Halt >> isResumable [

	^true
]

{ #category : 'description' }
Halt >> noHandler [
	"If this preference is set, Halt behaves like any other Exception:
		- following any appropriate exception handler
		- throwing the UnhandledError exception if no handler
	This means that Halt can be masked (hidden) by a handler of UnhandledError.
	This is the traditional behavior, but can be a pain if such handler is present."
	(Preferences at: #haltThrowsUnhandledError) ifTrue: [
		super noHandler.
		^self ].

	"Usually Halt should not be caught by an UnhandledError handler, meaning that a Debugger will open
	even in the presence of a handler of UnhandledError that hides most exceptions."
	UnhandledHalt signalForException: self
]

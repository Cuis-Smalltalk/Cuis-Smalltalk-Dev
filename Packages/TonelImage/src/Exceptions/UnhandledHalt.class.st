"
UnhandledHalt is an alternative for UnhandledError for Halts.

The idea is that even if UnhandledError is handled and masked, you still may want to get Halts.
"
Class {
	#name : 'UnhandledHalt',
	#superclass : 'Exception',
	#instVars : [
		'exception'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'as yet unclassified' }
UnhandledHalt class >> signalForException: anError [

	^ self new
		exception: anError;
		signal
]

{ #category : 'priv handling' }
UnhandledHalt >> defaultAction [
	"The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated."

	Smalltalk isDevelopmentEnvironmentPresent
		ifTrue: [ self devDefaultAction ]
		ifFalse: [ self standaloneAppDefaultAction ]
]

{ #category : 'priv handling' }
UnhandledHalt >> devDefaultAction [

	Processor activeProcess
		debugContext: exception signalerContext
		title: exception description.
]

{ #category : 'accessing' }
UnhandledHalt >> exception [

	^ exception
]

{ #category : 'accessing' }
UnhandledHalt >> exception: anError [

	exception := anError
]

{ #category : 'priv handling' }
UnhandledHalt >> isResumable [
	
	^ false
]

{ #category : 'priv handling' }
UnhandledHalt >> standaloneAppDefaultAction [
	"Dump the stack trace to a log file, then exit the program (image)."
	
	Smalltalk logError: self description inContext: self signalerContext to: 'CuisDebug'.
	Smalltalk quitPrimitive: 1
]

"
This exception represents an error exception that has gone unhandled.  Unhandled error conditions are fundamentally different from error exceptions, because error exceptions may have default handlers that address the error condition (by e.g. retrying the operation).  The job of unhandled errors is to report the original problem.  This reporting can be done in a variety of ways.  For example, in everyday practice, unhandled errors open the debugger.

Note the common practice of "catching all errors" with code such as this:

	[some code]
		on: Error
		do: [:ex | ex return]
		
is doubly problematic.  First, there is no specificity to the expected exceptions arising from the protected block.  Second, the handler block will prevent the exception's default handler from running, which may resolve or otherwise react to the error condition.  If one really wants to catch unhandled errors, the code should read like this instead:

	[some code]
		on: UnhandledError
		do: [:ex | ex return]
"
Class {
	#name : 'UnhandledError',
	#superclass : 'Exception',
	#instVars : [
		'exception'
	],
	#category : 'Exceptions-Kernel',
	#package : 'Exceptions',
	#tag : 'Kernel'
}

{ #category : 'as yet unclassified' }
UnhandledError class >> signalForException: anError [

	^ self new
		exception: anError;
		signal
]

{ #category : 'priv handling' }
UnhandledError >> defaultAction [
	"The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated."

	Smalltalk isDevelopmentEnvironmentPresent
		ifTrue: [ self devDefaultAction ]
		ifFalse: [ self standaloneAppDefaultAction ]
]

{ #category : 'priv handling' }
UnhandledError >> devDefaultAction [

	Processor activeProcess
		debugContext: exception signalerContext
		title: exception description.
]

{ #category : 'accessing' }
UnhandledError >> exception [

	^ exception
]

{ #category : 'accessing' }
UnhandledError >> exception: anError [

	exception := anError
]

{ #category : 'priv handling' }
UnhandledError >> isResumable [
	
	^ false
]

{ #category : 'priv handling' }
UnhandledError >> standaloneAppDefaultAction [
	"Dump the stack trace to a log file, then exit the program (image)."
	
	Smalltalk logError: self description inContext: self signalerContext to: 'CuisDebug'.
	Smalltalk quitPrimitive: 1
]

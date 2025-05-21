'From Cuis7.3 [latest update: #7187] on 21 May 2025 at 3:05:06 pm'!
!classDefinition: #UnhandledHalt category: #'Exceptions-Kernel'!
Exception subclass: #UnhandledHalt
	instanceVariableNames: 'exception'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Kernel'!

!UnhandledHalt commentStamp: '<historical>' prior: 0!
UnhandledHalt is an alternative for UnhandledError for Halts.

The idea is that even if UnhandledError is handled and masked, you still may want to get Halts.!


!UnhandledHalt methodsFor: 'priv handling' stamp: 'jmv 5/21/2025 14:55:52'!
defaultAction
	"The current computation is terminated. The cause of the error should be logged or reported to the user. If the program is operating in an interactive debugging environment the computation should be suspended and the debugger activated."

	Smalltalk isDevelopmentEnvironmentPresent
		ifTrue: [ self devDefaultAction ]
		ifFalse: [ self standaloneAppDefaultAction ]! !

!UnhandledHalt methodsFor: 'priv handling' stamp: 'jmv 5/21/2025 14:55:52'!
devDefaultAction

	Processor activeProcess
		debugContext: exception signalerContext
		title: exception description.! !

!UnhandledHalt methodsFor: 'priv handling' stamp: 'jmv 5/21/2025 14:55:52'!
isResumable
	
	^ false! !

!UnhandledHalt methodsFor: 'priv handling' stamp: 'jmv 5/21/2025 14:55:52'!
standaloneAppDefaultAction
	"Dump the stack trace to a log file, then exit the program (image)."
	
	Smalltalk logError: self description inContext: self signalerContext to: 'CuisDebug'.
	Smalltalk quitPrimitive: 1! !

!UnhandledHalt methodsFor: 'accessing' stamp: 'jmv 5/21/2025 14:55:52'!
exception

	^ exception! !

!UnhandledHalt methodsFor: 'accessing' stamp: 'jmv 5/21/2025 14:55:52'!
exception: anError

	exception := anError! !


!UnhandledHalt class methodsFor: 'as yet unclassified' stamp: 'jmv 5/21/2025 14:55:52'!
signalForException: anError

	^ self new
		exception: anError;
		signal! !


!Halt methodsFor: 'description' stamp: 'jmv 5/21/2025 14:59:01'!
noHandler
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
	UnhandledHalt signalForException: self! !


!ContextPart methodsFor: 'controlling' stamp: 'jmv 5/21/2025 15:01:00'!
runUntilErrorOrReturnFrom: aSenderContext
	"ASSUMES aSenderContext is a sender of self.  Execute self's stack until aSenderContext returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSenderContext returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSenderContext that jumps back to thisContext when evaluated.  We also insert an exception handler under aSenderContext that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error exceptionHandlerContext ensureContext ctxt here topContext |
	aSenderContext method isQuick ifTrue: [ 
		^ {aSenderContext step. nil}
	].
	
	here := thisContext.

	"Insert ensure and exception handler contexts under aSenderContext.
	As Halt may not raise an UnhandledError (see #noHandler), it may need to be handled explicitly here."
	error := nil.
	exceptionHandlerContext := ContextPart
		contextOn: UnhandledError, UnhandledHalt do: [ :unhandledError |
			error ifNil: [
				error := unhandledError exception.
				topContext := thisContext.
				Debugger updateDebuggersLeadingTo: aSenderContext for: error.
				unhandledError resumeUnchecked: here jump ]
			ifNotNil: [unhandledError pass ]].
	ctxt := aSenderContext insertSender: exceptionHandlerContext.
	ensureContext := ContextPart
		contextEnsure: [
			error ifNil: [
				topContext := thisContext.
				here jump ]].
	ctxt := ctxt insertSender: ensureContext.
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [
		"No error was raised, remove ensure context by stepping until popped"
		[ctxt isDead or: [topContext isNil]] whileFalse: [topContext := topContext stepToCalleeOrNil].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSenderContext terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].! !


!Process methodsFor: 'changing suspended state' stamp: 'jmv 5/21/2025 15:03:48'!
isSuspendedContextSignalUnhandledError

	^ suspendedContext selector == #signalForException:
		and: [suspendedContext receiver isBehavior 
		and: [
			(suspendedContext receiver includesBehavior: UnhandledError) or: [
				suspendedContext receiver includesBehavior: UnhandledHalt ]]]! !

!methodRemoval: UnhandledHalt class #defaultActionForException: stamp: 'jmv 5/21/2025 14:58:44'!
UnhandledHalt class removeSelector: #defaultActionForException:!
!methodRemoval: UnhandledError class #defaultActionForException: stamp: 'jmv 5/21/2025 14:58:55'!
UnhandledError class removeSelector: #defaultActionForException:!

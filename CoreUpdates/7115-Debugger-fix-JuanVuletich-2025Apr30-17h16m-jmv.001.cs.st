'From Cuis7.3 [latest update: #7114] on 30 April 2025 at 5:16:22 pm'!

!Process methodsFor: 'changing suspended state' stamp: 'jmv 4/30/2025 17:14:06'!
stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| home |
	home := aContext home.
	[
		suspendedContext := suspendedContext step.
		home == suspendedContext home or: [ suspendedContext sender isNil or: [suspendedContext sender isDead]]
	] whileFalse:
		[
			self ifSuspendedContextIsUnhandledErrorDo: [ :anError |
			anError signalerContext ifNotNil: [:unhandledErrorSignalerContext|
				self completeStepUpTo: unhandledErrorSignalerContext.

				"Give a debugger a chance to update its title to reflect the new exception"
				 Notification signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError.
				^unhandledErrorSignalerContext]]].
		
	^suspendedContext! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 4/30/2025 17:13:24'!
stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	self selectedContext willReturn ifTrue: [ ^self doStep ].
	self 
		handleLabelUpdatesIn: [interruptedProcess stepToHome: self selectedContext]
		whenExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn! !


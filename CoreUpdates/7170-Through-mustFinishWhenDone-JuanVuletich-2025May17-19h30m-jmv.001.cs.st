'From Cuis7.3 [latest update: #7167] on 17 May 2025 at 7:31:56 pm'!

!Process methodsFor: 'changing suspended state' stamp: 'jmv 5/17/2025 19:31:23'!
stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| home |
	home := aContext home.
	[
		suspendedContext := suspendedContext step.
		suspendedContext sender = aContext or:
			[ suspendedContext home == home or:
				[ suspendedContext sender isNil or: [suspendedContext sender isDead]]]
	] whileFalse:
		[
			self ifSuspendedContextIsUnhandledErrorDo:
				[ :anError |
					anError signalerContext ifNotNil:
						[ :unhandledErrorSignalerContext |
							self completeStepUpTo: unhandledErrorSignalerContext.
							"Give a debugger a chance to update its title to reflect the new exception"
							 Notification signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError.
							^unhandledErrorSignalerContext ]]].
	^suspendedContext! !


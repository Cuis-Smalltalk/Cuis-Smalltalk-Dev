'From Cuis7.3 [latest update: #7182] on 20 May 2025 at 4:37:35 pm'!

!Process methodsFor: 'changing suspended state' stamp: 'jmv 5/20/2025 16:23:09'!
stepToHome: requestedContext
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| requestedHome |

	requestedHome := requestedContext home.
	[
		(suspendedContext selector == #halt or: [suspendedContext selector == #halt:]) ifTrue: [
			self completeStepUpTo: requestedContext.
			^suspendedContext ].
		
		suspendedContext := suspendedContext step.
	
		"We reached the home context of the block we are doing [Through] on."
		suspendedContext home == requestedHome ifTrue: [
			^suspendedContext ].
	
		"But if the requestedHome is dead, meaning that we'll never reach back there,
		stop when we get back to where [Through] was requested."
		requestedHome isDead ifTrue: [
			suspendedContext = requestedContext ifTrue: [
				^suspendedContext ]].
	
		"There's nowhere to continue stepping into. Don't do it."
		(suspendedContext sender isNil or: [suspendedContext sender isDead]) ifTrue: [
			^suspendedContext ].
	
		self ifSuspendedContextIsUnhandledErrorDo: [ :anError |
			anError signalerContext ifNotNil: [ :unhandledErrorSignalerContext |
				self completeStepUpTo: unhandledErrorSignalerContext.
				"Give a debugger a chance to update its title to reflect the new exception"
				 Notification signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError.
				^unhandledErrorSignalerContext ]]
	
	] repeat.! !


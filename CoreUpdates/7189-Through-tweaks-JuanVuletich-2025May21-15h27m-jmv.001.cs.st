'From Cuis7.3 [latest update: #7188] on 21 May 2025 at 3:28:51 pm'!

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 5/21/2025 15:26:26'!
stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method.
	Button [Through] in the Debugger."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].
	"It is actually suspended, not terminated!!"
	interruptedProcess name = Debugger debuggingTerminatedProcessName ifTrue: [
		^self inform: 'Debugging terminated'].

	self selectedContext method isQuick ifTrue: [ ^self doStep ].

	(self selectedContext = interruptedProcess suspendedContext and: [
			interruptedProcess suspendedContext atEnd and: [interruptedProcess suspendedContext closure isNil]])
		ifTrue: [
			^self doStep ].

	self 
		handleLabelUpdatesIn: [
			interruptedProcess stepToHome: self selectedContext ]
		whenExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.! !


!Process methodsFor: 'changing suspended state' stamp: 'jmv 5/20/2025 13:22:52'!
stepToHome: requestedContext
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| requestedHome |
	requestedHome := requestedContext home.
	[
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


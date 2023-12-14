'From Cuis6.0 [latest update: #6118] on 14 December 2023 at 10:11:54 am'!

!ContextPart methodsFor: 'controlling' stamp: 'jmv 12/14/2023 10:11:21'!
runUntilErrorOrReturnFrom: aSenderContext
	"ASSUMES aSenderContext is a sender of self.  Execute self's stack until aSenderContext returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSenderContext returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSenderContext that jumps back to thisContext when evaluated.  We also insert an exception handler under aSenderContext that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error exceptionHandlerContext ensureContext ctxt here topContext |
	aSenderContext method isQuick ifTrue: [ 
		^ {aSenderContext step. nil}
	].
	
	here := thisContext.

	"Insert ensure and exception handler contexts under aSenderContext"
	error := nil.
	exceptionHandlerContext := ContextPart
		contextOn: UnhandledError do: [ :unhandledError |
			error ifNil: [
				error := unhandledError exception.
				topContext := thisContext.
				unhandledError resumeUnchecked: here jump]
			ifNotNil: [unhandledError pass ]].
	ctxt := aSenderContext insertSender: exceptionHandlerContext.
	ensureContext := ContextPart
		contextEnsure: [error ifNil: [
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
	].
! !


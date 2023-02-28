'From Cuis 6.0 [latest update: #5667] on 24 February 2023 at 10:19:15 pm'!

!ContextPart methodsFor: 'private-exceptions' stamp: 'jar 2/24/2023 21:27:11'!
unwindTo: aContext safely: aBoolean
	"Unwind self to aContext to execute pending #ensure:/#ifCurtailed: argument blocks between self
	 and aContext. If aBoolean is false, unwind only blocks that have not run yet, otherwise complete all
	 pending unwind blocks including those currently in the middle of their execution; these blocks will
	 just finish their execution. Run all unwinds on their original stack using #runUntilReturnFrom:."
	
	| top ctx |
	ctx _ top _ self.
	aBoolean ifTrue: [
	"If self is the top context of a stack already halfways through an unwind block, complete the outer-most
	 unfinished unwind block first; all nested pending unwind blocks will be completed in the process;
	 see testTerminationDuringUnwind and tests in ProcessTest/UnwindTest.
	 Note: Halfway-through blocks have already set the complete variable (ctxt tempAt: 2) in their
	 defining #ensure:/#ifCurtailed contexts from nil to true; we'll search for the bottom-most one."
		| outerMost |
		ctx isUnwindContext ifFalse: [ctx _ ctx findNextUnwindContextUpTo: aContext].
		[ctx isNil] whileFalse: [
			(ctx tempAt: 2) ifNotNil: [
				outerMost _ ctx].
			ctx _ ctx findNextUnwindContextUpTo: aContext].
		outerMost ifNotNil: [top _ top runUntilReturnFrom: outerMost]
		"By now no halfway-through unwind blocks are on the stack. 
		 Note: top points to the former outerMost sender now, i.e. to the next context to be explored."].

	ctx _ top ifNil: [^self].
	"#findNextUnwindContextUpTo: starts searching from the receiver's sender so we must check
	 the receiver explicitly whether it is an unwind context; see testTerminateEnsureAsStackTop.
	 Create a new top context (i.e. a new branch off the original stack) for each pending unwind block
	 (ctxt tempAt: 1) and execute it on the unwind block's stack to evaluate non-local returns correctly."
	ctx isUnwindContext ifFalse: [ctx _ ctx findNextUnwindContextUpTo: aContext].
	[ctx isNil] whileFalse: [
		(ctx tempAt: 2) ifNil: [
			ctx tempAt: 2 put: true.
			(ctx tempAt: 1) ifNotNil: [:unwindBlock |
				top _ unwindBlock asContextWithSender: ctx.
				top runUntilReturnFrom: top]].
		ctx _ ctx findNextUnwindContextUpTo: aContext]
	"Note: Cf. the unwind pattern in the previous versions of unwindTo: (1999-2021). Using #value
	 instead of #runUntilReturnFrom: lead to a failure to evaluate some non-local returns correctly;
	 a non-local return must be evaluated in the evaluation context (sender chain) in which it was defined."! !


!BlockClosure methodsFor: 'private' stamp: 'jar 2/10/2023 16:51:19'!
valueAndWaitWhileUnwinding: contextToUnwind
	"Evaluate the receiver and suspend current process until argument's sender chain is unwound."

	| semaphore newBottom |
	contextToUnwind ifNil: [^self].
	semaphore _ Semaphore new.
	newBottom _ contextToUnwind class contextEnsure: [semaphore signal].
	contextToUnwind bottomContext insertSender: newBottom.
	self value: contextToUnwind.
	semaphore wait! !


!ContextPart methodsFor: 'controlling' stamp: 'jar 2/24/2023 21:34:34'!
runUntilErrorOrReturnFrom: aSender 
	"ASSUMES aSender is a sender of self.  Execute self's stack until aSender returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSender returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSender that jumps back to thisContext when evaluated.  We also insert an exception handler under aSender that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error ctxt here topContext |
	here _ thisContext.

	"Insert ensure and exception handler contexts under aSender"
	error _ nil.
	ctxt _ aSender insertSender: (ContextPart
		contextOn: UnhandledError do: [:ex |
			error ifNil: [
				error _ ex exception.
				topContext _ thisContext.
				ex resumeUnchecked: here jump]
			ifNotNil: [ex pass]
		]).
	ctxt _ ctxt insertSender: (ContextPart
		contextEnsure: [error ifNil: [
				topContext _ thisContext.
				here jump]
		]).
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [
		"No error was raised, remove ensure context by stepping until popped"
		[ctxt isDead or: [topContext isNil]] whileFalse: [topContext _ topContext stepToCallee].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSender terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].
! !

!ContextPart methodsFor: 'private' stamp: 'jar 2/10/2023 15:56:30'!
unwindAndStop: aProcess
	"I'm a helper method to #terminate; I create and answer
	 a helper stack for a terminating process to unwind itself from.
	 Note: push a fake return value to create a proper top context."

	^(self class contextEnsure: [self unwindTo: nil]) 
		privSender: [aProcess suspend] asContext;
		push: nil
! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'jar 2/2/2023 16:36:54'!
unwindTo: aContext
	"Unwind the receiver to aContext to execute all pending unwind blocks."

	self unwindTo: aContext safely: true! !


!ContextPart class methodsFor: 'special context creation' stamp: 'jar 2/24/2023 21:08:32'!
contextOn: exceptionClass do: block
	"Create an #on:do: context that is ready to return from executing its receiver.
	 Note: push a fake return value to the ctxt stack before making the jump as 
	 #jump would remove it before returning (see #jump and #contextEnsure:)."

	| ctxt chain |
	ctxt _ thisContext.
	[chain _ thisContext sender cut: ctxt. ctxt push: nil; jump] on: exceptionClass do: block.
	"jump above will resume here without unwinding chain"
	^ chain! !


!MethodContext methodsFor: 'testing' stamp: 'jar 2/24/2023 21:44:08'!
atEnd: aPC
	^ self isExecutingBlock
		ifTrue: [ aPC == self closure endpc ]
		ifFalse: [ aPC >= self method endPC ]! !


!Process methodsFor: 'changing process state' stamp: 'jar 2/24/2023 22:22:22'!
terminate 
	"Stop the process that the receiver represents forever. Allow all pending unwind
	 blocks run before terminating; if they are currently in progress, let them finish."
	
	 "If terminating the active process, create a new stack and run unwinds from there.
	
	 If terminating a suspended process (including runnable and blocked), always
	 suspend the terminating process first so it doesn't accidentally get woken up,
	 and nil the suspended context to prevent accidental resumption or termination
	 while manipulating the suspended context.
	
	 Equally important is the side effect of the suspension: In 2022 a new suspend
	 semantics has been introduced: the revised #suspend backs up a process waiting
	 on a conditional variable to the send that invoked the wait state, while the previous
	 #suspend simply removed the process from the conditional variable's list it was
	 previously waiting on; see #suspend and #suspendAndUnblock comments.
	 
	 If the process is blocked, waiting to access the #critical: section, release it properly.
	
	 To allow a suspended process to unwind itself, create a new stack for the process
	 being terminated and resume the suspended process to complete its termination
	 from the new, parallel stack. Use a semaphore to make the process that invoked
	 the termination wait for self's completion. Execute the termination in the ensure
	 argument block to ensure it completes even if the terminator process itself gets
	 terminated before it's finished; see testTerminateInTerminate and others."

	self isRunning ifTrue: [
		^(thisContext unwindAndStop: self) jump].

	[
		[:contextToUnwind | 
			self priority: Processor activePriority;
				suspendedContext: (contextToUnwind unwindAndStop: self);
				resume
		] valueAndWaitWhileUnwinding: self suspendAndReleaseCriticalSection
	] valueEnsured! !


'From Cuis 6.0 [latest update: #5434] on 16 August 2022 at 9:00:09 pm'!

!ContextPart methodsFor: 'private' stamp: 'jar 7/22/2022 21:49:53'!
unwindAndStop: aProcess
	"I'm a helper method to #terminate; I create and answer
	 a helper stack for a terminating process to unwind itself from."

	^(ContextPart contextEnsure: [self unwindTo: nil]) 
		privSender: [aProcess suspend] asContext
! !


!SystemDictionary methodsFor: 'system attributes' stamp: 'eem 1/1/2022 17:39:00'!
processSuspensionUnblocks
	"Answer whether the suspend primitive backs up a process waiting on a
	 condition variable to before the send or not. The original behaviour was to
	 remove a process from its condition variable (Semaphore, Mutex; both kinds
	 of LinkedList).  Hence resuming would allow the process past the condition
	 variable, and hence required special behaviour in Process>>signalException:.
	 The new behaviour is to back up a blocked process one bytecode so that if
	 and when resumed it enters the wait state.  It also implies that the suspend
	 primitive answers nil if the process is blocked, and only answers a list when
	 suspending a runnable process that is not the active process.

	 To be clear, this method answers true on VMs with the old behaviour and
	  false on those with the new."

	^((self vmParameterAt: 65) ifNil: 0) noMask: 32! !


!Process methodsFor: 'changing process state' stamp: 'jar 8/15/2022 12:48:39'!
suspendAndUnblock
	"ar 12/7/2007 17:10:
	 Primitive. Stop the process that the receiver represents in such a way 
	 that it can be restarted at a later time (by sending the receiver the 
	 message resume). If the receiver represents the activeProcess, suspend it. 
	 Otherwise remove the receiver from the list of waiting processes.
	 The return value of this method is the list the receiver was previously on (if any)."

	<primitive: 88 error: ec>
	"ar 12/7/2007 17:10:
	 This is fallback code for VMs which only support the old primitiveSuspend which 
	 would not accept processes that are waiting to be run."
	 ^myList ifNotNil: "this allows us to use suspend multiple times"
		[:oldList|
		 myList _ nil.
		 oldList remove: self ifAbsent: [].
		 oldList]! !

!Process methodsFor: 'private' stamp: 'jar 8/15/2022 12:47:58'!
suspendAndReleaseCriticalSection
	"Figure out if we are terminating a process that is in the ensure: block of a critical section."

	| oldList |
	"Suspend and unblock the receiver from a condition variable using suspend primitive #88.
	 It answers the list the receiver was on before the suspension."
	oldList _ 	self suspendAndUnblock.
	suspendedContext ifNil: [^self].
	(oldList class == Semaphore and: [
		suspendedContext method == (Semaphore compiledMethodAt: #critical:)]) ifTrue: [
			suspendedContext _ suspendedContext home	]! !


!ContextPart methodsFor: 'private-exceptions' stamp: 'jar 7/22/2022 21:43:25'!
runUntilReturnFrom: aContext
	"Run the receiver (which must be a stack top context) until aContext returns. Avoid a context that cannot return. 
	Note: to avoid infinite recursion of MNU error inside unwind blocks, implement e.g. a wrapper around the message 
	sentTo: receiver in #doesNotUnderstand:. Note: This method was meant to be used exclusively by #unwindTo."

	| here unwindBottom newTop |
	here _ thisContext.
	"Avoid a context that cannot return (see Note 1 below)"
	unwindBottom _ self findContextSuchThat: [:ctx | ctx == aContext or: [ctx selector = #cannotReturn:]].
	newTop _ unwindBottom sender.
	"Insert ensure context under unwindBottom in self's stack (see Note 2 below)"
	unwindBottom insertSender: (self class contextEnsure: [here jump]).
	self jump.  "Control jumps to top (see Note 2 below)"
	"Control resumes here once the above inserted ensure block is executed"
	^newTop  "Return the new top context (see Note 3 below)"

	"Note 1: returning from #cannotReturn's sender would crash the VM so we install a guard ensure context right 
	above it and after returning here the unwind will continue safely. Try running and debugging this example 
	(avoid Proceeding the BCR error though; it may indeed crash the image):
	[[[] ensure: [^2]] ensure: [^42]] fork"

	"Note 2: the receiver is run by jumping directly to it (the active process abandons thisContext and executes 
	the receiver on its own stack; the receiver is its top context). However, before jumping to top we insert an ensure 
	block under unwindBottom context that will execute a jump back to thisContext when evaluated. The inserted 
	guard ensure context is removed once control jumps back to thisContext."

	"Note 3: it doesn't matter newTop is not a proper stack top context because #unwindTo will only use it as a starting 
	point in the search for the next unwind context and the computation will never return here. Cf. the pattern in 
	#runUntilErrorOrReturnFrom:: removing the inserted ensure context by stepping until popped when executing 
	non-local returns is not applicable here and would fail tests testTerminateInNestedEnsureWithReturn1 through 4."
! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'jar 7/22/2022 21:38:54'!
unwindTo: aContext

	| top ctxt outerMost |
	"If the receiver represents a block already halfways through an unwind, complete that unwind block
	first; if there are multiple such nested unwind blocks, try to complete the outer-most one; all nested 
	unwind blocks will be completed in the process; see tests in UnwindTest, testTerminationDuringUnwind. 
	Note: Halfway-through blocks have already set the complete variable (tempAt: 2) in their defining
	#ensure:/#ifCurtailed contexts from nil to true; we'll search for the bottom-most one.
	Note: #findNextUnwindContextUpTo: starts searching from the receiver's sender so we must check 
	whether the receiver itself is an unwind context as well; see testTerminateEnsureAsStackTop."
	ctxt _ top _ self.
	ctxt isUnwindContext ifFalse: [ctxt _ ctxt findNextUnwindContextUpTo: aContext].
	[ctxt isNil] whileFalse: [
		(ctxt tempAt:2) ifNotNil: [
			outerMost _ ctxt].
		ctxt _ ctxt findNextUnwindContextUpTo: aContext].
	outerMost ifNotNil: [top _ top runUntilReturnFrom: outerMost].

	"By now no halfway-through unwind blocks are on the stack. Create a new top context for each 
	pending unwind block (tempAt: 1) and execute it on the unwind block's stack to execute non-local 
	returns correctly. Cf. the unwind pattern in #resume:through:. In #unwindTo, using #value instead of 
	#runUntilReturnFrom: would lead to an incorrect evaluation of non-local returns on the wrong stack. 
	Note: top points to the former outerMost sender now, i.e. to the next unexplored context."
	ctxt _ top ifNil: [^self].
	ctxt isUnwindContext ifFalse: [ctxt _ ctxt findNextUnwindContextUpTo: aContext].
	[ctxt isNil] whileFalse: [
		(ctxt tempAt: 2) ifNil: [
			ctxt tempAt: 2 put: true.
			top _ (ctxt tempAt: 1) asContextWithSender: ctxt.
			top runUntilReturnFrom: top].
		ctxt _ ctxt findNextUnwindContextUpTo: aContext]
! !


!DelayWaitTimeout methodsFor: 'signaling' stamp: 'jar 8/15/2022 14:24:26'!
signalWaitingProcess
	"Release the given process from the semaphore it is waiting on.
	This method relies on running at highest priority so that it cannot be preempted
	by the process being released."
	beingWaitedOn := false.
	"Release the process but only if it is still waiting on its original list"
	process suspendingList == delaySemaphore ifTrue:[
		expired := true.
		process suspendAndUnblock; resume.
	].
! !


!Process methodsFor: 'changing process state' stamp: 'jar 8/15/2022 14:04:58'!
suspend
	"eem 1/3/2022 10:38:
	 Primitive. Suspend the receiver, aProcess, such that it can be executed again
	 by sending #resume. If the given process is not the active process, take it off
	 its corresponding list. If the list was not its run queue assume it was on some
	 condition variable (Semaphore, Mutex) and back up its pc to the send that
	 invoked the wait state the process entered.  Hence when the process resumes
	 it will reenter the wait state. Answer the list the receiver was previously on iff
	 it was not active and not blocked, otherwise answer nil."

	<primitive: 578 error: ec>
	"This is fallback code for VMs which only support the old primitiveSuspend 88.
	 Note: in this case some tests may fail and some methods assuming the revised
	 suspend semantics described above may not work entirely as expected (e.g.
	 Context >> #releaseCriticalSection or Process >> #signalException)."
	 ^self suspendAndUnblock! !

!Process methodsFor: 'changing process state' stamp: 'jar 7/22/2022 22:22:22'!
terminate 
	"Stop the process that the receiver represents forever. Unwind to execute pending ensure: and
	 ifCurtailed: blocks before terminating; allow all unwind blocks to run; if they are currently in
	 progress, let them finish. If the process is in the middle of a #critical: section, release it properly."
	
	"If terminating the active process, create a parallel stack and run unwinds from there;
	 if terminating a suspended process, create a parallel stack for the process being terminated
	 and resume the suspended process to complete its termination from the parallel stack. Use
	 a priority higher than the active priority to make the process that invoked the termination
	 wait for its completion."

	"If terminating a suspended process (including runnable or blocked), always suspend the 
	 terminating process first so it doesn't accidentally get woken up. Equally important is the
	 side effect of the suspension. In 2022 a new suspend semantics has been introduced:
	 the revised #suspend backs up a process waiting on a conditional variable to the send that
	 invoked the wait state, while the pre-2022 #suspend simply removed the process from
	 the conditional variable's list it was previously waiting on; figure out if we are terminating
	 the process while waiting in Semaphore>>critical:. In this case, pop the suspendedContext
	 so that we leave the ensure: block inside Semaphore>>critical: without signaling the semaphore.
	 Execute termination wrapped in #valueEnsured to ensure it completes even if the terminator
	 process itself gets terminated before it's finished; see testTerminateInTerminate."

	self isActiveProcess ifTrue: [
		^(thisContext unwindAndStop: self) jump].

	[	| terminator |
		self suspendAndReleaseCriticalSection.
		suspendedContext ifNil: [^self].
		terminator _ Semaphore new.
		suspendedContext bottomContext insertSender: (ContextPart contextEnsure: [terminator signal]).
		self suspendedContext: (suspendedContext unwindAndStop: self);
			priority: Processor activePriority;
			resume.
		terminator wait.
	] valueEnsured! !

!Process methodsFor: 'signaling' stamp: 'eem 6/27/2022 11:08:00'!
signalException: anException
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If 
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"

	"If we are the active process, go ahead and signal the exception"
	 self isActiveProcess ifTrue: [^anException signal].

	"One could raise an error here but that would force clients to employ a guard.
	 Since a terminated process can have no response to a signal (there is no
	 where for a handler to exist) it is legitimate to simply discard the signal."
	suspendedContext ifNil: [^self].

	"Suspend myself first to ensure that I won't run away
	 in the midst of the following modifications."
	self isSuspended ifFalse:
		[self suspend].

	"Add a new block context to the stack that will signal the exception (jar 6/27/2022)"
	suspendedContext _ 
		[anException signal. thisContext sender jump] asContextWithSender: suspendedContext.
	"Since this block is not called in a normal way, we need to take care
	that it doesn't directly return to the caller (because that could have 
	the potential to push an unwanted object on the caller's stack).
	Workspace example:
		p := [Semaphore new wait] fork.
		Processor yield.
		p signalException: Notification
	"
	^self resume! !


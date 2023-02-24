'From Cuis 6.0 [latest update: #5656] on 7 February 2023 at 9:21:30 pm'!

!BlockClosure methodsFor: 'private' stamp: 'jar 2/7/2023 21:10:00'!
valueAndWaitWhileUnwinding: contextToUnwind
	"Suspend current process and execute self, when it completes resume current process."

	| semaphore |
	contextToUnwind ifNil: [^self].
	semaphore _ Semaphore new.
	contextToUnwind bottomContext insertSender: (ContextPart contextEnsure: [semaphore signal]).
	self value: contextToUnwind.
	semaphore wait! !


!Process methodsFor: 'changing process state' stamp: 'jar 2/7/2023 21:09:56'!
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

	self isActiveProcess ifTrue: [
		^(thisContext unwindAndStop: self) jump].

	[
		[:contextToUnwind | 
			self priority: Processor activePriority;
				suspendedContext: (contextToUnwind unwindAndStop: self);
				resume
		] valueAndWaitWhileUnwinding: self suspendAndReleaseCriticalSection
	] valueEnsured! !

!Process methodsFor: 'private' stamp: 'jar 2/7/2023 21:11:00'!
suspendAndReleaseCriticalSection
	"Figure out if we are terminating a process that is in the ensure: block of a critical section."

	| oldList |
	"Suspend and unblock the receiver from a condition variable using suspend primitive #88.
	 It answers the list the receiver was on before the suspension."
	oldList _ 	self suspendAndUnblock.
	^suspendedContext ifNotNil: [:context |
		suspendedContext _ nil.
		(oldList class == Semaphore and: [
			context method == (Semaphore compiledMethodAt: #critical:)])
				ifTrue: [context home	]
				ifFalse: [context]]! !


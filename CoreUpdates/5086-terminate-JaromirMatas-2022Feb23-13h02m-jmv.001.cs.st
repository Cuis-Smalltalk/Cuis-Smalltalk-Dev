'From Cuis 6.0 [latest update: #5084] on 23 February 2022 at 1:09:48 pm'!

!Process methodsFor: 'changing process state' stamp: 'jar 2/22/2022 22:22:22'!
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

	| context |
	self isActiveProcess ifTrue: [
		context _ thisContext.
		^[context unwindTo: nil. self suspend] asContext jump].

	[	| oldList |
		oldList _ myList.
		self suspend.
		(oldList class == Semaphore and: [
			suspendedContext method == (Semaphore compiledMethodAt: #critical:)]) ifTrue: [
				suspendedContext _ suspendedContext home	].
		context _ suspendedContext ifNil: [^self].
		suspendedContext _ [context unwindTo: nil. self suspend] asContext.
		self priority: Processor activePriority + 1; resume
	] valueEnsured! !


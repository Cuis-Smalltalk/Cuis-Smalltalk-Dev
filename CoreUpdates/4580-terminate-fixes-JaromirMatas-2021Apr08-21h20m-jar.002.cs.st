'From Cuis 5.0 [latest update: #4527] on 12 April 2021 at 10:16:03 pm'!

!BlockClosure methodsFor: 'scheduling' stamp: 'jar 4/8/2021 19:59:08'!
newProcess
	"Answer a Process running the code in the receiver. The process is not 
	scheduled."
	<primitive: 19> "Simulation guard"
	^Process
		forContext: 
			[self value.
			Processor activeProcess suspend] asContext
		priority: Processor activePriority! !

!BlockClosure methodsFor: 'scheduling' stamp: 'jar 4/8/2021 19:59:28'!
newProcessWith: anArray 
	"Answer a Process running the code in the receiver. The receiver's block 
	arguments are bound to the contents of the argument, anArray. The 
	process is not scheduled."
	<primitive: 19> "Simulation guard"
	^Process
		forContext: 
			[self valueWithArguments: anArray.
			Processor activeProcess suspend] asContext
		priority: Processor activePriority! !


!Process methodsFor: 'changing process state' stamp: 'jar 4/12/2021 20:42:03'!
terminate 
	"Stop the process that the receiver represents forever.  Unwind to execute pending ensure:/ifCurtailed: blocks before terminating."

	| ctxt unwindBlock oldList outerMost |
	self isRunning ifTrue: [
		"If terminating the active process, suspend it first and terminate it as a suspended process."
		[self terminate] fork.
		^self suspend].

	"Always suspend the process first so it doesn't accidentally get woken up"
	oldList _ self suspend.
	suspendedContext ifNotNil:[
		"Figure out if we are terminating the process while waiting in Semaphore>>critical:
		In this case, pop the suspendedContext so that we leave the ensure: block inside
		Semaphore>>critical: without signaling the semaphore."
		(oldList class == Semaphore and:[
			suspendedContext method == (Semaphore compiledMethodAt: #critical:)]) ifTrue: [
				suspendedContext _ suspendedContext home.
		].

	"If terminating a process halfways through an unwind, try to complete that unwind block first;
	if there are multiple such nested unwind blocks, try to complete the outer-most one; the inner
	blocks will be completed in the process."
	ctxt _ suspendedContext.
	[(ctxt _ ctxt findNextUnwindContextUpTo: nil) isNil] whileFalse: 
		"Contexts under evaluation have already set their complete (tempAt: 2) to true."
		[(ctxt tempAt:2) ifNotNil: [outerMost _ ctxt]].
	outerMost ifNotNil: [
		"This is the outer-most unwind context currently under evaluation;
		let's find an inner context executing outerMost's argument block (tempAt: 1)"
		(suspendedContext findContextSuchThat: [:ctx | 
			ctx closure == (outerMost tempAt: 1)]) ifNotNil: [:inner | 
				"Let's finish the unfinished unwind context only (i.e. up to inner) and return here"
				suspendedContext runUntilErrorOrReturnFrom: inner.
				"Update the receiver's suspendedContext (the previous step reset its sender to nil); 
				return, if the execution stack reached its bottom (e.g. in case of non-local returns)."
				(suspendedContext _ outerMost sender) ifNil: [^self]]].

	"Now all unwind blocks caught halfway through have been completed; 
	let's execute the ones still pending. Note: #findNextUnwindContextUpTo: starts
	searching from the receiver's sender but the receiver itself may be an unwind context."
	ctxt _ suspendedContext.
	ctxt isUnwindContext ifFalse: [ctxt _ ctxt findNextUnwindContextUpTo: nil].
	[ctxt isNil] whileFalse: [
		(ctxt tempAt: 2) ifNil: [
			ctxt tempAt: 2 put: true.
			unwindBlock _ ctxt tempAt: 1.
			"Create a context for the unwind block and execute it on the unwind block's stack. 
			Note: using #value instead of #runUntilErrorOrReturnFrom: would lead to executing 
			the unwind on the wrong stack preventing the correct execution of non-local returns."
			suspendedContext _ unwindBlock asContextWithSender: ctxt.
			suspendedContext runUntilErrorOrReturnFrom: suspendedContext].
		ctxt _ ctxt findNextUnwindContextUpTo: nil].

	"Reset the context's pc and sender to nil for the benefit of isTerminated."
	suspendedContext terminate]! !

!Process methodsFor: 'testing' stamp: 'jar 4/8/2021 23:39:52'!
isTerminated
	"Answer if the receiver is terminated. A process is considered terminated
	if the suspendedContext is the bottomContext and the pc is at the endPC"

	self isRunning ifTrue: [^ false].
	^suspendedContext isNil or: [
		suspendedContext isBottomContext and: [
			suspendedContext isDead or: [suspendedContext atEnd]]]
! !


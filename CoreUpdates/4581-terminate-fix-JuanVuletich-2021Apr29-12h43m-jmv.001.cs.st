'From Cuis 5.0 [latest update: #4577] on 29 April 2021 at 12:45:09 pm'!

!Process methodsFor: 'changing process state' stamp: 'jmv 4/29/2021 12:44:59'!
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
				outerMost _ (suspendedContext runUntilErrorOrReturnFrom: inner) first.
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


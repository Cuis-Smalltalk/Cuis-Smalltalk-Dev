'From Cuis 6.0 [latest update: #5050] on 27 January 2022 at 3:10:23 pm'!

!Process methodsFor: 'test support' stamp: 'jar 6/1/2021 11:57:00'!
terminateFromForkedProcess
	"Stop the process that the receiver represents forever. Unwind to execute pending ensure:/ifCurtailed: blocks before terminating.
	Process termination and exception handling in border cases has been the subject of a deep overhaul in the first months of 2021, by Jaromir Matas.
	See, for instance
		https://lists.cuis.st/mailman/archives/cuis-dev/2021-May/003171.html
		https://lists.cuis.st/mailman/archives/cuis-dev/2021-June/003187.html
	You can also look for other related mail threads in the Cuis mail list.
	Many new tests were added to BaseImageTests.pck.st
	Thank you Jaromir for this important contribution!!
	Note: This implementation was replaced by a new one, also from Jaromir, on January, 2022. This new implementation
	instead of using a separate process, use a separate, cloned stack, from the same process.
	We are keeping the 2021 approach, terminating from a new, forked process, in this method,
	just in case it proves useful too."

	| ctxt unwindBlock oldList outerMost top newTop |
	self isRunning ifTrue: [
		"If terminating the active process, suspend it first and terminate it as a suspended process."
		[self terminate] fork.
		^self suspend].

	[ "run the whole termination wrapped in #valueEnsured to ensure unwind is completed 	even if 
	the process terminating another process gets terminated - see #testTerminateInTerminate"
	"Always suspend the process first so it doesn't accidentally get woken up"
	oldList _ self suspend.
	suspendedContext ifNil: [^self]. "self is already terminated"
	"Figure out if we are terminating the process while waiting in Semaphore>>critical:
	In this case, pop the suspendedContext so that we leave the ensure: block inside
	Semaphore>>critical: without signaling the semaphore."
	(oldList class == Semaphore and:[
		suspendedContext method == (Semaphore compiledMethodAt: #critical:)]) ifTrue: [
			suspendedContext _ suspendedContext home.
	].

	top _ suspendedContext.
	suspendedContext _ nil. "disable this process while running its stack in active process below"
	"If terminating a process halfways through an unwind, try to complete that unwind block first;
	if there are multiple such nested unwind blocks, try to complete the outer-most one; nested 
	unwind blocks will be completed in the process. Halfway-through blocks have already set the 
	complete variable (tempAt: 2) in their defining #ensure:/#ifCurtailed contexts from nil to true.
	Note: #findNextUnwindContextUpTo: starts searching from the receiver's sender but the receiver 
	itself may be an unwind context."
	ctxt _ top.
	ctxt isUnwindContext ifFalse: [ctxt _ ctxt findNextUnwindContextUpTo: nil].
	[ctxt isNil] whileFalse: [
		(ctxt tempAt:2) ifNotNil: [
			outerMost _ ctxt].
		ctxt _ ctxt findNextUnwindContextUpTo: nil].
	outerMost ifNotNil: [newTop _ self complete: top to: outerMost].

	"By now no halfway-through unwind blocks are on the stack. Create a new top context for each 
	pending unwind block (tempAt: 1) and execute it on the unwind block's stack. 
	Note: using #value instead of #complete:to: would lead to incorrect evaluation of non-local returns.
	Note: newTop sender points to the former outerMost sender, i.e. the next unexplored context."
	ctxt _ newTop ifNil: [top] ifNotNil: [newTop sender].
	ctxt isUnwindContext ifFalse: [ctxt _ ctxt findNextUnwindContextUpTo: nil].
	[ctxt isNil] whileFalse: [
		(ctxt tempAt: 2) ifNil: [
			ctxt tempAt: 2 put: true.
			unwindBlock _ ctxt tempAt: 1.
			top _ unwindBlock asContextWithSender: ctxt.
			self complete: top to: top].
		ctxt _ ctxt findNextUnwindContextUpTo: nil]
	] valueEnsured! !


'From Cuis 6.0 [latest update: #5038] on 18 January 2022 at 7:34:47 pm'!

!ContextPart methodsFor: 'private-exceptions' stamp: 'jar 1/18/2022 18:57:25'!
runUntilReturnFrom: aContext
	"Run the receiver (which must be a stack top context) until aContext returns. Avoid a context that cannot return. 
	Note: to avoid infinite recursion of MNU error inside unwind blocks, implement e.g. a wrapper around the message 
	sentTo: receiver in #doesNotUnderstand:. Note: This method was meant to be used exclusively by #unwindTo."

	| here unwindBottom newTop |
	here _ thisContext.
	"Avoid a context that cannot return (see Note 1 below)"
	unwindBottom _ (self findContextSuchThat: [:ctx | ctx selector = #cannotReturn:]) ifNil: [aContext].
	newTop _ aContext sender.
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


!ContextPart methodsFor: 'private-exceptions' stamp: 'jar 1/18/2022 18:55:12'!
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
	outerMost ifNotNil: [top _ (top runUntilReturnFrom: outerMost) sender].

	"By now no halfway-through unwind blocks are on the stack. Create a new top context for each 
	pending unwind block (tempAt: 1) and execute it on the unwind block's stack to execute non-local 
	returns correctly. Cf. the unwind pattern in #resume:through:. In #unwindTo, using #value instead of 
	#runUntilReturnFrom: would lead to an incorrect evaluation of non-local returns on the wrong stack. 
	Note: top points to the former outerMost sender now, i.e. to the next unexplored context."
	ctxt _ top.
	ctxt isUnwindContext ifFalse: [ctxt _ ctxt findNextUnwindContextUpTo: aContext].
	[ctxt isNil] whileFalse: [
		(ctxt tempAt: 2) ifNil: [
			ctxt tempAt: 2 put: true.
			top _ (ctxt tempAt: 1) asContextWithSender: ctxt.
			top runUntilReturnFrom: top].
		ctxt _ ctxt findNextUnwindContextUpTo: aContext]
! !


!Process methodsFor: 'changing process state' stamp: 'jar 1/18/2022 19:30:02'!
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
	 side effect of the suspension; for pre-2022 VMs #suspend returns a list the process was
	 previously waiting on; figure out if we are terminating the process while waiting in
	 Semaphore>>critical:. In this case, pop the suspendedContext so that we leave the ensure:
	 block inside Semaphore>>critical: without signaling the semaphore.
	 Execute termination wrapped in #valueEnsured to ensure it completes even if the 
	 terminator process itself gets terminated before it's finished; see testTerminateInTerminate."

	| top |
	self isActiveProcess ifTrue: [
		top _ thisContext.
		^[top unwindTo: nil. self suspend] asContext jump].

	[	| oldList |
		suspendedContext isContext ifFalse: [^self].
		oldList _ self suspend.
		(oldList class == Semaphore and:[
			suspendedContext method == (Semaphore compiledMethodAt: #critical:)]) ifTrue: [
				suspendedContext _ suspendedContext home	].
		top _ suspendedContext.
		suspendedContext _ [top unwindTo: nil. self suspend] asContext.
		self priority: Processor activePriority + 1; resume
	] valueEnsured! !


!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' closureCopy:copiedValues: jump pop push: quickSend:to:with:super: restart resume resume: resume:through: resumeEvaluating: return return: return:through: return:to: runUntilErrorOrReturnFrom: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: insertSender: isPrimFailToken: privSender: push:fromIndexable: runUnwindUntilErrorOrReturnFrom: stackPtr stackp: tryNamedPrimitiveIn:for:withArgs:)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext runUntilReturnFrom: unwindTo:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
('temporaries' tempAt: tempAt:put:)
!


!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend terminate)
('changing suspended state' activateReturn:value: canSearchForSignalerContextOf: complete: completeStep: completeStepUpTo: completeTo: ifSuspendedContextIsUnhandledErrorDo: install: isSuspendedContextSignalUnhandledError popTo: popTo:value: restartTop restartTopWith: return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' basicSuspendedContext calleeOf: copyStack name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString storeOn:)
('private' complete:to: suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debug:title: debug:title:full: debugFullWithTitle:)
('signaling' pvtSignal:list: signal: signalException:)
('test support' suspendPrimitivelyOrFail)
('UI support' animatedUI animatedUI:)
('testing' isActiveProcess isBlocked isReady isRunning isSuspended isTerminated isTheLastPreempted)
!


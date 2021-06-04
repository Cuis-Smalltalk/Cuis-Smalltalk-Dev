'From Cuis 5.0 [latest update: #4587] on 1 June 2021 at 1:31:25 pm'!
!classDefinition: #ProceedBlockCannotReturn category: #'Exceptions Kernel'!
Warning subclass: #ProceedBlockCannotReturn
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions Kernel'!

!ContextPart methodsFor: 'private' stamp: 'jar 6/1/2021 10:06:30'!
runUnwindUntilErrorOrReturnFrom: aSender 
	"ASSUMES aSender is a sender of self.  Execute self's stack until aSender returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSender returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSender that jumps back to thisContext when evaluated.  We also insert an exception handler under aSender that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."
	"This method is used by Process>>#complete:to: for unwinding during termination."

	| error ctxt here topContext |
	here _ thisContext.

	"Insert ensure and exception handler contexts under aSender"
	error _ nil.
	ctxt _ aSender insertSender: (ContextPart
		contextOn: UnhandledError do: [:ex |
			error ifNil: [
				error _ ex exception.
				topContext _ thisContext.
				here jump.
				ex signalerContext restart] "re-signal the error if jumped back"
			ifNotNil: [ex pass]
		]).
	ctxt _ ctxt insertSender: (ContextPart
		contextEnsure: [error ifNil: [
				topContext _ thisContext.
				here jump]
		]).
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [ "No error was raised, return the sender of the above ensure context (see Note 1)"
		{ctxt sender. nil}

	] ifNotNil: [ "Error was raised, remove inserted above contexts then return signaler context"
		aSender terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	]

"Note 1: It doesn't matter 'ctxt sender' is not a proper top context because #terminate will use it only as a starting point in the search for the next unwind context and the computation will never return here. Removing the inserted ensure context (i.e. ctxt) by stepping until popped (as in #runUntilErrorOrReturnFrom:) when executing non-local returns is not applicable here and would fail testTerminationDuringNestedUnwindWithReturn1 through 4."
! !


!Process methodsFor: 'private' stamp: 'jar 6/1/2021 10:14:38'!
complete: topContext to: aContext 
	"Run topContext on behalf of self on topContext's stack until aContext is popped or an unhandled 
	error is raised. Return self's new top context. Note: topContext must be a stack top context.
	Note: This method is meant to be called primarily by Process>>#terminate."

	| pair top error |
	pair _ topContext runUnwindUntilErrorOrReturnFrom: aContext.
	top _ pair first.
	error _ pair second.
	"If an error was detected jump back to the debugged process and re-signal the error;
	some errors may require a special care - see notes below."
	error ifNotNil: [
		error class == ProceedBlockCannotReturn ifTrue: [^top]. "do not jump back"
		error class == MessageNotUnderstood ifTrue: [error initialize]. "reset reachedDefaultHandler"
		top jump].
	^top

"Note 1: To prevent an infinite recursion of the MessageNotUnderstood error, reset reachedDefaultHandler before jumping back; this will prevent #doesNotUnderstand: from resending the unknown message.
Note 2; To prevent returning from the BlockCannotReturn error, do not jump back when ProceedBlockCannotReturn warning has been raised."! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'jar 6/1/2021 12:14:16'!
return: value from: aSender 
	"For simulation.  Roll back self to aSender and return value from it.  Execute any unwind blocks on the way.  ASSUMES aSender is a sender of self"

	| newTop |
	aSender isDead ifTrue: [
		^ self send: #cannotReturn: to: self with: {value} super: false].
	newTop _ aSender sender.
	(self findNextUnwindContextUpTo: newTop) ifNotNil: [
		^ self send: #aboutToReturn:through: to: self with: {value. nil} super: false].
	self releaseTo: newTop.
	newTop ifNotNil: [newTop push: value].
	^ newTop
! !

!ContextPart methodsFor: 'controlling' stamp: 'jar 6/1/2021 12:16:39'!
resume: value through: firstUnwindCtxt
	"Unwind thisContext to self and resume with value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	| ctxt unwindBlock |
	self isDead ifTrue: [self cannotReturn: value to: self].
	ctxt _ firstUnwindCtxt ifNil: [thisContext findNextUnwindContextUpTo: self].
	[ctxt isNil] whileFalse:
		[(ctxt tempAt: 2) ifNil:
			[ctxt tempAt: 2 put: true.
			 unwindBlock _ ctxt tempAt: 1.
			 thisContext terminateTo: ctxt.
			 unwindBlock value].
		 ctxt _ ctxt findNextUnwindContextUpTo: self].
	thisContext terminateTo: self.
	^value
! !


!MethodContext methodsFor: 'private' stamp: 'jar 6/1/2021 13:22:36'!
cannotReturn: result
	closureOrNil ifNotNil: [
		self cannotReturn: result to: self home sender.
		ProceedBlockCannotReturn new signal: 'This block has ended, continue with sender?'.
		^thisContext privSender: self sender].
	Debugger
		openContext: thisContext
		label: 'computation has been terminated'
		contents: nil! !


!Process methodsFor: 'test support' stamp: 'jar 6/1/2021 11:57:00'!
terminate 
	"Stop the process that the receiver represents forever. Unwind to execute pending ensure:/ifCurtailed: blocks before terminating.
	Process termination and exception handling in border cases has been the subject of a deep overhaul in the first months of 2021, by Jaromir Matas.
	See, for instance
		https://lists.cuis.st/mailman/archives/cuis-dev/2021-May/003171.html
		https://lists.cuis.st/mailman/archives/cuis-dev/2021-June/003187.html
	You can also look for other related mail threads in the Cuis mail list.
	Many new tests were added to BaseImageTests.pck.st
	Thank you Jaromir for this important contribution!!"

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


!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size tempAt: tempAt:put:)
('instruction decoding' doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: longStack messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' closureCopy:copiedValues: hasSender: jump pop push: quickSend:to:with:super: restart resume resume: resume:through: resumeEvaluating: return return: return:through: return:to: runUntilErrorOrReturnFrom: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: insertSender: isPrimFailToken: privSender: push:fromIndexable: runUnwindUntilErrorOrReturnFrom: simulatedAboutToReturn:through: stackPtr stackp: tryNamedPrimitiveIn:for:withArgs:)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext unwindTo:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
!


!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend)
('changing suspended state' activateReturn:value: canSearchForSignalerContextOf: complete: completeStep: completeStepUpTo: completeTo: ifSuspendedContextIsUnhandledErrorDo: install: isSuspendedContextSignalUnhandledError popTo: popTo:value: restartTop restartTopWith: return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' basicSuspendedContext calleeOf: copyStack name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString)
('private' complete:to: suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debug:title: debug:title:full: debugFullWithTitle:)
('signaling' pvtSignal:list: signal: signalException:)
('test support' suspendPrimitivelyOrFail terminate)
('UI support' animatedUI animatedUI:)
('testing' isActiveProcess isBlocked isReady isRunning isSuspended isTerminated isTheLastPreempted)
!


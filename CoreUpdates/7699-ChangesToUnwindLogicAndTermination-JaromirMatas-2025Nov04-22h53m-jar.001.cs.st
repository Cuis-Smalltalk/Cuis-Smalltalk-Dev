'From Cuis7.5 [latest update: #7681] on 9 November 2025 at 9:26:31 pm'!

!ContextPart methodsFor: 'controlling' stamp: 'jar 11/4/2025 23:13:21'!
returnEvaluating: aBlock
	"Unwind thisContext to self's sender and resume with aBlock value as result of last send'.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self's sender is a sender of thisContext."

	^thisContext unwindAndResume: sender evaluating: aBlock! !

!ContextPart methodsFor: 'private' stamp: 'jar 11/9/2025 16:56:50'!
finishIncompleteUnwindsUpTo: aContext
	"A helper method to Process #unwindTo:safely:. Find out if there are unfinished
	 unwind blocks between self and aContext and complete the outer-most one;
	 all nested unwind blocks will be completed in the process.
	 Answer self or the outer-most unfinished context's sender."
	
	| ctx outerMost |
	ctx := self.
	ctx isUnwindContext ifFalse: [ctx := ctx findNextUnwindContextUpTo: aContext]. "Note 1"
	[ctx isNil] whileFalse: [  "ctx is an #ensure: or #ifCurtailed: context"
		(ctx tempAt: 2) == #unwindRunnable ifTrue: [outerMost := ctx]. "Note 2"
		ctx := ctx findNextUnwindContextUpTo: aContext].
	outerMost ifNotNil: [^self runUntilReturnFrom: outerMost]

"Note 1: #findNextUnwindContextUpTo: starts searching from the receiver's sender hence we must check the receiver whether it is an unwind context; see testTerminateEnsureAsStackTop.

Note 2: Halfway-through unwind blocks have already set their completionStatus variable (ctx tempAt: 2) in their defining #ensure:/#ifCurtailed contexts to #unwindRunnable which means the unwind block execution has started but not finished yet; we'll search for the outer-most one and finish its execution."! !

!ContextPart methodsFor: 'private' stamp: 'jar 11/4/2025 23:22:46'!
stepToSenderOrNil
	"Step to sender or step to return and answer nil in case sender cannot be returned to."

	| ctxt |
	ctxt := self.
	[(ctxt willReturn and: [ctxt sender isNil or: [ctxt sender isDead]]) not and: [(ctxt := ctxt step) == self or: [ctxt hasSender: self]]] whileTrue.
	ctxt == self ifTrue: [^nil].
	^ctxt! !

!ContextPart methodsFor: 'private' stamp: 'jar 11/9/2025 16:57:48'!
unwindAndResume: aContext evaluating: aBlock
	"Unwind the receiver to aContext and resume with aBlock value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES aContext is a sender of thisContext."
	
	| ctx |
	ctx := self.
	ctx isUnwindContext ifFalse: [ctx := ctx findNextUnwindContextUpTo: aContext]. "Note 1"
	[ctx isNil] whileFalse: [  "ctx is an #ensure: or #ifCurtailed: context"
		(ctx tempAt: 2) ifNil: [ | unwindBlock |   "unwind block hasn't run yet; see #ensure:"
			ctx tempAt: 2 put: #unwindRunnable.
			unwindBlock := ctx tempAt: 1.   "#ensure:/ifCurtailed: argument block"
			thisContext terminateTo: ctx.   "Note 2"
			unwindBlock value.   "execute the unwind block"
			ctx tempAt: 2 put: #unwindComplete].   "unwind block has been completed"
		ctx := ctx findNextUnwindContextUpTo: aContext].
	thisContext terminateTo: aContext.
	^aBlock value

"Note 1: #findNextUnwindContextUpTo: starts searching from the receiver's sender hence we must check the receiver whether it is an unwind context; see testTerminateEnsureAsStackTop.

Note 2: Non-local returns must be evaluated in the evaluation context (sender chain) in which they were defined. Because the unwind algorithm runs on top of the context chain being unwound we need to keep removing parts of the sender chain so that each unwind block is executed in the right evaluation context."! !

!ContextPart methodsFor: 'private' stamp: 'jar 11/9/2025 16:58:06'!
unwindAndStop
	"A helper method to Process #terminate. Create and answer
	 a helper stack for a terminating process to unwind itself from.
	 Note: push a fake return value to create a proper top context."

	^(self class contextEnsure: [self unwindTo: nil]) push: nil; 
		privSender: [Processor activeProcess suspend] asContext! !


!BlockClosure methodsFor: 'exceptions' library: #MinimalKernel01 stamp: 'jar 11/9/2025 18:01:30'!
ensure: aBlock
	"Evaluate a termination block after evaluating the receiver, regardless of
	 whether the receiver's evaluation completes.  N.B.  This method is *not*
	 implemented as a primitive.  Primitive 198 always fails.  The VM uses prim
	 198 in a context's method as the mark for an ensure:/ifCurtailed: activation."

	| completionStatus returnValue |
	<primitive: 198>
	returnValue := self valueNoContextSwitch.
	completionStatus ifNil:[
		completionStatus := #unwindRunnable.
		aBlock value.
	].
	^returnValue

"The completionStatus temporary variable tracks the progress of the #ensure:/#ifCurtailed: contexts execution using values:
	nil | #unwindRunnable | #unwindComplete
nil indicates the receiver block execution is in progress, #unwindRunnable indicates the receiver block execution has finished but the unwind block (the argument) is still in progress, and #unwindComplete indicates the execution of the unwind block has finished.

The completionStatus variable is being used by the unwinding machinery (#unwindAndResume:evaluating:, #unwindTo:safely:, and #finishIncompleteUnwindsUpTo:) during abnormal termination (i.e. non-local return or termination) to find all unexecuted or partially executed unwind blocks in the sender chain that need to be executed. Note that during an abnormal termination the sender chain is not executed, but only searched for unwind contexts that may contain unexecuted unwind blocks. The completionStatus variable is then accessed by the execution machinery via `ctx tempAt: 2` and the unwind block (#ensure:/#ifCurtailed: argument) as `ctx tempAt: 2`, where ctx refers to the #ensure:/#ifCurtailed: context found in the sender chain.

Originally, only two values (nil | true) were used to indicate whether the receiver block had already run. We introduce a third value to mark the completion of unwind blocks to prevent erroneous repeat evaluation of unwind blocks during process termination. The VM may suspend the execution of the #unwindAndResume:evaluating: or #unwindTo:safely: methods at the suspension point at the target of the backward jump. Without tracking the completion status of unwind blocks, the #finishIncompleteUnwindsUpTo: would incorrectly evaluate the unwind block twice when the process was terminated at such a suspension point. See also #testTerminateEverywhere tests."! !

!BlockClosure methodsFor: 'exceptions' stamp: 'jar 11/9/2025 16:55:17'!
ifCurtailed: aBlock
	"Evaluate the receiver with an abnormal termination action.
	 Evaluate aBlock only if execution is unwound during execution
	 of the receiver.  If execution of the receiver finishes normally do
	 not evaluate aBlock.  N.B.  This method is *not* implemented as a
	 primitive.  Primitive 198 always fails.  The VM uses prim 198 in a
	 context's method as the mark for an ensure:/ifCurtailed: activation."
	| completionStatus returnValue |
	<primitive: 198>
	returnValue := self valueNoContextSwitch.
	completionStatus := #unwindRunnable.
	^returnValue! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'jar 11/4/2025 23:06:39'!
return: value from: aSender 
	"For simulation.  Roll back self to aSender and return value from it.  Execute any unwind blocks on the way.  ASSUMES aSender is a sender of self"

	| newTop |
	newTop := aSender sender.
	(aSender isDead or: [newTop isNil or: [newTop isDead]]) ifTrue:
		[^self pc: nil; send: #cannotReturn: to: self with: {value}].
	(self findNextUnwindContextUpTo: newTop) ifNotNil: 
		[:unwindProtectCtxt|
		^self send: #aboutToReturn:through: to: self with: {value. unwindProtectCtxt}].
	self releaseTo: newTop.
	newTop ifNotNil: [newTop push: value].
	^newTop! !

!ContextPart methodsFor: 'controlling' stamp: 'jar 11/4/2025 23:10:40'!
restart
	"Unwind thisContext to self and resume from beginning. 
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	^thisContext unwindAndResume: self evaluating: [self privRefresh]! !

!ContextPart methodsFor: 'controlling' stamp: 'jar 11/4/2025 23:11:55'!
resume: value
	"Unwind thisContext to self and resume with value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	^thisContext unwindAndResume: self evaluating: [value]! !

!ContextPart methodsFor: 'controlling' stamp: 'jar 11/4/2025 23:12:33'!
resumeEvaluating: aBlock
	"Unwind thisContext to self and resume with aBlock value as result of last send. 
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	^thisContext unwindAndResume: self evaluating: aBlock! !

!ContextPart methodsFor: 'controlling' stamp: 'jar 11/4/2025 23:12:55'!
return: value
	"Unwind thisContext to self's sender and resume with value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self's sender is a sender of thisContext."

	^thisContext unwindAndResume: sender evaluating: [value]! !

!ContextPart methodsFor: 'controlling' stamp: 'jar 11/4/2025 23:29:42'!
runUntilErrorOrReturnFrom: aSenderContext
	"ASSUMES aSenderContext is a sender of self.  Execute self's stack until aSenderContext returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSenderContext returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSenderContext that jumps back to thisContext when evaluated.  We also insert an exception handler under aSenderContext that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error exceptionHandlerContext ensureContext ctxt here topContext |
	aSenderContext method isQuick ifTrue: [ 
		^ {aSenderContext step. nil}
	].
	
	here := thisContext.

	"Insert ensure and exception handler contexts under aSenderContext.
	As Halt may not raise an UnhandledError (see #noHandler), it may need to be handled explicitly here."
	error := nil.
	exceptionHandlerContext := ContextPart
		contextOn: UnhandledError, UnhandledHalt do: [ :unhandledError |
			error ifNil: [
				error := unhandledError exception.
				topContext := thisContext.
				Debugger updateDebuggersLeadingTo: aSenderContext for: error.
				unhandledError resumeUnchecked: here jump ]
			ifNotNil: [unhandledError pass ]].
	ctxt := aSenderContext insertSender: exceptionHandlerContext.
	ensureContext := ContextPart
		contextEnsure: [
			error ifNil: [
				topContext := thisContext.
				here jump ]].
	ctxt := ctxt insertSender: ensureContext.
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [
		"No error was raised, remove ensure context by stepping until popped"
		[ctxt isDead or: [topContext isNil]] whileFalse: [topContext := topContext stepToSenderOrNil].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSenderContext terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].! !

!ContextPart methodsFor: 'private' stamp: 'jar 11/9/2025 17:16:33'!
runUntilReturnFrom: unwindBottom
	"Run the receiver (which must be a stack top context) until unwindBottom context returns. 
	 This is a helper method for #unwindTo:safely: to unwind non-local returns inside unwind blocks."
	
	| here newTop |
	here := thisContext.
	newTop := unwindBottom sender.  "Note 1"
	unwindBottom insertSender: (self class contextEnsure: [here jump]).  "Note 2"
	self jump.  "run the receiver until the above inserted ensure block is executed"
	^newTop  "control resumes here once the above inserted ensure block is executed"
	
"Note 1: It doesn't matter newTop is not a proper stack top context because the unwind machinery will only use it as a starting point in the search for the next unwind context and the computation will never return here. We could make newTop a proper top context by pushing nil to its stack (^newTop push: nil) if need be (see #jump comments).
	
Note 2: The receiver (self) is run by jumping directly to it (the active process abandons thisContext and executes self on its own stack; self must be the top context). However, before jumping to self we insert an ensure block under the unwindBottom context that will execute a jump back to thisContext.

To avoid an infinite recursion of MNU errors inside unwind blocks, Object>>doesNotUnderstand: needs to detect and prevent it.

To avoid a VM crash during termination, #cannotReturn:to: has to make sure its sender's pc is nilled before returning. To try, **save your image**, comment out `pc := nil.` in #cannotReturn:to: and run the following Workspace example:
	 ([[] ensure: [^2]] forkAt: Processor activePriority + 1) terminate
"! !

!ContextPart methodsFor: 'private' stamp: 'jar 11/9/2025 18:13:35'!
unwindTo: aContext safely: aBoolean
	"Unwind the receiver to aContext. Execute any unwind blocks while unwinding.
	 If aBoolean is false, unwind only blocks that have not run yet, otherwise complete
	 all unwind blocks including those currently in the middle of their execution.
	 Run all unwinds on their original stack."
	
	| ctx |
	ctx := self.
	aBoolean ifTrue: [ctx := (self finishIncompleteUnwindsUpTo: aContext) ifNil: [^self]].
	ctx isUnwindContext ifFalse: [ctx := ctx findNextUnwindContextUpTo: aContext]. "Note 1"
	[ctx isNil] whileFalse: [  "ctx is an #ensure: or #ifCurtailed: context"
		(ctx tempAt: 2) ifNil: [ | top |   "unwind block hasn't run yet; see #ensure:"
			ctx tempAt: 2 put: #unwindRunnable.
			top := (ctx tempAt: 1) asContextWithSender: ctx.   "Note 2"
			top runUntilReturnFrom: top.   "execute the unwind block and return here"
			ctx tempAt: 2 put: #unwindComplete].   "unwind block has been completed"
		ctx := ctx findNextUnwindContextUpTo: aContext]

"Note 1: #findNextUnwindContextUpTo: starts searching from the receiver's sender hence we must check the receiver whether it is an unwind context; see testTerminateEnsureAsStackTop.

Note 2: Create a new top context for each unexecuted unwind block (ctx tempAt: 1) and attach it to its defining context (ctx) on the original stack to execute it in the correct evaluation context (sender chain). Do not allow non-local returns jump out from inside unwind blocks. For more information see #runUntilReturnFrom:.

Cf. the unwind pattern in #unwindAndResume:evaluating:. Using #value instead of #runUntilReturnFrom: for termination would allow non-local returns to escape termination. Note that the approach implemented in this method differs from the intent stated in the comment to #valueUninterruptably. Note, however, that *any* non-local return from an unwind block is specified as undefined by the ANSI standard.

Also note that #unwindTo:safely: can be used to unwind a process (or a sender chain) by another process, as well as to unwind the active process by itself, unlike #unwindAndResume:evaluating:, which only unwinds the active process."! !


!MethodContext methodsFor: 'private' stamp: 'jar 11/4/2025 23:09:12'!
aboutToReturn: result through: firstUnwindContext 
	"Called from VM when an unwindBlock is found between self and its home.
	 Return to home's sender, executing unwind blocks on the way."

	^firstUnwindContext unwindAndResume: self home sender evaluating: [result]! !


!Process methodsFor: 'changing process state' stamp: 'jar 11/9/2025 17:04:50'!
terminate
	"Stop the process that the receiver represents forever. Allow all pending unwind
	 blocks to run before terminating; if they are currently in progress, let them finish.
	 This is the kind of behavior we expect when terminating a healthy process.
	 See further comments in #terminateAggressively and #destroy methods dealing
	 with process termination when closing the debugger or after a catastrophic failure."
	
	self isActiveProcess ifTrue: [^thisContext unwindAndStop jump]. "Note 1"

	[[:contextToUnwind | "Note 2"
	self 
		priority: Processor activePriority;  "Note 3"
		suspendedContext: contextToUnwind unwindAndStop; "Note 4"
		resume
	] valueAndWaitWhileUnwinding: "Note 5" self suspendAndReleaseCriticalSection "Note 6"] valueEnsured

"Note 1: If terminating the active process, create a new stack and run unwinds from there.

Note 2: Execute the termination in #ensure: argument block to guarantee it completes even if the terminator process itself gets terminated before it's finished. Otherwise #testTerminateInTerminate would fail.

Note 3: Adjust the terminating process's priority to prevent the active process from getting stuck waiting on a lower priority process to complete its termination.

Note 4: To allow a suspended process to unwind itself, create a new, parallel stack and resume the suspended process from there to unwind itself.

Note 5: Use a semaphore to make the process that invoked the termination wait for self's completion.

Note 6: If terminating a suspended process (including runnable and blocked), always suspend the process being terminated first so it doesn't accidentally get woken up, and nil the suspended context to prevent accidental resumption or termination with a wrong suspended context while manipulating it. If the process is blocked, waiting to access the #critical: section of a condition variable (a Semaphore or a Mutex), release it properly.

In 2022 a new suspend semantics has been introduced: the revised #suspend backs up a process waiting on a condition variable to the send that invoked the wait state, while the previous #suspend simply removed the process from the condition variable's list it was previously waiting on; see #suspend and #suspendAndUnblock comments."
! !

!Process methodsFor: 'simulate execution' stamp: 'jar 11/9/2025 17:13:42'!
complete: aContext
	"Run self until aContext is popped or an unhandled error is raised.  Return self's new top context, unless an unhandled error was raised then return the signaler context (rather than open a debugger)."

	| ctxt pair error returnContext |
	ctxt := suspendedContext.
	suspendedContext := nil. "disable this process while running its stack in active process below"
	"adjust the returning context to account for a non-local return to avoid the BCR error when stepping over ^2 in [^2] ensure: []"
	((returnContext := aContext) 
		findContextSuchThat: [:ctx | ctx selector == #aboutToReturn:through:]) 
		ifNotNil: [:ctx | returnContext := ctx tempAt: 2].
	pair := Processor activeProcess
		evaluate: [ctxt runUntilErrorOrReturnFrom: returnContext]
		onBehalfOf: self.
	suspendedContext := pair first.
	error := pair second.
	error ifNotNil: [^ error signalerContext].
	^ suspendedContext! !

!methodRemoval: ContextPart #resume:through: stamp: 'jar 11/4/2025 23:12:14'!
ContextPart removeSelector: #resume:through:!
!methodRemoval: ContextPart #return:through: stamp: 'jar 11/4/2025 23:12:18'!
ContextPart removeSelector: #return:through:!
!methodRemoval: ContextPart #unwindAndStop: stamp: 'jar 11/4/2025 23:17:20'!
ContextPart removeSelector: #unwindAndStop:!

!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' directedSuperSend:numArgs: doDup doNop doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' activateMethod:withArgs:receiver: closureCopy:copiedValues: executeMethod:forSelector:withArgs:receiver: jump pop push: quickSend:to:with:lookupIn: restart resume resume: resumeEvaluating: return return: return:to: returnEvaluating: runUntilErrorOrReturnFrom: send:to:with: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: finishIncompleteUnwindsUpTo: insertSender: isPrimFailToken: privSender: push:fromIndexable: runUntilReturnFrom: stackPtr stackp: stepToCalleeOrNil stepToSenderOrNil tryNamedPrimitiveIn:for:withArgs: unwindAndResume:evaluating: unwindAndStop unwindTo:safely: willFailReturn)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext unwindTo:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead isDeadOrAtEnd isDone secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
('temporaries' tempAt: tempAt:put:)
('instruction decoding implicit literals' methodReturnSpecialConstant: pushSpecialConstant: sendSpecial:numArgs:)
('read-only objects' modificationForbiddenFor:at:put: modificationForbiddenFor:instVarAt:put: simulatedObject:instVarAt:put:)
!

"(email sent to the Cuis mail list by Jaromir on 2025Nov18 with this proposed change set)
Hi Juan,

Recently I've done some cleanup/fixing in Squeak and thought you might
want to take a look and perhaps integrate to Cuis. Some of the changes
are already part of the Squeak Trunk, the rest waiting to be merged.

The changes are in the following areas:

1) ContextPart return & resume suite of methods - all simplified and
redirected straight to the unified #unwindAndResume:evaluating: method;
removing some now obsolete (#return:through: and #resume:through:) -
please let me know whether you prefer to go via deprecation like Squeak
or delete them right away (they should not really have been used as
public anywhere, their only purpose was to serve to
#aboutToReturn:through:). No change in functionality, just improved readability.

2) slightly simplified #terminate and #runUntilReturnFrom:

3) the unwind control logic around #ensure:/#ifCurtailed: enriched by an
additional value to track the completion status of the unwinding -> see
#unwindAndResume:evaluating:, #unwindTo:safely:, and
#finishIncompleteUnwindsUpTo: (The reason for this change is a
suspension point on the backward branch of the unwind loop which was
observed to cause termination failures under heavy multithreaded load by Christoph).

4) *finally* really cleanly fixing the 'step-over bug' - see
Process>>complete - and reverting #return:from:

5) fixing a little bug hiding in #runUntilErrorOrReturnFrom:

6) reworked many comments

I'd be very thankful if you could review the changes and let me know if
you spot anything suspicious; tests are green in both Cuis and Squeak.

I look forward to your feedback.

best,
Jaromir"
!


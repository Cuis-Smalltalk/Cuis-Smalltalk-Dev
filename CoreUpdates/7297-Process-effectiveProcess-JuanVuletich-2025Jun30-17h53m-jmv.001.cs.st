'From Cuis7.5 [latest update: #7296] on 1 July 2025 at 11:06:26 am'!

!Process methodsFor: 'debugging' stamp: 'jmv 7/1/2025 09:59:25'!
effectiveProcess
	"If we are debugging a Process, simulating its execution as we step in the Debugger even if it is suspended, answer it.
	Otherwise answer self.
	See #evaluate:onBehalfOf:
	"

	^self environmentAt: #effectiveProcess ifAbsent: self! !

!Process methodsFor: 'debugging' stamp: 'jmv 7/1/2025 10:04:44'!
evaluate: aBlock onBehalfOf: aProcess
	"Evaluate aBlock setting effectiveProcess to aProcess.
	Needed so #effectiveProcess gives the correct answer.
	See senders."

	| oldEffectiveProcess |
	aProcess == self ifTrue: [^ aBlock value]. "Optimization"
	
	oldEffectiveProcess := self environmentAt: #effectiveProcess ifAbsent: nil.
	self environmentAt: #effectiveProcess put: aProcess.
	^ aBlock ensure: [
		oldEffectiveProcess
			ifNotNil: [ self environmentAt: #effectiveProcess put: oldEffectiveProcess]
			ifNil: [ self environmentRemoveKey: #effectiveProcess ifAbsent: nil ]]! !

!Process methodsFor: 'testing' stamp: 'LM 10/10/2023 14:42'!
isInExternalCode
	"In a threaded VM, a Smalltalk Process may run a primitive in the background.
	Answer whether this is currently the case for this Process."

	"Special Object 41 (40 in VM) is the ProcessInExternalCodeTag, which is an empty LinkedList."
	^ myList == (Smalltalk specialObjectsArray at: 41)! !


!Process methodsFor: 'simulate execution' stamp: 'eem 6/23/2014 10:13'!
activateReturn: aContext value: value
	"Activate 'aContext return: value', so execution will return to aContext's sender"

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext activateReturn: aContext value: value]
		onBehalfOf: self! !

!Process methodsFor: 'simulate execution' stamp: 'jmv 7/1/2025 10:10:00'!
complete: aContext
	"Run self until aContext is popped or an unhandled error is raised.  Return self's new top context, unless an unhandled error was raised then return the signaler context (rather than open a debugger)."

	| ctxt pair error |
	ctxt := suspendedContext.
	suspendedContext := nil. "disable this process while running its stack in active process below"
	pair := Processor activeProcess
		evaluate: [ctxt runUntilErrorOrReturnFrom: aContext]
		onBehalfOf: self.
	suspendedContext := pair first.
	error := pair second.
	error ifNotNil: [^ error signalerContext].
	^ suspendedContext! !

!Process methodsFor: 'simulate execution' stamp: 'eem 4/11/2017 12:03'!
completeStep: aContext 
	"Resume self until aContext is on top, or if already on top, complete next step"

	| callee |
	self isTerminated ifTrue:
		[^ nil].
	self suspendedContext == aContext ifFalse:
		[^ self complete: (self calleeOf: aContext)].
	callee := self step.
	callee == aContext ifTrue: [^ callee].
	aContext isDead ifTrue: [^ self suspendedContext].  "returned"
	^ self complete: callee  "finish send"! !

!Process methodsFor: 'simulate execution' stamp: 'eem 6/23/2014 10:19'!
popTo: aContext 
	"Pop self down to aContext by remote returning from aContext's callee.  Unwind blocks will be executed on the way.
	This is done by pushing a new context on top which executes 'aContext callee return' then resuming self until aContext is reached.  This way any errors raised in an unwind block will get handled by senders in self and not by senders in the activeProcess.
	If an unwind block raises an error that is not handled then the popping stops at the error and the signalling context is returned, othewise aContext is returned."

	self == Processor activeProcess ifTrue:
		[^self error: 'The active process cannot pop contexts'].
	^(self calleeOf: aContext)
		ifNil: [aContext]  "aContext is on top"
		ifNotNil:
			[:callee|
			 Processor activeProcess
				evaluate: [self return: callee value: callee receiver]
				onBehalfOf: self]! !

!Process methodsFor: 'simulate execution' stamp: 'eem 6/23/2014 10:19'!
popTo: aContext value: aValue
	"Replace the suspendedContext with aContext, releasing all contexts 
	 between the currently suspendedContext and it."

	self == Processor activeProcess ifTrue:
		[^self error: 'The active process cannot pop contexts'].
	^(self calleeOf: aContext)
		ifNil: [aContext]  "aContext is on top"
		ifNotNil:
			[:callee|
			 Processor activeProcess
				evaluate: [self return: callee value: aValue]
				onBehalfOf: self]! !

!Process methodsFor: 'simulate execution' stamp: 'eem 6/23/2014 10:13'!
return: aContext value: value
	"Pop thread down to aContext's sender.  Execute any unwind blocks on the way.  See #popTo: comment and #runUntilErrorOrReturnFrom: for more details."

	suspendedContext == aContext ifTrue:
		[^Processor activeProcess
			evaluate: [suspendedContext := aContext return: value from: aContext]
			onBehalfOf: self].
	self activateReturn: aContext value: value.
	^self complete: aContext! !

!Process methodsFor: 'simulate execution' stamp: 'eem 9/7/2009 11:19'!
step

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext step]
		onBehalfOf: self! !

!Process methodsFor: 'simulate execution' stamp: 'eem 3/15/2013 09:35'!
step: aContext 
	"Resume self until aContext is on top, or if already on top, do next step"

	^Processor activeProcess
		evaluate:
			[self suspendedContext == aContext
				ifTrue: [suspendedContext := suspendedContext step]
				ifFalse: [self complete: (self calleeOf: aContext)]]
		onBehalfOf: self! !

!Process methodsFor: 'simulate execution' stamp: 'eem 9/7/2009 11:52'!
stepToCallee
	"Step until top context changes"

	Processor activeProcess
		evaluate:
			[| ctxt |
			ctxt := suspendedContext.
			[ctxt == suspendedContext] whileTrue: [
				suspendedContext := suspendedContext step]]
		onBehalfOf: self.
	^suspendedContext! !

!Process methodsFor: 'simulate execution' stamp: 'jmv 7/1/2025 10:36:40'!
stepToHome: requestedContext
	"Resume self until the home of top context is aContext.  Top context may be a block context.
	 Catch any UnhandledErrors that are created while stepping, answering the relevant signalerContext
	 if so. Note that this will cause weird effects if using through to step through UnhandledError
	 code, but as the doctor ordered, don't do that; use over or into instead."

	| requestedHome |
	Processor activeProcess
		evaluate:
			[
				requestedHome := requestedContext home.
				[
					suspendedContext := suspendedContext step.
				
					"We reached the home context of the block we are doing [Through] on."
					suspendedContext home == requestedHome ifTrue: [
						^suspendedContext ].
				
					"But if the requestedHome is dead, meaning that we'll never reach back there,
					stop when we get back to where [Through] was requested."
					requestedHome isDead ifTrue: [
						suspendedContext = requestedContext ifTrue: [
							^suspendedContext ]].
				
					"There's nowhere to continue stepping into. Don't do it."
					(suspendedContext sender isNil or: [suspendedContext sender isDead]) ifTrue: [
						^suspendedContext ].
				
					self ifSuspendedContextIsUnhandledErrorDo: [ :anError |
						anError signalerContext ifNotNil: [ :unhandledErrorSignalerContext |
							self completeStepUpTo: unhandledErrorSignalerContext.
							"Give a debugger a chance to update its title to reflect the new exception"
							 Notification signalToUpdateDebuggerOn: unhandledErrorSignalerContext dueTo: anError.
							^unhandledErrorSignalerContext ]]
				
				] repeat.
			]
		onBehalfOf: self.! !

!Process methodsFor: 'simulate execution' stamp: 'eem 9/7/2009 11:10'!
stepToSendOrReturn

	^Processor activeProcess
		evaluate: [suspendedContext := suspendedContext stepToSendOrReturn]
		onBehalfOf: self! !

!Process methodsFor: 'private' stamp: 'jmv 7/1/2025 10:29:12'!
resolvePendingQuickReturns
	| method |	
	method := suspendedContext method.
	method isQuick ifTrue: [
		Processor activeProcess
			evaluate: [suspendedContext := suspendedContext callPrimitive: method primitive]
			onBehalfOf: self ]! !

!Process methodsFor: 'changing suspended state' stamp: 'eem 3/30/2017 15:44'!
restartTopWith: method
	"Rollback top context and replace with new method.  Assumes self is suspended"

	method isQuick 
		ifTrue: [self popTo: suspendedContext sender]
		ifFalse:
			[suspendedContext method frameSize >= method frameSize
				ifTrue: [suspendedContext privRefreshWith: method]
				ifFalse:
					[self assert: suspendedContext isExecutingBlock not.
					 suspendedContext := MethodContext
												sender: suspendedContext sender
												receiver: suspendedContext receiver
												method: method
												arguments: ((1 to: method numArgs) collect:
																[:i| suspendedContext tempAt: i])]].
! !


!Process class methodsFor: 'instance creation' stamp: 'jmv 7/1/2025 10:40:19'!
forContext: aMethodContext priority: anInteger 
	"Answer an instance of me that has suspended aContext at priority anInteger."

	| newProcess |
	newProcess := self new.
	newProcess priority: anInteger.
	newProcess suspendedContext: aMethodContext.
	^newProcess! !


!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend suspendAndUnblock terminate)
('simulate execution' activateReturn:value: canSearchForSignalerContextOf: complete: completeStep: completeStepUpTo: completeTo: ifSuspendedContextIsUnhandledErrorDo: install: isSuspendedContextSignalUnhandledError popTo: popTo:value: restartTop return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' basicSuspendedContext calleeOf: copyStack name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString storeOn:)
('private' resolvePendingQuickReturns suspendAndReleaseCriticalSection suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debugContext:title: debugWithTitle: effectiveProcess evaluate:onBehalfOf:)
('signaling' signalException:)
('test support' suspendPrimitivelyOrFail)
('testing' isActiveProcess isBlocked isInExternalCode isReady isRunning isSuspended isTerminated isTheLastPreempted)
('process state' environmentAt:ifAbsent: environmentAt:ifAbsentPut: environmentAt:put: environmentRemoveKey:ifAbsent:)
('changing suspended state' restartTopWith:)
!


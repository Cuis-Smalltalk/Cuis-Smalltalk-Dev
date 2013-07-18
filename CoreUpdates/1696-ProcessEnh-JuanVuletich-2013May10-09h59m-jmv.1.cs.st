'From Cuis 4.1 of 12 December 2012 [latest update: #1695] on 10 May 2013 at 1:25:50.654692 pm'!

!Process methodsFor: 'printing' stamp: 'jmv 5/10/2013 13:19'!
browserPrintStringFull: aBoolean

	^String streamContents: [ :stream |
		stream
			nextPutAll: self statusString;
			space.
		priority printOn: stream.
		stream nextPutAll: ' ('.
		self hash printOn: stream.
		stream
			nextPutAll: ') ';
			nextPutAll: self name.
		self animatedUI ifNotNil: [ :ui |
			stream nextPutAll: ' - '.
			ui printOn: stream ].
		aBoolean ifTrue: [
			stream
				nextPutAll: ': ';
				nextPutAll: suspendedContext asString ]]! !

!Process methodsFor: 'printing' stamp: 'jmv 5/10/2013 13:10'!
statusString
	self isRunning ifTrue: [ ^ 'running' ].
	self isReady ifTrue: [ ^ 'ready' ].
	self isSuspended ifTrue: [ ^ 'suspended' ].
	self isBlocked ifTrue: [ ^ 'blocked' ].
	self isTerminated ifTrue: [ ^'terminated' ].
	^'unknown (bug?)'! !

!Process methodsFor: 'testing' stamp: 'jmv 5/10/2013 12:20'!
isActiveProcess
	"Are we the cunrrently running process?
	Just for compatibility."
	^self isRunning! !

!Process methodsFor: 'testing' stamp: 'jmv 5/10/2013 13:03'!
isBlocked
	"Answer true if blocked on a semaphore."
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList class == Semaphore! !

!Process methodsFor: 'testing' stamp: 'jmv 5/10/2013 13:24'!
isReady
	"True if not running right now, but might be scheduled anytime.

	Useful to play with:

	| s |	
	s _ Semaphore new.
	s inspect.
	([ s wait . 10000 timesRepeat: [ 10000 factorial. (Delay forSeconds: 1) wait. 7 print ]] newProcess name: 'test5') inspect
	
	{ self isRunning . self isReady . self isSuspended . self isBlocked . self isTerminated } print
	"
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList class == LinkedList! !

!Process methodsFor: 'testing' stamp: 'jmv 5/10/2013 11:34'!
isRunning
	"Are we the cunrrently running process?"
	^ self == Processor activeProcess! !

!Process methodsFor: 'testing' stamp: 'jmv 5/10/2013 13:00'!
isTheLastPreempted
	"Answer wether I am the last process that was preempted"
	^self == Processor preemptedProcess! !


!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 5/10/2013 11:25'!
spyEvery: millisecs on: aBlock
	"Create a spy and spy on the given block at the specified rate."
	"Spy only on the active process (in which aBlock is run)"

	| myDelay time0 observedProcess |
	aBlock isBlock
		ifFalse: [ self error: 'spy needs a block here' ].
	self class: aBlock receiver class method: aBlock method.
		"set up the probe"
	observedProcess _ Processor activeProcess.
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time localMillisecondClock.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[ true ] whileTrue: [
			| startTime |
			startTime := Time localMillisecondClock.
			myDelay wait.

			self
				tally: Processor preemptedProcess suspendedContext
				in: (observedProcess isTheLastPreempted ifTrue: [observedProcess])
				"tally can be > 1 if ran a long primitive"
				by: (Time localMillisecondClock - startTime) // millisecs].
		nil] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	^ aBlock ensure: [
		"cancel the probe and return the value"
		"Could have already been terminated. See #terminateTimerProcess"
		Timer ifNotNil: [
			Timer terminate.
			Timer _ nil ].
		"Collect gc statistics"
		Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
			gcVal isNumber ifTrue: [
				gcStats at: idx put: (gcVal - (gcStats at: idx))]].
		time := Time localMillisecondClock - time0]! !

!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 5/10/2013 11:25'!
spyEvery: millisecs onProcess: aProcess forMilliseconds: msecDuration
	"Create a spy and spy on the given process at the specified rate."
	| myDelay time0 endTime observedProcess sem |
	(aProcess isKindOf: Process)
		ifFalse: [self error: 'spy needs a Process here'].
	self class: aProcess suspendedContext receiver class method: aProcess suspendedContext method.
	"set up the probe"
	observedProcess _ aProcess.
	myDelay _ Delay forMilliseconds: millisecs.
	time0 _ Time localMillisecondClock.
	endTime _ time0 + msecDuration.
	sem _ Semaphore new.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
			[
				| startTime |
				startTime _ Time localMillisecondClock.
				myDelay wait.
				self
					tally: Processor preemptedProcess suspendedContext
					in: (observedProcess isTheLastPreempted ifTrue: [ observedProcess ])
					"tally can be > 1 if ran a long primitive"
					by: (Time localMillisecondClock - startTime) // millisecs.
				startTime < endTime
			] whileTrue.
			sem signal.
		] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	"activate the probe and wait for it to finish"
	sem wait.
	"Collect gc statistics"
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
		gcVal isNumber ifTrue: [
			gcStats at: idx put: (gcVal - (gcStats at: idx))]].
	time _ Time localMillisecondClock - time0! !


!Process methodsFor: 'changing process state' stamp: 'jmv 5/10/2013 12:12'!
resume
	"Allow the process that the receiver represents to continue. Put  
	the receiver in line to become the activeProcess. Check for a nil 
	suspendedContext, which indicates a previously terminated Process that 
	would cause a vm crash if the resume attempt were permitted.
	
	This resumes the receiver if
		- It was never scheduled
		- it is suspended (was sent #suspend)
		- It is ready to run (was preempted). In this case there is no effect.
		- it is waiting on a semaphore. In this case, the semaphore is ignored.
	"

	suspendedContext ifNil: [^ self primitiveFailed].
	^ self primitiveResume! !

!Process methodsFor: 'changing process state' stamp: 'jmv 5/10/2013 11:32'!
terminate 
	"Stop the process that the receiver represents forever.  Unwind to execute pending ensure:/ifCurtailed: blocks before terminating."

	| ctxt unwindBlock oldList |
	self isRunning ifTrue: [
		ctxt := thisContext.
		[	ctxt := ctxt findNextUnwindContextUpTo: nil.
			ctxt isNil
		] whileFalse: [
			(ctxt tempAt: 2) ifNil:[
				ctxt tempAt: 2 put: nil.
				unwindBlock := ctxt tempAt: 1.
				thisContext terminateTo: ctxt.
				unwindBlock value].
		].
		thisContext terminateTo: nil.
		self suspend.
	] ifFalse: [
		"Always suspend the process first so it doesn't accidentally get woken up"
		oldList := self suspend.
		suspendedContext ifNotNil:[
			"Figure out if we are terminating the process while waiting in Semaphore>>critical:
			In this case, pop the suspendedContext so that we leave the ensure: block inside
			Semaphore>>critical: without signaling the semaphore."
			(oldList class == Semaphore and:[
				suspendedContext method == (Semaphore compiledMethodAt: #critical:)]) ifTrue: [
					suspendedContext := suspendedContext home.
			].

			"If we are terminating a process halfways through an unwind, try
			to complete that unwind block first."
			(suspendedContext findNextUnwindContextUpTo: nil) ifNotNil:[:outer|
				(suspendedContext findContextSuchThat:[:c| c closure == (outer tempAt: 1)]) ifNotNil: [ :inner|
					"This is an unwind block currently under evaluation"
					suspendedContext runUntilErrorOrReturnFrom: inner.
				].
			].

			ctxt := self popTo: suspendedContext bottomContext.
			ctxt == suspendedContext bottomContext ifFalse: [
				self debug: ctxt title: 'Unwind error during termination']].
	].
! !

!Process methodsFor: 'accessing' stamp: 'jmv 5/10/2013 11:52'!
suspendedContext
	"Answer the context the receiver has suspended.
	I am Ready to Run or Suspended. Answer the context I was running when last preempted.

	Otherwise, (Running or Terminated) answer nil."

	^self isTerminated ifFalse: [ suspendedContext ]! !

!Process methodsFor: 'accessing' stamp: 'jmv 5/10/2013 12:58'!
suspendingList
	"Answer the list on which the receiver has been suspended."

	"myList can be:
		- A LinkedList: The Processor queue we are in. This, if we are Ready to Run.
		- A Semaphore. This, if we are Blocked waiting on it.
		- nil otherwise, i.e. if we are Running, Suspended, Terminated."
	^myList! !

!Process methodsFor: 'printing' stamp: 'jmv 5/10/2013 11:29'!
browserPrintString
	^self browserPrintStringFull: true! !

!Process methodsFor: 'printing' stamp: 'jmv 5/10/2013 13:06'!
longPrintOn: stream

	| ctxt |
	super printOn: stream.
	stream newLine.
	ctxt _ self suspendedContext.
	[ctxt == nil] whileFalse: [
		stream space.
		ctxt printOn: stream.
		stream newLine.
		ctxt _ ctxt sender.
	]! !

!Process methodsFor: 'debugging' stamp: 'jmv 5/10/2013 11:32'!
debug: context title: title full: bool
	"Open debugger on self with context shown on top"

	| topCtxt |
	topCtxt _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	(topCtxt hasContext: context) ifFalse: [^ self error: 'context not in process'].
	Debugger openOn: self context: context label: title fullView: bool.
! !

!Process methodsFor: 'debugging' stamp: 'jmv 5/10/2013 11:33'!
debugFullWithTitle: title
	"Open debugger on self"

	| context |
	context _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	self debug: context title: title full: true! !

!Process methodsFor: 'signaling' stamp: 'svp 9/19/2003 18:41'!
pvtSignal: anException list: aList
	"Private. This method is used to signal an exception from another
	process...the receiver must be the active process.  If the receiver 
	was previously waiting on a Semaphore, then return the process
	to the waiting state after signaling the exception and if the Semaphore
	has not been signaled in the interim"

	"Since this method is not called in a normal way, we need to take care
	that it doesn't directly return to the caller (because I believe that could
	have the potential to push an unwanted object on the caller's stack)."

	| blocker |
	self isActiveProcess ifFalse: [^self].
	anException signal.
	blocker := Semaphore new.
	[self suspend.
	suspendedContext := suspendedContext swapSender: nil.
	aList class == Semaphore 
		ifTrue:
			[aList isSignaled
				ifTrue: 
					[aList wait.  "Consume the signal that would have restarted the receiver"
					self resume]
				ifFalse:
					["Add us back to the Semaphore's list (and remain blocked)"
					myList := aList.
					aList add: self]]
		ifFalse: [self resume]] fork.
	blocker wait.


! !

!Process methodsFor: 'signaling' stamp: 'jmv 5/10/2013 11:32'!
signal: anException
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If 
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"

	"If we are the active process, go ahead and signal the exception"
	self isRunning ifTrue: [^anException signal].

	"Add a new method context to the stack that will signal the exception"
	suspendedContext := MethodContext
		sender: suspendedContext
		receiver: self
		method: (self class methodDict at: #pvtSignal:list:)
		arguments: (Array with: anException with: myList).

	"If we are on a list to run, then suspend and restart the receiver 
	(this lets the receiver run if it is currently blocked on a semaphore).  If
	we are not on a list to be run (i.e. this process is suspended), then when the
	process is resumed, it will signal the exception"

	myList ifNotNil: [self suspend; resume].! !

!Process methodsFor: 'signaling' stamp: 'jmv 5/10/2013 11:32'!
signalException: anException
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If 
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"
	| oldList |
	"If we are the active process, go ahead and signal the exception"
	self isRunning ifTrue: [^anException signal].

	"Suspend myself first to ensure that I won't run away in the
	midst of the following modifications."
	myList ifNotNil:[oldList := self suspend].

	"Add a new method context to the stack that will signal the exception"
	suspendedContext := MethodContext
		sender: suspendedContext
		receiver: self
		method: (self class lookupSelector: #pvtSignal:list:)
		arguments: (Array with: anException with: oldList).

	"If we are on a list to run, then suspend and restart the receiver 
	(this lets the receiver run if it is currently blocked on a semaphore).  If
	we are not on a list to be run (i.e. this process is suspended), then when the
	process is resumed, it will signal the exception"

	oldList ifNotNil: [self resume].
! !

!Process methodsFor: 'testing' stamp: 'jmv 5/10/2013 13:05'!
isSuspended
	"Answer true if I was never scheduled yet (new process, never been sent #resume) or paused (was sent #suspend)"
	self isRunning ifTrue: [ ^false ].
	self isTerminated ifTrue: [ ^false ].
	^myList isNil! !

!Process methodsFor: 'testing' stamp: 'jmv 5/10/2013 13:00'!
isTerminated
	"Answer true if terminated, i.e. can never be resumed again, because have nothing to run."
	self isRunning ifTrue: [^ false].
	^suspendedContext isNil
		or: [ suspendedContext pc isNil
			or: ["If the suspendedContext is the bottomContext it is the block in Process>>newProcess.
		   		If so, and the pc is greater than the startpc, the bock has alrteady sent and returned
		   		from value and there is nothing more to do."
				suspendedContext isBottomContext
					and: [
						suspendedContext pc > suspendedContext startpc]]]! !


!ProcessBrowser methodsFor: 'process actions' stamp: 'jmv 5/10/2013 13:02'!
suspendProcess
	| rule |
	(selectedProcess isSuspended or: [ selectedProcess isTerminated ])
		ifTrue: [^ self].
	rule _ (self class rulesFor: selectedProcess) first.
	rule
		ifFalse: [
			PopUpMenu inform: 'Nope, won''t suspend ' , selectedProcess name.
			^ self].
	self class suspendProcess: selectedProcess.
	self updateProcessList! !

!ProcessBrowser methodsFor: 'process list' stamp: 'jmv 5/10/2013 11:29'!
processNameList
	"since processList is a WeakArray, we have to strengthen the result"
	| tally |
	tally _ CPUWatcher current ifNotNil: [ :pw | pw tally ].
	^ (processList asOrderedCollection
		copyWithout: nil)
		collect: [:each | | percent |
			percent _ tally
				ifNotNil: [ ((((tally occurrencesOf: each) * 100.0 / tally size) roundTo: 1)
						asString padded: #left to: 2 with: $ ), '% '  ]
				ifNil: [ '' ].
			percent, (each browserPrintStringFull: false)
		] ! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 5/10/2013 10:09'!
resumeProcess: aProcess
	| priority |
	priority _ self suspendedProcesses
				removeKey: aProcess
				ifAbsent: [aProcess priority].
	aProcess priority: priority.
	aProcess isReady ifTrue: [ aProcess resume ]
! !

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 5/10/2013 11:29'!
dumpPigStackOn: aStream andClose: aBoolean
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process depth stack suspendedContext |
	promise := Processor tallyCPUUsageFor: 1 every: 10.
	tally := promise value.
	"WorldState addDeferredUIMessage: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; newLine.
	self dumpTally: tally on: aStream.
	aStream newLine; nextPutAll: '====Process using most CPU===='; newLine.
	process _ tally sortedCounts first value.
	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.
	aStream
		nextPutAll: ' % ';
		nextPutAll: (process browserPrintStringFull: false);
		newLine.
	depth _ 20.
	stack _ process == Processor activeProcess
		ifTrue: [thisContext stackOfSize: depth]
		ifFalse: [suspendedContext _ process suspendedContext.
			suspendedContext
				ifNotNil: [suspendedContext stackOfSize: depth]].
	stack 
		ifNil: [ aStream nextPutAll: 'No context'; newLine]
		ifNotNil: [
			stack do: [ :c | 
				c printOn: aStream.
				aStream newLine]].
	aBoolean ifTrue: [aStream close]! !


!ProcessorScheduler methodsFor: 'CPU usage tally' stamp: 'jmv 5/10/2013 10:09'!
nextReadyProcess
	quiescentProcessLists reverseDo: [ :list |
		list isEmpty ifFalse: [ | proc |
			proc _ list first.
			proc isReady ifTrue: [ ^proc ]]].
	^nil! !

!methodRemoval: Process #browserPrintStringWith:!
Process removeSelector: #browserPrintStringWith:!
!methodRemoval: Process #isWaiting!
Process removeSelector: #isWaiting!

!Process reorganize!
('changing process state' primitiveResume resume resumeAt: run suspend terminate)
('changing suspended state' activateReturn:value: complete: completeStep: completeTo: install: popTo: popTo:value: restartTop restartTopWith: return:value: step step: stepToCallee stepToHome: stepToSendOrReturn)
('accessing' calleeOf: copyStack name name: priority priority: suspendedContext suspendingList)
('printing' browserPrintString browserPrintStringFull: longPrintOn: printOn: statusString)
('private' suspendedContext:)
('objects from disk' objectForDataStream:)
('debugging' debug:title: debug:title:full: debugFullWithTitle:)
('signaling' pvtSignal:list: signal: signalException:)
('*KernelTests-Processes' suspendPrimitivelyOrFail)
('UI support' animatedUI animatedUI:)
('testing' isActiveProcess isBlocked isReady isRunning isSuspended isTerminated isTheLastPreempted)
!


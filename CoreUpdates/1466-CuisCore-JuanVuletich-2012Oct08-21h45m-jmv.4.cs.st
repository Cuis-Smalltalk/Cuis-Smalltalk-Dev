'From Cuis 4.0 of 21 April 2012 [latest update: #1465] on 8 October 2012 at 10:49:41 pm'!

!Delay class methodsFor: 'timer process' stamp: 'jmv 10/8/2012 22:41'!
timerProcess
	^TimerEventLoop! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 10/8/2012 22:44'!
rulesFor: aProcess
	"Answer two flags: allow-stop, and allow-debug"
	^ [aProcess caseOf: {
		[] -> [{false. false}].
		[Smalltalk lowSpaceWatcherProcess] -> [{false. false}].
		[WeakArray runningFinalizationProcess] -> [{false. false}].
		[Processor activeProcess] -> [{false. true}].
		[Processor backgroundProcess] -> [{false. false}].
		[Sensor interruptWatcherProcess] -> [{false. false}].
		[Sensor eventTicklerProcess] -> [{false. false}].
		[ProjectX uiProcessX] -> [{false. false}].
		[SoundPlayer playerProcess] -> [{false. false}].
		[CPUWatcher currentWatcherProcess] -> [{false. false}].
		[Delay timerProcess] -> [{false. false}]}
		otherwise: [ {true. true}]]
		ifError: [ :err :rcvr | {true. true}]! !


!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 10/8/2012 22:20'!
valueWithin: aDuration onTimeout: timeoutBlock
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog |

	aDuration <= Duration zero ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := aDuration asDelay.

	"make a watchdog process"
	watchdog := [
		delay wait. 	"wait for timeout or completion"
		theProcess ifNotNil:[ theProcess signalException: TimedOut ] 
	] newProcess.

	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchdog priority: Processor timingPriority-1.
	watchdog name: 'BlockClosure Watchdog'.

	"catch the timeout signal"
	^ [	watchdog resume.				"start up the watchdog"
		self ensure:[						"evaluate the receiver"
			theProcess := nil.				"it has completed, so ..."
			delay delaySemaphore signal.	"arrange for the watchdog to exit"
		]] on: TimedOut do: [ :e | timeoutBlock value ].
! !

!BlockClosure methodsFor: 'private' stamp: 'jmv 10/8/2012 22:20'!
grabProcessorFor: milliseconds onTimeout: timeoutBlock
	"Evaluate the receiver (block), without the possibility of preemption by regular priority processes.
	If not finished after milliseconds, restore original priority and evaluate timeoutBlock.
	Use with care!!"
	"Based on #valueUnpreemptively"
	
	| activeProcess oldPriority result done |
	activeProcess _ Processor activeProcess.
	oldPriority _ activeProcess priority.
	done _ false.
	
	activeProcess priority: Processor highIOPriority + Processor lowIOPriority // 2.
	milliseconds ifNotNil: [
		[
			(Delay forMilliseconds: milliseconds) wait.
			done ifFalse: [
				activeProcess
					suspend;
					priority: oldPriority;
					resume.
				timeoutBlock value ]
		] forkAt: Processor highIOPriority named: '#grabProcessorFor:onTimeout:' ].

	result _ self ensure: [
		done _ true.
		activeProcess priority: oldPriority].
	
	"Yield after restoring priority to give the preempted processes a chance to run"
	Processor yield.
	^result! !


!BlockContext methodsFor: 'evaluating' stamp: 'jmv 10/8/2012 22:19'!
valueWithin: aDuration onTimeout: timeoutBlock
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog |

	aDuration <= Duration zero ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := aDuration asDelay.

	"make a watchdog process"
	watchdog := [
		delay wait. 	"wait for timeout or completion"
		theProcess ifNotNil:[ theProcess signalException: TimedOut ] 
	] newProcess.

	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchdog priority: Processor timingPriority-1.
	watchdog name: 'BlockContext Watchdog'.

	"catch the timeout signal"
	^ [	watchdog resume.				"start up the watchdog"
		self ensure:[						"evaluate the receiver"
			theProcess := nil.				"it has completed, so ..."
			delay delaySemaphore signal.	"arrange for the watchdog to exit"
		]] on: TimedOut do: [ :e | timeoutBlock value ].
! !


!CPUWatcher methodsFor: 'porcine capture' stamp: 'jmv 10/8/2012 22:45'!
catchThePig: aProcess
	| rule |
	"nickname, allow-stop, allow-debug"
	rule _ (ProcessBrowser rulesFor: aProcess) first.

	(ProcessBrowser isUIProcess: aProcess)
		ifTrue: [ "aProcess debugWithTitle: 'Interrupted from the CPUWatcher'." ]
		ifFalse: [
			rule ifFalse: [ ^self ].
			ProcessBrowser suspendProcess: aProcess.
			self openWindowForSuspendedProcess: aProcess ]! !

!CPUWatcher methodsFor: 'porcine capture' stamp: 'jmv 10/8/2012 22:45'!
openMorphicWindowForSuspendedProcess: aProcess
	| menu rule |
	menu _ MenuMorph new.
	"nickname  allow-stop  allow-debug"
	rule _ (ProcessBrowser rulesFor: aProcess) second.
	menu add: 'Dismiss this menu' target: menu selector: #delete; addLine.
	menu add: 'Open Process Browser' target: ProcessBrowserWindow selector: #openProcessBrowser.
	menu add: 'Resume'
		target: self
		selector: #resumeProcess:fromMenu:
		argumentList: { aProcess . menu }.
	menu add: 'Terminate'
		target: self
		selector: #terminateProcess:fromMenu:
		argumentList: { aProcess . menu }.
	rule ifTrue: [
		menu add: 'Debug at a lower priority'
			target: self
			selector: #debugProcess:fromMenu:
			argumentList: { aProcess . menu }.
	].
	menu addTitle: aProcess identityHash asString,
		' ', aProcess name,
		' is taking too much time and has been suspended.
What do you want to do with it?'.
	menu stayUp.
	menu popUpInWorld
! !

!CPUWatcher methodsFor: 'startup-shutdown' stamp: 'jmv 10/8/2012 21:54'!
monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean
	self stopMonitoring.

	watcher _ [ [ | promise |
		promise _ Processor tallyCPUUsageFor: secs every: msecs.
		tally _ promise value.
		promise _ nil.
		aBoolean ifTrue: [ self findThePig ].
	] repeat ] newProcess.
	watcher priority: Processor highestPriority.
	watcher name: 'CPUWatcher monitor'.
	watcher resume.
	Processor yield ! !


!ConnectionQueue methodsFor: 'private' stamp: 'jmv 10/8/2012 22:17'!
initPortNumber: anInteger queueLength: queueLength
	"Private!! Initialize the receiver to listen on the given port number. Up to queueLength connections will be queued."

	portNumber _ anInteger.
	maxQueueLength _ queueLength.
	connections _ OrderedCollection new.
	accessSema _ Semaphore forMutualExclusion.
	socket _ nil.
	process _ [self listenLoop] newProcess.
	process priority: Processor highIOPriority.
	process name: 'ConnectionQueue'.
	process resume! !


!Delay class methodsFor: 'timer process' stamp: 'jmv 10/8/2012 22:10'!
startTimerEventLoop
	"Start the timer event loop"
	"Delay startTimerEventLoop"
	self stopTimerEventLoop.
	AccessProtect := Semaphore forMutualExclusion.
	ActiveDelayStartTime := Time millisecondClockValue.
	SuspendedDelays := 
		Heap withAll: (SuspendedDelays ifNil:[#()])
			sortBlock: [:d1 :d2 | d1 resumptionTime <= d2 resumptionTime].
	TimingSemaphore := Semaphore new.
	RunTimerEventLoop := true.
	TimerEventLoop := [self runTimerEventLoop] newProcess.
	TimerEventLoop
		priority: Processor timingPriority;
		name: 'Delay Scheduling';
		resume.
	TimingSemaphore signal. "get going"
! !


!DelayTest methodsFor: 'testing-limits' stamp: 'jmv 10/8/2012 22:17'!
testMultiProcessWaitOnSameDelay
	"Ensure that waiting on the same delay from multiple processes raises an error"
	"
	self new testMultiProcessWaitOnSameDelay
	"
	| delay p1 p2 wasRun |
	delay := Delay forSeconds: 1.
	wasRun := false.
	p1 := [delay wait] newProcess.
	p1 priority: Processor activePriority+1.
	p1 name: 'testMultiProcessWaitOnSameDelay-1'.
	p1 resume.
	p2 := [
		self should:[delay wait] raise: Error.
		wasRun := true.
	] newProcess.
	p2 priority:  Processor activePriority+1.
	p2 name: 'testMultiProcessWaitOnSameDelay-2'.
	p2 resume.
	p1 terminate.
	p2 terminate.
	self assert: wasRun.

! !


!InputSensor methodsFor: 'user interrupts' stamp: 'jmv 10/8/2012 21:59'!
installInterruptWatcher
	"Initialize the interrupt watcher process. Terminate the old process if any."
	"Sensor installInterruptWatcher"

	InterruptWatcherProcess ifNotNil: [InterruptWatcherProcess terminate].
	InterruptSemaphore _ Semaphore new.
	InterruptWatcherProcess _ [self userInterruptWatcher] newProcess.
	InterruptWatcherProcess priority: Processor lowIOPriority.
	InterruptWatcherProcess name: 'User interrupt watcher'.
	InterruptWatcherProcess resume.
	self primInterruptSemaphore: InterruptSemaphore! !


!EventSensor methodsFor: 'private' stamp: 'jmv 10/8/2012 21:58'!
installEventTickler
	"Initialize the interrupt watcher process. Terminate the old process if any."
	"Sensor installEventTickler"

	EventTicklerProcess ifNotNil: [ EventTicklerProcess terminate ].
	EventTicklerProcess _ [ self eventTickler ] newProcess.
	EventTicklerProcess priority: Processor lowIOPriority.
	EventTicklerProcess name: 'Event Tickler'.
	EventTicklerProcess resume! !


!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jmv 10/8/2012 22:16'!
startMIDITracking

	midiParser ifNil: [^ self].
	midiParser midiPort ifNil: [^ self].
	midiParser midiPort ensureOpen.
	self stopMIDITracking.
	SoundPlayer useShortBuffer.
	process _ [self midiTrackingLoop] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'MIDISynth'.
	process resume! !


!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 10/8/2012 22:16'!
spyAllEvery: millisecs on: aBlock
	"Create a spy and spy on the given block at the specified rate."
	"Spy all the system processes"

	| myDelay time0 |
	aBlock isBlock
		ifFalse: [ self error: 'spy needs a block here' ].
	self class: aBlock receiver class method: aBlock method.
		"set up the probe"
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time millisecondClockValue.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[true] whileTrue: [
			| startTime observedProcess |
			startTime := Time millisecondClockValue.
			myDelay wait.
			observedProcess := Processor preemptedProcess.
			self
				tally: observedProcess suspendedContext
				in: observedProcess
				"tally can be > 1 if ran a long primitive"
				by: (Time millisecondClockValue - startTime) // millisecs].
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
			gcVal ifNotNil: [
				gcStats at: idx put: (gcVal - (gcStats at: idx))]].
		time := Time millisecondClockValue - time0]! !

!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 10/8/2012 22:16'!
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
	time0 := Time millisecondClockValue.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[ true ] whileTrue: [
			| startTime |
			startTime := Time millisecondClockValue.
			myDelay wait.
			self
				tally: Processor preemptedProcess suspendedContext
				in: (observedProcess == Processor preemptedProcess ifTrue: [observedProcess] ifFalse: [nil])
				"tally can be > 1 if ran a long primitive"
				by: (Time millisecondClockValue - startTime) // millisecs].
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
			gcVal ifNotNil: [
				gcStats at: idx put: (gcVal - (gcStats at: idx))]].
		time := Time millisecondClockValue - time0]! !

!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 10/8/2012 22:15'!
spyEvery: millisecs onProcess: aProcess forMilliseconds: msecDuration
	"Create a spy and spy on the given process at the specified rate."
	| myDelay time0 endTime observedProcess sem |
	(aProcess isKindOf: Process)
		ifFalse: [self error: 'spy needs a Process here'].
	self class: aProcess suspendedContext receiver class method: aProcess suspendedContext method.
	"set up the probe"
	observedProcess _ aProcess.
	myDelay _ Delay forMilliseconds: millisecs.
	time0 _ Time millisecondClockValue.
	endTime _ time0 + msecDuration.
	sem _ Semaphore new.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
			[
				| startTime |
				startTime _ Time millisecondClockValue.
				myDelay wait.
				self
					tally: Processor preemptedProcess suspendedContext
					in: (observedProcess == Processor preemptedProcess ifTrue: [ observedProcess ] ifFalse: [nil])
					"tally can be > 1 if ran a long primitive"
					by: (Time millisecondClockValue - startTime) // millisecs.
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
		gcVal ifNotNil: [
			gcStats at: idx put: (gcVal - (gcStats at: idx))]].
	time _ Time millisecondClockValue - time0! !


!Process methodsFor: 'accessing' stamp: 'jmv 10/8/2012 21:52'!
name

	^name ifNil: [ 'unnamed' ]! !

!Process methodsFor: 'printing' stamp: 'jmv 10/8/2012 22:30'!
browserPrintStringWith: anObject

	^String streamContents: [ :stream |
		stream nextPut: $(.
		priority printOn: stream.
		self isSuspended
			ifTrue: [ stream nextPut: $s ].
		stream nextPutAll: ') '.
		self hash printOn: stream.
		stream space.
		stream nextPutAll: self name.
		anObject ifNotNil: [
			stream
				nextPutAll: ': ';
				nextPutAll: anObject asString ]]! !


!ProcessBrowser methodsFor: 'process actions' stamp: 'jmv 10/8/2012 22:45'!
suspendProcess
	| rule |
	selectedProcess isSuspended
		ifTrue: [^ self].
	rule _ (self class rulesFor: selectedProcess) first.
	rule
		ifFalse: [
			PopUpMenu inform: 'Nope, won''t suspend ' , selectedProcess name.
			^ self].
	self class suspendProcess: selectedProcess.
	self updateProcessList! !

!ProcessBrowser methodsFor: 'process actions' stamp: 'jmv 10/8/2012 22:45'!
terminateProcess
	| rule |
	rule _ (self class rulesFor: selectedProcess) first.
	rule
		ifFalse: [PopUpMenu inform: 'Nope, won''t kill ' , selectedProcess name.
			^ self].
	self class terminateProcess: selectedProcess.	
	self updateProcessList! !

!ProcessBrowser methodsFor: 'process list' stamp: 'jmv 10/8/2012 22:32'!
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
			percent, (each browserPrintStringWith: nil)
		] ! !


!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 10/8/2012 22:46'!
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
		nextPutAll: (process browserPrintStringWith: nil);
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

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 10/8/2012 22:47'!
dumpTally: tally on: aStream
	"tally is from ProcessorScheduler>>tallyCPUUsageFor:
	Dumps lines with percentage of time, hash of process, and a friendly name"

	tally sortedCounts do: [ :assoc |
		(((assoc key / tally size) * 100.0) roundTo: 1) printOn: aStream.
		aStream
			nextPutAll: '%   ';
			print: assoc value identityHash; space;
			nextPutAll: assoc value name;
			newLine.
	]! !

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 10/8/2012 22:22'!
tallyCPUUsageFor: seconds every: msec 
	"Compute CPU usage using a msec millisecond sample for the given number of seconds,
	then dump the usage statistics on the Transcript. The UI is free to continue, meanwhile.
	This method has no senders, but is a useful utility"

	"ProcessBrowser tallyCPUUsageFor: 10 every: 100"

	| promise |
	promise := Processor tallyCPUUsageFor: seconds every: msec.
	
	[| tally |
	tally := promise value.
	WorldState addDeferredUIMessage: [self dumpTallyOnTranscript: tally]] 
			fork! !


!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 10/8/2012 22:47'!
processListMenu
	| menu |
	menu _ MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			| rules | 
			rules _ model class rulesFor: model selectedProcess.
			menu addList: #(
				('inspect (i)'					#inspectProcess)
				('explore (I)'				#exploreProcess)
				('inspect Pointers (P)'		#inspectPointers)).
			rules first
				ifTrue: [
					menu add: 'terminate (t)' target: model action: #terminateProcess.
					selectedProcess isSuspended
						ifTrue: [menu add: 'resume (r)' target: model action: #resumeProcess]
						ifFalse: [menu add: 'suspend (s)' target: model action: #suspendProcess]].
			rules second
				ifTrue: [
					menu addList: #(
						('change priority (p)'		 	#changePriority)
						('debug (d)'						#debugProcess))].
			menu addList: #(('profile messages (m)'	#messageTally)).
			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [menu add: 'signal Semaphore (S)' target: model action: #signalSemaphore].
			menu add: 'full stack (k)' target: model action: #moreStack.
			menu addLine].

	menu addList: #(
		('find context... (f)'		#findContext)
		('find again (g)'			#nextContext		''		model)).
	menu addLine.

	menu
		add: (isStepping
				ifTrue: ['turn off auto-update (a)']
				ifFalse: ['turn on auto-update (a)'])
		action: #toggleAutoUpdate.
	menu add: 'update list (u)' target: model action: #updateProcessList.

	menu addLine.
	CPUWatcher isMonitoring
			ifTrue: [ menu add: 'stop CPUWatcher' action: #stopCPUWatcher ]
			ifFalse: [ menu add: 'start CPUWatcher' action: #startCPUWatcher  ].

	^ menu! !

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 10/8/2012 22:47'!
changePriority
	| str newPriority rule |
	rule _ (model class rulesFor: model selectedProcess) second.
	rule
		ifFalse: [PopUpMenu inform: 'Nope, won''t change priority of ' , model selectedProcess name.
			^ self].
	str _ FillInTheBlankMorph request: 'New priority' initialAnswer: model selectedProcess priority asString.
	newPriority _ str asNumber asInteger.
	newPriority
		ifNil: [^ self].
	(newPriority < 1
			or: [newPriority > Processor highestPriority])
		ifTrue: [PopUpMenu inform: 'Bad priority'.
			^ self].
	model class setProcess: model selectedProcess toPriority: newPriority.
	model updateProcessList! !

!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 10/8/2012 22:47'!
debugProcess
	| rule |
	rule _ (model class rulesFor: model selectedProcess) second.
	rule third
		ifFalse: [PopUpMenu inform: 'Nope, won''t debug ' , model selectedProcess name.
			^ self].
	model class debugProcess: model selectedProcess.! !


!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:15'!
testSchedulerTermination
	| process sema gotHere sema2 |
	gotHere _ false.
	sema _ Semaphore new.
	sema2 _ Semaphore new.
	process _ [
		sema signal.
		sema2 wait.
		"will be suspended here"
		gotHere _ true ] newProcess.
	process priority: Processor activeProcess priority.
	process name: 'testSchedulerTermination'.
	process resume.
	sema wait.
	"until process gets scheduled"
	process terminate.
	sema2 signal.
	Processor yield.
	"will give process a chance to continue and horribly screw up"
	self assert: gotHere not.! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:15'!
testUnwindFromActiveProcess
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert:(sema isSignaled).
	process := [
		sema critical:[
			self deny: sema isSignaled.
			Processor activeProcess terminate.
		]
	] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'testUnwindFromActiveProcess'.
	process resume.
	self assert: sema isSignaled! !

!ProcessTerminateBug methodsFor: 'tests' stamp: 'jmv 10/8/2012 22:14'!
testUnwindFromForeignProcess
	| sema process |
	sema := Semaphore forMutualExclusion.
	self assert: sema isSignaled.
	process := [
		sema critical:[
			self deny: sema isSignaled.
			sema wait. "deadlock"
		]
	] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'testUnwindFromForeignProcess'.
	process resume.
	self deny: sema isSignaled.
	"This is for illustration only - the BlockCannotReturn cannot 
	be handled here (it's truncated already)"
	self shouldnt: [process terminate] raise: BlockCannotReturn.
	self assert: sema isSignaled! !


!ProcessorScheduler methodsFor: 'CPU usage tally' stamp: 'jmv 10/8/2012 22:23'!
tallyCPUUsageFor: seconds every: msec
	"Start a high-priority process that will tally the next ready process for the given
	number of seconds. Answer a Block that will return the tally (a Bag) after the task
	is complete" 
	| tally sem delay endDelay |
	tally _ IdentityBag new: 200.
	delay _ Delay forMilliseconds: msec truncated.
	endDelay _ Delay forSeconds: seconds.
	endDelay schedule.
	sem _ Semaphore new.
	[
		[ endDelay isExpired ] whileFalse: [
			delay wait.
			tally add: Processor nextReadyProcess
		].
		sem signal.
	] forkAt: self highestPriority named: 'Processor CPU Usage Tallier'.

	^[ sem wait. tally ]! !


!ProcessorScheduler class methodsFor: 'background process' stamp: 'jmv 10/8/2012 22:00'!
startUp
	"Install a background process of the lowest possible priority that is always runnable."
	"Details: The virtual machine requires that there is aways some runnable process that can be scheduled; this background process ensures that this is the case."

	Smalltalk installLowSpaceWatcher.
	BackgroundProcess ifNotNil: [BackgroundProcess terminate].
	BackgroundProcess _ [self idleProcess] newProcess.
	BackgroundProcess priority: SystemRockBottomPriority.
	BackgroundProcess name: 'Idle Process'.
	BackgroundProcess resume.
! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 10/8/2012 21:57'!
spawnNewProcessX

	UIProcess _ [
		World clearWaitDelay.
		[World doOneCycle.  Processor yield.  false] whileFalse: [].
	] newProcess priority: Processor userSchedulingPriority.
	UIProcess name: 'Morphic UI'.
	UIProcess resume! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 10/8/2012 22:06'!
styleInBackgroundProcess
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, via #stylerStyled:checkForChanges:
	The the model should grab the TextAttributes we added to the copy, as appropriate."
	| afterFormatProcess |
	self terminateBackgroundStylingProcess.

	formattedText _ textModel actualContents copy.
	self mutex critical: [
		
		"This part, running at current priority, and fired by sem, is for events to be triggered at current priority"
		sem _ Semaphore new. 
		afterFormatProcess _ [
			sem ifNotNil: [
				sem wait.
				textModel stylerStyled: formattedText checkForChanges: true ]
		] newProcess.
		afterFormatProcess
			priority: Processor activePriority;
			name: 'Shout after style update';
			resume.

		"This part runs at low priority, and signals sem when finished"
		backgroundProcess _  [
			textModel privateStyleWith: self.
			sem signal] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]! !


!ScorePlayer methodsFor: 'midi output' stamp: 'jmv 10/8/2012 22:14'!
startMIDIPlaying
	"Start up a process to play this score via MIDI."

	midiPort ensureOpen.
	midiPlayerProcess ifNotNil: [midiPlayerProcess terminate].
	midiPlayerProcess _ [self midiPlayLoop] newProcess.
	midiPlayerProcess
		priority: Processor userInterruptPriority;
		name: 'ScorePlayer';
		resume.
! !


!SoundPlayer class methodsFor: 'player process' stamp: 'jmv 10/8/2012 22:14'!
startPlayerProcessBufferSize: bufferSize rate: samplesPerSecond stereo: stereoFlag sound: aSound
	"Start the sound player process. Terminate the old process, if any."
	"SoundPlayer startPlayerProcessBufferSize: 1000 rate: 11025 stereo: false"

	self stopPlayerProcess.
	aSound
		ifNil:[ActiveSounds _ OrderedCollection new]
		ifNotNil:[ActiveSounds _ OrderedCollection with: aSound].
	Buffer _ SoundBuffer newStereoSampleCount: (bufferSize // 4) * 4.
	LastBuffer ifNotNil:[LastBuffer _ SoundBuffer basicNew: Buffer basicSize].
	PlayerSemaphore _ Semaphore forMutualExclusion.
	SamplingRate _ samplesPerSecond.
	Stereo _ stereoFlag.
	ReadyForBuffer _ Semaphore new.
	SoundSupported _ true. "Assume so"
	UseReadySemaphore _ true.  "set to false if ready semaphore not supported by VM"
	self primSoundStartBufferSize: Buffer stereoSampleCount
		rate: samplesPerSecond
		stereo: Stereo
		semaIndex: (Smalltalk registerExternalObject: ReadyForBuffer).
	"Check if sound start prim was successful"
	SoundSupported ifFalse:[^self].
	UseReadySemaphore
		ifTrue: [PlayerProcess _ [SoundPlayer playLoop] newProcess]
		ifFalse: [PlayerProcess _ [SoundPlayer oldStylePlayLoop] newProcess].
	UseReverb ifTrue: [self startReverb].

	PlayerProcess
		priority: Processor userInterruptPriority;
		name: 'Sound Player';
		resume! !


!SoundRecorder methodsFor: 'recording controls' stamp: 'jmv 10/8/2012 22:13'!
startRecording
	"Turn of the sound input driver and start the recording process. Initially, recording is paused."

	| semaIndex |
	recordLevel ifNil: [recordLevel _ 0.5].  "lazy initialization"
	Preferences canRecordWhilePlaying ifFalse: [SoundPlayer shutDown].
	recordProcess ifNotNil: [self stopRecording].
	paused _ true.
	meteringBuffer _ SoundBuffer newMonoSampleCount: 1024.
	meterLevel _ 0.
	self allocateBuffer.
	bufferAvailableSema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: bufferAvailableSema.
	self primStartRecordingDesiredSampleRate: samplingRate asInteger
		stereo: stereo
		semaIndex: semaIndex.
	RecorderActive _ true.
	samplingRate _ self primGetActualRecordingSampleRate.
	self primSetRecordLevel: (1000.0 * recordLevel) asInteger.
	recordProcess _ [self recordLoop] newProcess.
	recordProcess
		priority: Processor userInterruptPriority;
		name: 'Sound Recorder';
		resume.
! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 10/8/2012 22:03'!
installLowSpaceWatcher
	"Start a process to watch for low-space conditions."
	"Smalltalk installLowSpaceWatcher"

	self primSignalAtBytesLeft: 0.  "disable low-space interrupts"
	LowSpaceProcess ifNotNil: [LowSpaceProcess terminate].
	LowSpaceProcess _ [self lowSpaceWatcher] newProcess.
	LowSpaceProcess priority: Processor lowIOPriority.
	LowSpaceProcess name: 'Low Space Watcher'.
	LowSpaceProcess resume.

! !


!Transcripter class methodsFor: 'instance creation' stamp: 'jmv 10/8/2012 22:12'!
startTranscriptProcess   "Transcripter startTranscriptProcess"
	| activeProcess |
	Transcript _ self newInFrame: Display boundingBox.
	activeProcess _ [Transcript readEvalPrint.
					Smalltalk processShutDownList: true; quitPrimitive]
						newProcess.
	activeProcess
		priority: Processor userSchedulingPriority;
		name: 'Transcripter';
		resume.
	Processor terminateActive
! !


!WeakArray class methodsFor: 'private' stamp: 'jmv 10/8/2012 22:02'!
restartFinalizationProcess
	"kill any old process, just in case"
	FinalizationProcess
		ifNotNil: [ 
			FinalizationProcess terminate.
			FinalizationProcess := nil ].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
	FinalizationLock := Semaphore forMutualExclusion.
	FinalizationProcess := [ self finalizationProcess ] newProcess.
	FinalizationProcess priority: Processor userInterruptPriority.
	FinalizationProcess name: 'WeakArray finalization'.
	FinalizationProcess resume! !

!methodRemoval: ProcessBrowser class #nameAndRulesFor:!
ProcessBrowser class removeSelector: #nameAndRulesFor:!
!methodRemoval: ProcessBrowser #nameAndRulesFor:!
ProcessBrowser removeSelector: #nameAndRulesFor:!
!methodRemoval: ProcessBrowser #nameAndRulesForSelectedProcess!
ProcessBrowser removeSelector: #nameAndRulesForSelectedProcess!
!methodRemoval: ProcessBrowser #prettyNameForProcess:!
ProcessBrowser removeSelector: #prettyNameForProcess:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Sensor installEventTickler.
Sensor installInterruptWatcher.
Delay startTimerEventLoop!


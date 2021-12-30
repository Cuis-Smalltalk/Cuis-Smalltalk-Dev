'From Cuis 5.0 [latest update: #5011] on 29 December 2021 at 11:25:40 am'!

!CPUWatcher methodsFor: 'startup-shutdown' stamp: 'jmv 12/29/2021 10:07:04'!
monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean
	| delay |
	self stopMonitoring.
	watcher _ [
		delay _ Delay forMilliseconds: msecs truncated.
		[
			| thisTally |
			thisTally _ IdentityBag new: 20.
			secs * 1000 // msecs timesRepeat: [
				"Could happen if process is halted and resumed."
				delay beingWaitedOn ifTrue: [ delay _ Delay forMilliseconds: delay delayDuration ].
				delay wait.
				thisTally add: Processor nextReadyProcess ].
			tally _ thisTally.
			aBoolean ifTrue: [ self findThePig ]
		] repeat
	] newProcess.
	watcher
		priority: Processor highestPriority;
		name: 'CPUWatcher monitor';
		resume.
	Processor yield.! !


!Delay class methodsFor: 'timer process' stamp: 'jmv 12/29/2021 11:23:55'!
startTimerEventLoop
	"Start the timer event loop"
	"Delay startTimerEventLoop"
	self stopTimerEventLoop.
	ActiveDelayStartTime := Time millisecondClockValue.
	SuspendedDelays := 
		Heap withAll: (SuspendedDelays ifNil:[#()])
			sortBlock: [:d1 :d2 | d1 resumptionTime <= d2 resumptionTime].
	TimingSemaphore := Semaphore new.
	ScheduledDelayNilledSemaphore := Semaphore new.
	FinishedDelayNilledSemaphore := Semaphore new.
	RunTimerEventLoop := true.
	TimerEventLoop := [self runTimerEventLoop] newProcess.
	TimerEventLoop
		priority: Processor timingPriority;
		name: '[system] Delay Scheduling';
		resume.
	TimingSemaphore signal. "get going"
! !


!InputSensor methodsFor: 'user interrupts' stamp: 'jmv 12/29/2021 11:24:44'!
installInterruptWatcher
	"Initialize the interrupt watcher process. Terminate the old process if any.
	This process simply waits for the VM to signal the 'user interrupt' semaphore, and opens a debugger."
	"
	Sensor installInterruptWatcher
	"

	InterruptWatcherProcess ifNotNil: [InterruptWatcherProcess terminate].
	InterruptSemaphore _ (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	InterruptWatcherProcess _ [self userInterruptWatcher] newProcess.
	InterruptWatcherProcess priority: Processor lowIOPriority.
	InterruptWatcherProcess name: '[system] User interrupt watcher'.
	InterruptWatcherProcess resume! !


!EventSensor methodsFor: 'private' stamp: 'jmv 12/29/2021 11:24:49'!
installEventTickler
	"Initialize the event tickler process. Terminate the old process if any."
	"
	Sensor installEventTickler
	"

	EventTicklerProcess ifNotNil: [ EventTicklerProcess terminate ].
	EventTicklerProcess _ [ self eventTickler ] newProcess.
	EventTicklerProcess priority: Processor lowIOPriority.
	EventTicklerProcess name: '[system] Event Tickler'.
	EventTicklerProcess resume! !


!WeakArray class methodsFor: 'private' stamp: 'jmv 12/29/2021 11:15:37'!
restartFinalizationProcess
	"kill any old process, just in case"
	self stopFinalizationProcess.

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
	FinalizationLock := Semaphore forMutualExclusion.
	FinalizationProcess := [ self finalizationProcess ] newProcess.
	FinalizationProcess priority: Processor userInterruptPriority.
	FinalizationProcess name: '[system] WeakArray finalization'.
	FinalizationProcess resume! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 4/13/2015 11:29'!
installLowSpaceWatcher
	"Start a process to watch for low-space conditions."
	"Smalltalk installLowSpaceWatcher"

	self stopLowSpaceWatcher.
	LowSpaceProcess _ [self lowSpaceWatcher] newProcess.
	LowSpaceProcess priority: Processor lowIOPriority.
	LowSpaceProcess name: '[system] Low Space Watcher'.
	LowSpaceProcess resume.

! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/29/2021 11:22:37'!
handleUserInterrupt
	| p |
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	p _ Sensor shiftPressed | Preferences cmdDotInterruptTakesStatistics
		ifTrue: [Utilities reportCPUandRAM]
		ifFalse: [Utilities processTakingMostCPU].
	Preferences cmdDotEnabled ifTrue: [
		"The background process can't be interrupted, or Cuis will likely crash."
		p == Processor backgroundProcess ifTrue: [
			'Can not interrupt backgroundProcess' print.
			^self ].
		(p name beginsWith: '[system]') ifTrue: [
			 ('Process {', p printString, '} is critical for system stability. Can not interrupt it.') print.
			^self ].
		[
			EventSensor install.
			p isTerminated
				ifTrue: [ ('Process {', p printString, '} isTerminated. Can not interrupt it.') print ]
				ifFalse: [ UISupervisor userInterrupt: p ]] fork
		]! !


!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 12/29/2021 11:16:13'!
installScreenUpdater
	"Initialize the event tickler process. Terminate the old process if any."
	"
	DisplayScreen installScreenUpdater
	"

	self terminateScreenUpdater.
	ScreenUpdaterProcess _ [ self screenUpdater ] newProcess.
	ScreenUpdaterProcess priority: Processor lowIOPriority.
	ScreenUpdaterProcess name: '[system] Background Screen updater'.
	ScreenUpdaterProcess resume! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
EventSensor install.
Delay startTimerEventLoop.
DisplayScreen installScreenUpdater.
WeakArray restartFinalizationProcess.
Smalltalk installLowSpaceWatcher.!


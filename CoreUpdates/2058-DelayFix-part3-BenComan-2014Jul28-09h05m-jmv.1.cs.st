'From Cuis 4.2 of 25 July 2013 [latest update: #2057] on 28 July 2014 at 9:12:57.945526 am'!
!classDefinition: #Delay category: #'Kernel-Processes'!
Object subclass: #Delay
	instanceVariableNames: 'delayDuration resumptionTime delaySemaphore beingWaitedOn '
	classVariableNames: 'AccessProtect ActiveDelay ActiveDelayStartTime DelaySuspended FinishedDelay FinishedDelayNilledSemaphore RunTimerEventLoop ScheduledDelay ScheduledDelayNilledSemaphore SuspendedDelays TimerEventLoop TimingSemaphore '
	poolDictionaries: ''
	category: 'Kernel-Processes'!

!Delay class methodsFor: 'snapshotting' stamp: 'jmv 7/28/2014 09:10'!
shutDown
	"Suspend the active delay, if any, before snapshotting. It will be reactived when the snapshot is resumed."
	"Details: This prevents a timer interrupt from waking up the active delay in the midst snapshoting, since the active delay will be restarted when resuming the snapshot and we don't want to process the delay twice."

	self stopTimerEventLoop.
	self saveResumptionTimes! !

!Delay class methodsFor: 'snapshotting' stamp: 'jmv 7/28/2014 09:10'!
startUp
	"Restart active delay, if any, when resuming a snapshot."

	self restoreResumptionTimes.
	self startTimerEventLoop.! !

!Delay class methodsFor: 'timer process' stamp: 'jmv 7/28/2014 09:10'!
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
		name: 'Delay Scheduling';
		resume.
	TimingSemaphore signal. "get going"
! !

!methodRemoval: Delay class #nextWakeUpTime!
Delay class removeSelector: #nextWakeUpTime!
!classDefinition: #Delay category: #'Kernel-Processes'!
Object subclass: #Delay
	instanceVariableNames: 'delayDuration resumptionTime delaySemaphore beingWaitedOn'
	classVariableNames: 'ActiveDelay ActiveDelayStartTime FinishedDelay FinishedDelayNilledSemaphore RunTimerEventLoop ScheduledDelay ScheduledDelayNilledSemaphore SuspendedDelays TimerEventLoop TimingSemaphore'
	poolDictionaries: ''
	category: 'Kernel-Processes'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Delay startTimerEventLoop!


'From Cuis 4.2 of 25 July 2013 [latest update: #2055] on 27 July 2014 at 11:42:10.366801 pm'!
!classDefinition: #Delay category: #'Kernel-Processes'!
Object subclass: #Delay
	instanceVariableNames: 'delayDuration resumptionTime delaySemaphore beingWaitedOn'
	classVariableNames: 'AccessProtect ActiveDelay ActiveDelayStartTime DelaySuspended FinishedDelay FinishedDelayNilledSemaphore RunTimerEventLoop ScheduledDelay ScheduledDelayNilledSemaphore SuspendedDelays TimerEventLoop TimingSemaphore'
	poolDictionaries: ''
	category: 'Kernel-Processes'!

!Delay class methodsFor: 'timer process' stamp: 'jmv 7/27/2014 23:41'!
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
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Delay startTimerEventLoop!


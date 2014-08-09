'From Cuis 4.2 of 25 July 2013 [latest update: #2056] on 28 July 2014 at 10:19:28.016062 pm'!

!Delay methodsFor: 'private' stamp: 'BenComan 7/28/2014 15:48'!
schedule
	"Schedule this delay."

	beingWaitedOn ifTrue: [ ^ self error: 'This Delay has already been scheduled.' ].
	"Assuming comparison, #ifTrue:ifFalse and assignment all inlined bytecodes that can't be interrupted."
	ScheduledDelay == nil
		ifTrue: [
			"No other process will overwrite "
			ScheduledDelay _ self ]
		ifFalse: [
			"Assuming when semaphore is signalled to continue this process, it cannot be interrupted again before assignment"
			ScheduledDelayNilledSemaphore wait.
			ScheduledDelay _ self ].
	"Signal semaphore in #handleTimerEvent (highest priority process) to action ScheduledDelay,
        set ScheduledDelay to nil and signal ScheduledDelayNilledSemaphore"
	TimingSemaphore signal
! !

!Delay methodsFor: 'private' stamp: 'BenComan 7/28/2014 15:48'!
unschedule

	"Assuming comparison, #ifTrue:ifFalse and assignment all inlined bytecodes that can't be interrupted."
	FinishedDelay == nil
		ifTrue: [ FinishedDelay _ self ]
		ifFalse: [
			"Assuming when semaphore is signalled to continue this process, it cannot be interrupted again before assignment"
			FinishedDelayNilledSemaphore wait.
			FinishedDelay _ self ].
	"Signal semaphore in #handleTimerEvent (highest priority process) to action FinishedDelay,
        set FinishedDelay to nil and signal FinishedDelayNilledSemaphore"
	TimingSemaphore signal
! !


!Delay class methodsFor: 'timer process' stamp: 'BenComan 7/28/2014 15:48'!
handleTimerEvent
	"Handle a timer event; which can be either:
		- a schedule request (ScheduledDelay notNil)
		- an unschedule request (FinishedDelay notNil)
		- a timer signal (not explicitly specified)
	We check for timer expiry every time we get a signal."
	| nowTick nextTick |
	"Wait until there is work to do."
	TimingSemaphore wait.

	"Process any schedule requests"
	ScheduledDelay ifNotNil: [
		"Schedule the given delay"
		self scheduleDelay: ScheduledDelay.
		ScheduledDelay := nil.
		ScheduledDelayNilledSemaphore initSignals. 
		ScheduledDelayNilledSemaphore signal ].

	"Process any unschedule requests"
	FinishedDelay ifNotNil: [
		self unscheduleDelay: FinishedDelay.
		FinishedDelay := nil.
		FinishedDelayNilledSemaphore initSignals.
		FinishedDelayNilledSemaphore signal ].

	"Check for clock wrap-around."
	nowTick := Time millisecondClockValue.
	nowTick < ActiveDelayStartTime ifTrue: [
		"clock wrapped"
		self saveResumptionTimes.
		self restoreResumptionTimes ].
	ActiveDelayStartTime := nowTick.

	"Signal any expired delays"
	[ ActiveDelay notNil and:[ nowTick >= ActiveDelay resumptionTime ]] whileTrue: [
		ActiveDelay signalWaitingProcess.
		SuspendedDelays isEmpty 
			ifTrue: [ ActiveDelay := nil ] 
			ifFalse:[ ActiveDelay := SuspendedDelays removeFirst ]].

	"And signal when the next request is due. We sleep at most 1sec here
	as a soft busy-loop so that we don't accidentally miss signals."
	nextTick := nowTick + 1000.
	ActiveDelay ifNotNil: [
		nextTick := nextTick min: ActiveDelay resumptionTime ].
	nextTick := nextTick min: SmallInteger maxVal.

	"Since we have processed all outstanding requests, reset the timing semaphore so
	that only new work will wake us up again. Do this RIGHT BEFORE setting the next
	wakeup call from the VM because it is only signaled once so we mustn't miss it."
	TimingSemaphore initSignals.
	Delay primSignal: TimingSemaphore atMilliseconds: nextTick.

	"This last test is necessary for the obscure case that the msecs clock rolls over
	after nowTick has been computed (unlikely but not impossible). In this case we'd
	wait for MillisecondClockMask msecs (roughly six days) or until another delay gets
	scheduled (which may not be any time soon). In any case, since handling the
	condition is easy, let's just deal with it"
	Time millisecondClockValue < nowTick
		ifTrue:[ TimingSemaphore signal ]. "retry"
! !


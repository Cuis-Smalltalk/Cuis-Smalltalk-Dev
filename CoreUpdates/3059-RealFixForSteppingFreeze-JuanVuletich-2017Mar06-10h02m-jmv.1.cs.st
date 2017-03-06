'From Cuis 5.0 of 7 November 2016 [latest update: #3058] on 6 March 2017 at 10:14:29 am'!

!Delay class methodsFor: 'timer process' stamp: 'jmv 3/6/2017 10:13:34'!
handleTimerEvent
	"Handle a timer event; which can be either:
		- a schedule request (ScheduledDelay notNil)
		- an unschedule request (FinishedDelay notNil)
		- a timer signal (not explicitly specified)
	We check for timer expiry every time we get a signal."
	| nowTick nextTick afterwardsTick |
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

	"This test is necessary for the obscure case that the msecs clock rolls over
	after nowTick has been computed (unlikely but not impossible). In this case we'd
	wait for MillisecondClockMask msecs (roughly six days) or until another delay gets
	scheduled (which may not be any time soon). In any case, since handling the
	condition is easy, let's just deal with it."
	afterwardsTick _ Time millisecondClockValue.
	afterwardsTick < nowTick  						"Means clock rollover."
		ifTrue: [ TimingSemaphore signal ]. "retry"

	"It seems that #primSignal:atMilliseconds: might not signal the semaphore if the requested moment is in the past.
	This behavior was observed in Linux Spur64 VMs.
	If this happens, next delay never finishes. Usual symptom is frozen Morphic.
	If there is any risk of this happening, just do another iteration right now."
	nextTick < afterwardsTick  						"We're already late for next signal."
		ifTrue: [ TimingSemaphore signal ]. "retry"! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/5/2017 00:38:27'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	lastCycleHadAnyEvent
		ifTrue: [
			pause _ 20.				"This value will only be used when there are no more events to serve."
			wait _ 0 ]					"Don't wait"
		ifFalse: [
			pause < 200				"No events processed? Start saving CPU!! But never make the user wait more than 200ms for being responsive again."
				ifTrue: [ pause _ pause * 21//20 ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].

			wait _ waitUntil - Time localMillisecondClock ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait > 0
		ifFalse: [ Processor yield ]
		ifTrue: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow! !


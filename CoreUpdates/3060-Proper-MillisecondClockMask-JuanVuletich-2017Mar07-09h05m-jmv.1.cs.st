'From Cuis 5.0 of 7 November 2016 [latest update: #3059] on 7 March 2017 at 9:52:45 am'!

!Time class methodsFor: 'primitives' stamp: 'jmv 3/7/2017 09:52:37'!
primMillisecondClock
	"Primitive. Answer the number of milliseconds since the millisecond clock
	was last reset or rolled over. No sync to any system clock.
	Implemented by all major platforms.
	Essential. See Object documentation whatIsAPrimitive. 
	
	Time primMillisecondClock
	Time primMillisecondClock / 1000 / 60.0

	Range is  from zero to 16r1FFFFFFF.
	The VM defines MillisecondClockMask as 16r1FFFFFFF

	Overflows usually every six days.
	Still used in #localMillisecondClock if the VM doesn't implement
		Time primLocalMicrosecondClock
	"
"Not really a clock, but a timer or ticker"

	<primitive: 135>
	self primitiveFailed! !


!Delay class methodsFor: 'timer process' stamp: 'jmv 3/7/2017 09:51:10'!
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
	nextTick := nextTick min: 16r1FFFFFFF.	"MillisecondClockMask"

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

